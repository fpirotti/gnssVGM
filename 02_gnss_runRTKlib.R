library(this.path) # For non-overlapping labels
library(pbmcapply)
library(parallel)
library(tools)
library(curl)
num_cores <- detectCores()/2 - 1
oldwd <- getwd()
setwd(this.path::this.dir())
authToken = "Authorization: Bearer eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6ImZwaXJvdHRpIiwiZXhwIjoxNzgzNzA0MzQxLCJpYXQiOjE3Nzg1MjAzNDEsImlzcyI6Imh0dHBzOi8vdXJzLmVhcnRoZGF0YS5uYXNhLmdvdiIsImlkZW50aXR5X3Byb3ZpZGVyIjoiZWRsX29wcyIsImFjciI6ImVkbCIsImFzc3VyYW5jZV9sZXZlbCI6M30.7MzocbnbnY0FG73OZxwKGo4ug6ahOBeLV3D4aPIbpZTrH2B0n-zeuU4YlYmnmWomJbJbCAPz0Syx1dtYoTpjP9b5uddHbR3OUbjF85QPxE3nMwDydn6f6BVUAAbw8P07f-mlmjVf1rPvvQC_Fb974lbfTUqsePOdayqPZNe9-RaLZ_86gjlN_9FrEhPIIPd5vfXw1MayE9G81Gf3wsdi0QZOzlwihVtRqRpueKPaarCITMk4w17_Aeg1SDM-2tbkz59otZHPi7M9epDILQ4BIse3bFeq9Kq7We2R1WVAUen91dcgYlq0FV4-AOTkK9g74KDZmmzW2W_JH9hwLgSvDQ"
message("Using ", num_cores, " cores...")
temp_log_dir <- "parallel_logs"
if(!dir.exists(temp_log_dir)) dir.create(temp_log_dir)

source("0000_global.R")
if(!exists("baselines_named")){
  message("Running  01_gnss_prepare.R ... ")
  plotIt<-F
  source("01_gnss_prepare.R")
}

## output folder will be named <BASE>_<ROV> and contain POS
out.root <- dir.with.POS
if(!dir.exists(out.root)) dir.create(out.root)

conf_file <- file.path(this.path::this.dir(), "rtklibConf.conf")
cmd <- "rnx2rtkp"

download_nav_r <- function(date, destination, force=F) {
  year <- format(date, "%y")
  full_year <- format(date, "%Y")
  doy <- sprintf("%03d", as.numeric(format(date, "%j")))

  # 2. Construct the RINEX 3 filename pattern
  # Note: The '00' in '20260200000' represents HHMMSS (start of day)
  file_name <- sprintf("BRDC00IGS_R_%s%s0000_01D_MN.rnx.gz", full_year, doy)

  # 3. Construct the CDDIS Path
  # https://cddis.nasa.gov/archive/gnss/data/daily/2024/brdc/
  # Structure: /gnss/data/daily/YYYY/brdc/YYYY/
  url <- sprintf("https://cddis.nasa.gov/archive/gnss/data/daily/%s/brdc/%s",
                 full_year, file_name)
  dest_file <- file.path(destination, basename(url))
  if(file.exists(dest_file) && !force){
    return()
  }
  # 4. Download using curl (handles NASA's authentication/redirects best)
  message(paste("Downloading:", file_name))

  # -L follows redirects, -n or -u handles credentials
  cmd <- sprintf("curl -L -H \"%s\" -o %s %s",
                 authToken, dest_file, url)


  output <- system(cmd)
  if(file.exists(dest_file)){
    exit_status <- system(paste("uncompress -f", dest_file),
                          ignore.stdout = TRUE,
                          ignore.stderr = TRUE)

    if (exit_status != 0) {
      # Handle the error gracefully
      message(sprintf("System Error: 'uncompress' failed on file %s with code %d", dest_file, exit_status))
      # You can trigger a fallback action here or stop execution:
      stop("Decompression failed.")
      return(paste("FAILED :", basename(dest_file)))
    }
    return(paste("SUCCESS:", basename(dest_file)))
  }
  return(cmd)
}

#' runRTKLIB
#'
#' @param out.pos - folder or output file with .pos extension
#' @param B - full path to base rinex
#' @param R - full path to rover rinex
#' @param NAV  - full path to nav  rinex
#' @param dry - if true it returns string of command, does not run
#'
#' @returns
#' @export
#'
#' @examples
runRTKLIB <- function(out.pos, B, R, NAV, conf_file, cmd, dry=TRUE, force=F) {
  # 1. Thread-safe directory check
  # 'recursive = TRUE' and ignoring warnings prevents errors if another
  # thread creates the directory while this one is trying.
  ext <- tools::file_ext(out.pos)
  if(ext == "") {
    if(!dir.exists(out.pos)) {
      dir.create(out.pos, showWarnings = FALSE, recursive = TRUE)
    }

    # Construct filename
    nm <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(R)))
    year <- basename(dirname(R))
    nm2 <- sub("([0-9]{4})$", paste0("_", year, "_\\1"), nm)
    out.pos <- file.path(out.pos, sprintf("%s.pos", nm2))
  }

  if(file.exists(out.pos) && !force){
    return(paste("FILE EXISTS:", out.pos))
  }
  args <- c("-k", conf_file, "-o", out.pos, R, B, NAV)

  if(!dry) {
    # 2. Capture output to avoid garbled terminal logs
    result <- system2(command = cmd, args = args, stdout = FALSE, stderr = FALSE)

  } else {
    return(sprintf("DRY RUN: %s %s", cmd, paste(args, collapse=" ")))
  }

  if(result != 0) {
    return(paste("FAIL", paste(c(cmd, args), collapse=" ")))
  }

  return(paste("SUCCESS"))
}


timecoverage <- list()
## loop baselines -----
## files should be in folders with the name of station, then either 1sec or 30sec, and then
##the year, e.g. /gnss_data/BORC/30sec/2025/borc1520.25d.Z

nav.files <- list.files(nav.base, pattern=".*\\.rnx$", recursive = T, full.names = T)
nav.doy <- as.integer(substr(gsub("BRDC00IGS_R_","", basename(nav.files),fixed = T), 5,7))
nav.years <- as.integer(substr(gsub("BRDC00IGS_R_","", basename(nav.files),fixed = T), 1,4))
nav.dates <- as.Date(nav.doy - 1, origin = paste0(nav.years, "-01-01"))
for( i in 1:nrow(baselines_named)){

  tw <- baselines_named[i,]
  message(tw$baseline_name)
  logfile  <- file.path(temp_log_dir, paste0(tw$baseline_name, ".log"))
  cat(" ================ START ===============\n======  ", as.character(Sys.time()), " =====\n ==================================\n",
      file = logfile  )

  BASE <- sprintf("%s/%s",dir.with.rinex, tw$start_id)
  ROVER <- sprintf("%s/%s",dir.with.rinex, tw$end_id )
  bases <- list.files(BASE, pattern=".*\\.Z$", recursive = T, full.names = T)
  rovers <- list.files(ROVER, pattern=".*\\.Z$", recursive = T, full.names = T)

  doy <- as.integer(substr(gsub(tolower(tw$start_id),"", basename(bases),fixed = T), 1,3))
  yy <- (substr(gsub(BASE,"", bases,fixed = T), 8,11))
  TSbase <-    as.Date(doy - 1, origin = paste0(yy, "-01-01"))

  doy <- as.integer(substr(gsub(tolower(tw$end_id),"", basename(rovers),fixed = T), 1,3))
  yy <- (substr(gsub(ROVER,"", rovers,fixed = T), 8,11))
  TSrover <- as.Date(doy - 1, origin = paste0(yy, "-01-01"))

  timecoverage[[basename(BASE)]] <- TSbase
  timecoverage[[basename(ROVER)]] <- TSrover
  save(timecoverage, file="timecoverage.rda")
  ## match
  # get ID of bases matching rovers and viceversa
  stzBasesID <- which(TSbase %in% TSrover)
  stzRoversID <- which(TSrover %in% TSbase[stzBasesID])
  ids <- data.frame(basesIDs=stzBasesID,
                    roversIDs=stzRoversID,
                    bases=bases[stzBasesID],
                    rovers=rovers[stzRoversID],
                    basesDates = TSbase[stzBasesID],
                    roverDates = TSrover[stzRoversID] )

  # Define global variables locally so workers inherit them via fork
  NAV_idx_missing <- which(!ids$roverDates%in%nav.dates)
  if(length(NAV_idx_missing)>0) {
    message(paste(NAV_idx_missing))
    msgs <- lapply(NAV_idx_missing, function(datNAV){
      download_nav_r(ids$roverDates[[datNAV]], nav.base )
    })
  }
  # cat(unlist(msgs), sep = "\n", file = logfile, append = TRUE)

  conf_file_path <- conf_file
  rtk_cmd <- cmd
  # pbmc
  results <- pbmclapply(1:nrow(ids), function(row_idx) {
    row_data <- ids[row_idx, ]
    NAV_idx <- which(nav.dates == row_data$roverDates)

    if(length(NAV_idx) == 0) {
      return(paste("MISSING NAV"))
    }

    baseline <- file.path(out.root, tw$baseline_name)
    # Run the thread-safe function
    # Each worker returns a string
    res <- runRTKLIB(
      out.pos = baseline,
      B = row_data$bases,
      R = row_data$rovers,
      NAV = nav.files[[NAV_idx]],
      conf_file = conf_file_path,
      cmd = rtk_cmd,
      dry = FALSE,
      force = T
    )
    return(res)
  }
  , mc.cores = num_cores
  )

  # After parallel finishes, write all results to your logfile cleanly
  cat(unlist(results), sep = "\n", file = logfile, append = TRUE)
  torem <- list.files(file.path(out.root,tw$baseline_name), pattern="events.pos", full.names = T)
  successRem <- file.remove(torem)
}


pos.files <- list.files("POS", recursive = T, full.names = T)

## PLOT TIME OVERLAP ----

# ggplot(TS, aes(Date, Type)) +
#   geom_line() +
#   scale_x_date(
#     date_breaks = "1 month",
#     date_labels = "%b"
#   ) +
#   theme_minimal()
