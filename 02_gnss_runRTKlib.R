library(this.path) # For non-overlapping labels
library(pbmcapply)
library(parallel)
library(tools)
library(curl)
num_cores <- detectCores()/2 - 1
oldwd <- getwd()
setwd(this.path::this.dir())
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

# conf_file <- file.path(this.path::this.dir(), "rtklibConfUp.conf")
conf_file <- file.path(this.path::this.dir(), "confFiles/rtklibConf_ionoopt-brdc_tropopt-ztdgrad_sateph-precise.conf")
cmd <- "rnx2rtkp"

#' runRTKLIB
#'
#' @param out.pos - folder or output file with .pos extension
#' @param B - full path to base rinex
#' @param R - full path to rover rinex
#' @param NAV  - full path to nav  rinex
#' @param SP3  - full path to precise ephemeris SP3  rinex
#' @param BIA  - full path to BIA for ppp   rinex
#' @param dry - if true it returns string of command, does not run
#'
#' @returns
#' @export
#'
#' @examples
runRTKLIB <- function(out.pos, B, R, NAV, SP3=NULL, BIA=NULL,
                      conf_file=NULL, cmd=NULL, dry=TRUE, force=F) {
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

  if(!is.null(SP3)){
    args <-  c(args, SP3)
  }
  if(!is.null(BIA)){
    args <-  c(args, BIA)
  }



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
load( file="timecoverage.rda")
## loop baselines -----
## files should be in folders with the name of station, then either 1sec or 30sec, and then
##the year, e.g. /gnss_data/BORC/30sec/2025/borc1520.25d.Z

nav.files <- list.files(nav.base, pattern=".*\\.rnx$", recursive = T, full.names = T)
sp3.files <- list.files(nav.base, pattern=".*\\.sp3$", recursive = T, ignore.case = T, full.names = T)
bia.files <- list.files(nav.base, pattern=".*\\.bia$", recursive = T, ignore.case = T, full.names = T)
nav.dates <- extract_rinex_date(basename(nav.files))
sp3.dates <-  Date(length(sp3.files))
sp3.dates[nchar(basename(sp3.files))>13] <- extract_rinex_date(basename(sp3.files)[nchar(basename(sp3.files))>13])
sp3.dates[is.na(sp3.dates)] <- extract_rinex_date(basename(sp3.files)[is.na(sp3.dates)])
if(anyNA(sp3.dates)){
  browser()
  stop("Some na dates in SP3")
}
bia.dates <- extract_rinex_date(basename(bia.files))
if(anyNA(sp3.dates)){
  browser()
  stop("Some na dates in SP3")
}
##
baselines_named_AT <- data.frame(start_id="inbk", end_id="pat2", baseline_name="inbk_pat2")
for( i in 1:nrow(baselines_named)){

  tw <- baselines_named[i,]
  message(tw$baseline_name)

  logfile  <- file.path(temp_log_dir, paste0(tw$baseline_name, ".log"))
  cat("================ START ===============\n====  ", as.character(Sys.time()),
      "\n======================================\n",
      file = logfile  )

  # BASE <- sprintf("%s/%s",dir.with.rinex, tw$start_id)
  # ROVER <- sprintf("%s/%s",dir.with.rinex, tw$end_id )
  BASE <-   tw$start_id
  ROVER <-  tw$end_id
  bases <- list.files(dir.with.rinex, pattern=sprintf("^%s.*\\.(Z|zip|obs|o)$", tw$start_id),
                      recursive = T,
                      ignore.case = T,
                      full.names = T)
  rovers <- list.files(dir.with.rinex, pattern=sprintf("^%s.*\\.(Z|zip|obs|o)$", tw$end_id),
                       recursive = T,
                       ignore.case = T,
                       full.names = T)

  # dates.bases <- extract_rinex_date(bases)
  # doy <- as.integer(substr(gsub(tolower(tw$start_id),"", basename(bases),fixed = T), 1,3))
  # yy <- (substr(gsub(BASE,"", bases,fixed = T), 8,11))
  TSbase <- tryCatch(extract_rinex_date(bases),
                     warning=function(e){
                       browser()
                       stop(e$message)
                       })

  # doy <- as.integer(substr(gsub(tolower(tw$end_id),"", basename(rovers),fixed = T), 1,3))
  # yy <- (substr(gsub(ROVER,"", rovers,fixed = T), 8,11))
  TSrover <- extract_rinex_date(rovers)

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
  SP3_idx_missing <- which(!ids$roverDates%in%sp3.dates)
  BIA_idx_missing <- which(!ids$roverDates%in%bia.dates)
  if(length(NAV_idx_missing)>0) {
    msgs <- lapply(NAV_idx_missing, function(datNAV){
      download_nav_r(ids$roverDates[[datNAV]], nav.base )
    })
  }
  cat("=== NAV files match\n", file = logfile, append = TRUE)
  cat(unlist(msgs[!grepl("^SUCCESS", msgs)]), sep = "\n", file = logfile, append = TRUE)
  if(length(SP3_idx_missing)>0) {
    msgs <- lapply(SP3_idx_missing, function(datNAV){
      download_precise_r(ids$roverDates[[datNAV]], nav.base )
    })
  }

  cat("=== SP3 files match\n", file = logfile, append = TRUE)
  cat(unlist(msgs[!grepl("^SUCCESS", msgs)]), sep = "\n", file = logfile, append = TRUE)


  # if(length(BIA_idx_missing)>0) {
  #   msgs <- lapply(BIA_idx_missing, function(datNAV){
  #     download_precise_r(ids$roverDates[[datNAV]], nav.base, type = "bia" )
  #   })
  # }
  # cat("=== BIA files match\n", file = logfile, append = TRUE)
  # cat(unlist(msgs[!grepl("^SUCCESS", msgs)]), sep = "\n", file = logfile, append = TRUE)

  rtk_cmd <- cmd

  results <- pbmclapply(1:nrow(ids), function(row_idx) {
    row_data <- ids[row_idx, ]
    NAV_idx <- which(nav.dates == row_data$roverDates)
    SP3_idx <- which(sp3.dates == row_data$roverDates)
    # BIA_idx <- which(bia.dates == row_data$roverDates)

    if(length(NAV_idx) == 0 ) {
      return(paste("MISSING NAV in ", row_data$roverDates))
    }

    if(length(SP3_idx) == 0 ) {
      return(paste("MISSING SP3 in ", row_data$roverDates))
    }
    baseline <- file.path(out.root, tw$baseline_name)
    # Run the thread-safe function
    # Each worker returns a string
    res <- runRTKLIB(
      out.pos = baseline,
      B = row_data$bases,
      R = row_data$rovers,
      NAV = nav.files[[NAV_idx]],
      SP3 = sp3.files[[SP3_idx]],
      # BIA = bia.files[[BIA_idx]],
      conf_file = conf_file,
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
