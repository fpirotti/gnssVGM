## GLOBAL FUNCTIONS AND VARS -----
nav.base <- "/archivio/shared/gnssVGM/GW_VS_INSAR/apos/brdc"
dir.with.POS <- "/archivio/shared/gnssVGM/POS"
dir.with.rinex <- "/archivio/shared/gnssVGM/gnss_data"
atx <- normalizePath("igs20.atx")
authToken = sprintf("Authorization: Bearer %s", readLines(file("auth")))


extract_rinex_date <- function(file_path) {
  # Get just the filename, and convert the whole path to lowercase for easier matching
  filename <- basename(file_path) |> str_to_lower()
  path_lower <- str_to_lower(file_path)

  # -------------------------------------------------------------------------
  # FORMAT 1: Modern RINEX 3/4 Long Filename (e.g., "aaaa00bbb_r_YYYYDOYHHMM_...")
  # Matches a 4-digit year followed by a 3-digit DOY (total 7 digits)
  # -------------------------------------------------------------------------
r3_match <- str_match(filename, regex("_[rs]_(20\\d{2})(\\d{3})\\d{4}_", ignore_case = TRUE))
if (!is.na(r3_match[1])) {
    year <- as.integer(r3_match[,2])
    doy  <- as.integer(r3_match[,3])
    return(as.Date(doy - 1, origin = paste0(year, "-01-01")))
}
r3_match <- str_match(filename, "(\\d{4})_(\\d{3})\\.")
if (!is.na(r3_match[1])) {
  year <- as.integer(r3_match[,2])
  doy  <- as.integer(r3_match[,3])
  return(as.Date(doy - 1, origin = paste0(year, "-01-01")))
}

r3_match <- str_match(path_lower, "(?i)/(20\\d{2})/(\\d{3})/.*")
if (!is.na(r3_match[1])) {
  year <- as.integer(r3_match[,2])
  doy  <- as.integer(r3_match[,3])
  return(as.Date(doy - 1, origin = paste0(year, "-01-01")))
}

r3_match <- str_match(path_lower, "_(\\d{4})(\\d{3})")
if (!is.na(r3_match[1])) {
  year <- as.integer(r3_match[,2])
  doy  <- as.integer(r3_match[,3])
  return(as.Date(doy - 1, origin = paste0(year, "-01-01")))
}
  # -------------------------------------------------------------------------
  # FORMAT 2: Standard Legacy RINEX 2 Filename (e.g., "ssssddd0.yyo")
  # Matches 4 chars, 3-digit DOY, 1 char/digit, a dot, and a 2-digit year
  # -------------------------------------------------------------------------
  r2_match <- str_match(filename, regex("^[a-z0-9]{4}(\\d{3})[a-z0-9]\\.(\\d{2})[o|d|n|g|l|m]", ignore_case = TRUE) )
  if (!is.na(r2_match[1])) {
    doy      <- as.integer(r2_match[,1])
    year_2d  <- as.integer(r2_match[,2])
    # Handle the century window (e.g., 80-99 = 1900s, 00-79 = 2000s)
    year     <- if_else(year_2d >= 80, 1900 + year_2d, 2000 + year_2d)
    return(as.Date(doy - 1, origin = paste0(year, "-01-01")))
  }

  # -------------------------------------------------------------------------
  # FORMAT 3: Year/DOY hidden in the Folder Structure (e.g., "/2026/143/samp.26o")
  # Looks for a 4-digit folder followed by a 3-digit DOY folder
  # -------------------------------------------------------------------------
  path_match <- str_match(path_lower, "/(20\\d{2})/(\\d{3})/")
  if (!is.na(path_match[1])) {
    year <- as.integer(path_match[2])
    doy  <- as.integer(path_match[3])
    return(as.Date(doy - 1, origin = paste0(year, "-01-01")))
  }
  # -------------------------------------------------------------------------
  # FORMAT 4: Hatanaka / Compressed format variations (e.g., "samp1430.26d.z")
  # Strips common compression extensions and re-checks Legacy format
  # -------------------------------------------------------------------------
  cleaned_filename <- str_remove(filename, "\\.(z|gz|bz2|zip)$")
  r2_comp_match <- str_match(cleaned_filename, regex("^[a-z0-9]{4}(\\d{3})[a-z0-9]\\.(\\d{2})[d|o]", ignore_case = TRUE) )
  if (!is.na(r2_comp_match[1])) {
    doy      <- as.integer(r2_comp_match[1])
    year_2d  <- as.integer(r2_comp_match[2])
    year     <- if_else(year_2d >= 80, 1900 + year_2d, 2000 + year_2d)
    return(as.Date(doy - 1, origin = paste0(year, "-01-01")))
  }

  # Return NA if all regex patterns fail to parse a valid date
  return(NA)
}



download_precise_r <- function(date, destination, type = c("sp3", "bia"), force = FALSE) {
  type <- match.arg(type)

  full_year <- format(date, "%Y")
  doy       <- sprintf("%03d", as.numeric(format(date, "%j")))

  # 1. Calculate the 4-digit GPS Week & Day of Week
  gps_epoch <- as.Date("1980-01-06")
  days_diff <- as.numeric(date - gps_epoch)
  gps_week  <- floor(days_diff / 7)
  gps_day   <- days_diff %% 7

  # 2. Split logic for filenames and dynamic URL building
  is_legacy <- date < as.Date("2022-11-27")

  if (is_legacy) {
    # Legacy Format (Short names)
    file_name <- sprintf("igs%d%d.%s.Z", gps_week, gps_day, type)
  } else {
    # Modern Format (Long names)
    yyddd     <- paste0(full_year, doy)
    suffix    <- ifelse(type=="sp3", "01D_05M_ORB.SP3.gz",   "01D_05M_OSB.BIA.gz" )
    file_name <- sprintf("IGS0DEMFIN_%s0000_%s", yyddd, suffix)
  }

  # 3. Construct the official CDDIS Path targeting the GPS Week folder
  url <- sprintf("https://cddis.nasa.gov/archive/gnss/products/%d/%s",
                 gps_week, file_name)

  dest_file <- file.path(destination, file_name)

  # Determine final uncompressed filename (removes either .gz or .Z)
  final_uncompressed_file <- file.path(destination, sub("\\.(gz|Z)$", "", file_name, ignore.case = TRUE))

  # Check if uncompressed file already exists to skip re-downloading
  if (file.exists(final_uncompressed_file) && !force) {
    return(paste("EXISTS:", basename(final_uncompressed_file)))
  }

  if (!dir.exists(destination)) dir.create(destination, recursive = TRUE)

  # 4. Download using curl (with correct Bearer Token header parsing)
  message(paste("Downloading precise", type, "from GPS Week", gps_week, ":", file_name))

  # Added -s (silent) to clean terminal logs, and added explicit quote formatting around paths
  cmd <- sprintf("curl -s -L -H \"%s\" -o \"%s\" \"%s\"",
                 authToken, dest_file, url)
  exit_code <- system(cmd)

  # Verify if download was successful and not a ghost 404 HTML file
  if (exit_code != 0 || !file.exists(dest_file) || file.size(dest_file) < 5000) {
    if (file.exists(dest_file)) file.remove(dest_file)
    return(paste("FAILED TO DOWNLOAD:", file_name))
  }

  # 5. Dual Decompression Logic (.Z vs .gz)
  tryCatch({
    if (is_legacy) {
      # .Z files use standard system uncompress
      system(paste("uncompress -f", shQuote(dest_file)),
             ignore.stdout = TRUE, ignore.stderr = TRUE)
    } else {
      # .gz files use R's native cross-platform decompression
      gz_con   <- gzfile(dest_file, "rb")
      raw_data <- readBin(gz_con, "raw", n = 5e7) # Buffer limits up to 50MB
      close(gz_con)

      writeBin(raw_data, final_uncompressed_file)
      file.remove(dest_file)
    }

    return(paste("SUCCESS:", basename(final_uncompressed_file)))

  }, error = function(e) {
    if (file.exists(dest_file)) file.remove(dest_file)
    stop(paste("Decompression failed:", e$message))
  })
}


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
