library(httr)
library(rvest)
source("0000_global.R")
# log_file <- file("download_log.txt", open = "wt")
# sink(log_file, type = "message")
# sink(log_file)

cat("=======\n", file="forbidden.log")
# (Optional) If you also want to catch errors/warnings in the log file, uncomment this line:
# sink(log_file, type = "message")

download_data <- function(storage_path, year_url, station_name, interval, year, doy_str = NULL) {
  # GET request with Basic Auth
  res <- tryCatch(
    GET(year_url, authenticate("usnm", "psw")),
    error = function(e) return(NULL)
  )

  # Skip if URL doesn't exist or fails
  if (is.null(res) || status_code(res) >= 400) {
    # warning(year_url, " status code", res)
    return()
  }

  # Parse HTML
  page <- read_html(res)
  links <- html_nodes(page, "a")

  sub_links <- list()

  for (link in links) {
    href <- html_attr(link, "href")
    text <- html_text(link)

    # Equivalent to Python's strip('/')
    text <- sub("/$", "", text)

    if (!is.na(href) && text != "Parent Directory") {
      if (endsWith(href, "/") && (!is.null(doy_str) || interval == "30sec")) {
        sub_links[[length(sub_links) + 1]] <- list(local_name = text, href = href)
      } else if (grepl("\\.(zip|Z|rnx)$", href)) {
        sub_links[[length(sub_links) + 1]] <- list(local_name = text, href = href)
      }
    }
  }

  # Build local directory path
  local_base <- file.path(storage_path, station_name, interval, as.character(year))
  if (!is.null(doy_str)) {
    local_base <- file.path(local_base, doy_str)
  }

  # Ensure directories exist (exist_ok = True equivalent)
  dir.create(local_base, recursive = TRUE, showWarnings = FALSE)

  # Strip trailing slash from base url
  base_url <- sub("/$", "", year_url)

  for (item in sub_links) {
    local_name <- item$local_name
    href <- item$href

    # Construct the file URL
    file_url <- paste0(base_url, "/", ifelse(href != "", href, local_name))
    local_path <- file.path(local_base, local_name)

    if (file.exists(local_path)) {
      if(file.size(local_path) < 1e4){
        cat(sprintf("Warning %s: File already exists but overwriting.\n", local_name))
        file.remove(local_path)
      } else {
        cat(sprintf("Skipping %s: File already exists.\n", local_name))
        next
      }
    }

    # Download file and stream to disk

    res_file <-
      GET(file_url,
          authenticate("usnm", "psw"),
          user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
          write_disk(local_path, overwrite = FALSE))

    # Skip if URL doesn't exist or fails
    # if (is.null(res_file) || status_code(res_file) >= 400) return()
    if ( status_code(res_file) >= 400) {
      cat(file_url, "\n", file="forbidden.log", append=T)
    }
  }
}

# --- Main Script ---

storage_path <- dir.with.rinex
start_year <- 2016       # Starting year, to be changed according to plan
end_year <- 2026         # End year, to be changed according to plan
observation_intervals <- c("30sec") # Add "1sec" to this vector if needed

if(!dir.exists(storage_path)) dir.create(storage_path, recursive = TRUE, showWarnings = FALSE)

file_url <- "https://retegnssveneto.cisas.unipd.it/Dati/Rinex/"
response <- GET(file_url, authenticate("usnm", "psw"))
soup <- read_html(response)

links <- html_nodes(soup, "a")

for (link in links) {
  href <- html_attr(link, "href")
  text <- html_text(link)

  if (!is.na(href) && endsWith(href, "/") && text != "Parent Directory") {
    # Equivalent to Python's strip('/')
    station_name <- sub("/$", "", text)
    cat(sprintf("\nStation: %s\n", station_name))

    for (interval in observation_intervals) {
      cat(sprintf("Downloading %s observations...\n", interval))

      # Setup progress bar
      pb <- txtProgressBar(min = start_year - 1, max = end_year, style = 3)

      for (year in start_year:end_year) {
        if (interval == "1sec") {
          # Python's range(1, 366) equates to 1:365 in R
          for (doy in 1:365) {
            # zfill(3) equivalent
            doy_str <- sprintf("%03d", doy)
            year_url <- sprintf("%s%s/%s/%d/%s/", file_url, station_name, interval, year, doy_str)
          }
        } else {
          year_url <- sprintf("%s%s/%s/%d/", file_url, station_name, interval, year)
          doy_str <- NULL
        }

        res_file <- tryCatch({
          download_data(storage_path, year_url, station_name, interval, year, doy_str)
        },
        error = function(e) {
          browser()
          message("ERROR: ", e$message)
          return(e$message)
        },
        warning = function(e) {
          browser()
          message("warning: ", e$message)
          return(e$message)
        }
        )
        # Update progress bar
        setTxtProgressBar(pb, year)
      }
      close(pb)
    }
  }
}

sink(type = "message")
