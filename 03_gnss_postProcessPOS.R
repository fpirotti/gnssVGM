library(this.path) # For non-overlapping labels
library(pbmcapply)
library(parallel)
library(tools)
library(tidyverse)
library(data.table)
library(xts)
library(fasttime)


num_cores <- detectCores()/2 - 1
oldwd <- getwd()
setwd(this.path::this.dir())

message("Using ", num_cores, " cores...")
temp_log_dir <- "parallel_logs_pp"
if(!dir.exists(temp_log_dir)) dir.create(temp_log_dir)

source("0000_global.R")
if(!exists("baselines_named")){
  message("Running  01_gnss_prepare.R ... ")
  plotIt<-F
  source("01_gnss_prepare.R")
}

## output folder will be named <BASE>_<ROV> and contain POS
out.root <- dir.with.POS
if(!dir.exists(out.root)) {
  stop(out.root, " not exists")
}
baselineProcLog <- list()
process <- function(root){
  files=list.files(root,pattern="pos", full.names = T)

  # pbmc
  data_corr= lapply(files, function(x) {
    data.table::fread(x,skip=9,header=T, select = c(1,2:5,6),  stringsAsFactors=F)
    } )

  data_corr <- data.table::rbindlist(data_corr)

  names(data_corr)[1] <- "V1"
  setDT(data_corr)

  data_corr[, date := fastPOSIXct(
    paste(V1, GPST)
  )]

  ##only fixed results
  data_corr_fixed <- data_corr[Q == 1]

  # message("Removed ", nrow(data_corr) - nrow(data_corr_fixed), " not fixed position rows (",
  #         round((nrow(data_corr) - nrow(data_corr_fixed))/nrow(data_corr) *100,2),"%)")


  ##remove outliers - FP used a simple statistic to remove outliers
  median <- median( data_corr_fixed$`height(m)` )
  iqr <- IQR( data_corr_fixed$`height(m)`)
  high.t <- median+3*iqr
  low.t  <- median-3*iqr
  col <- "height(m)"
  data_corr_fixed[
    get(col) < low.t | get(col) > high.t,
    (col) := NA_real_
  ]

  data_corr_fixed.rmOutliers <- data_corr_fixed[
    !is.na(get(col))
  ][order(date)]

  df <- data.frame( Date=data_corr_fixed.rmOutliers$date,
                    MonthYear=factor(
                      format(data_corr_fixed.rmOutliers$date, "%Y-%m")
                      # levels = month.abb
                    ),
                   X = data_corr_fixed.rmOutliers$`longitude(deg)`,
                   Y = data_corr_fixed.rmOutliers$`latitude(deg)`,
                   Z = data_corr_fixed.rmOutliers$`height(m)`
  )
  dfsf <- df |> st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE) |> st_transform(crs = 3035)
  dfc <- st_coordinates(dfsf)
  df[, c("X","Y")]<-dfc
  median.x <- median( df$X )
  median.y <- median( df$Y )
  iqr.x <- IQR( df$X)
  iqr.y <- IQR( df$Y)

  df$relX <- df$X - median.x
  df$relY <- df$Y - median.y
  df$relZ <- df$Z - median

  png( file.path("PLOTS", paste0(basename(root), ".png")) , width=2000,
       height=4000,
       res=200)

     par(
        mfrow = c(4, 1),   # 2 rows, 1 column
        mar = c(4, 4, 2, 1)
      )
     plot( df$Date, df$relZ*1000, pch=".",
         main = paste("Residual position time series ", basename(root)),
         xlab = "Date", ylab="Z (mm)" )
     boxplot(
       Z ~ MonthYear,
       data = df,
       outline = FALSE,
       main = paste("Vertical Monthly Distribution", basename(root)),
       xlab = "Date",
       ylab = "Height (m) "
     )
     plot( df$Date, df$relX*1000, pch=".",
           xlab = "Date", ylab="X (mm)", ylim=c(-iqr.x*3000, iqr.x*3000) )
     plot( df$Date, df$relY*1000, pch=".",
           xlab = "Date", ylab="Y (mm)", ylim=c(-iqr.y*3000, iqr.y*3000) )
  dev.off()


  baselineProcLog[[basename(root)]] <<- c(
    n.totReadings = nrow(data_corr),
    n.unfixed=nrow(data_corr) - nrow(data_corr_fixed),
    percUnfixed=(nrow(data_corr) - nrow(data_corr_fixed))/nrow(data_corr) *100,
    n.afterOutlierRemoval = nrow(data_corr_fixed.rmOutliers),
    median = median,
    IQR=iqr
  )

  finalT <- as.data.frame(do.call(rbind, (baselineProcLog)))
#
# browser()
# x <- xts(
#   data_corr_fixed.rmOutliers$`height(m)`,
#   order.by = data_corr_fixed.rmOutliers$date
# )
#
# full_index <- seq(
#   min(data_corr_fixed.rmOutliers$date),
#   max(data_corr_fixed.rmOutliers$date),
#   by = "30 sec"
# )
#
# x_full <- merge(x, xts(, full_index))
# plot(x_full, main = "Height with gdata coverage", type = "l")
#
# points(index(x), rep(min(xh, na.rm=TRUE), length(xh)),
#        col = ifelse(!is.na(xh), "black", "red"),
#        pch = "|", cex = 0.5)
}
## PLOT -----
load(file="timecoverage.rda")
df <- enframe(timecoverage, name="station", value="date") |>
  unnest(date)

png("PLOTS/temporalOverlapVENETO.png", res=250, height=1500, width=2200)
ggplot(df, aes(date, station)) +
  geom_tile(height=0.9, width=0.9) +
  ggtitle(label = "Veneto Region GNSS Base Stations Overlap") +
  theme_classic()
dev.off()
baselines_named <- list.dirs(out.root, recursive=F)
for( baseline in baselines_named){
  message("Start ", basename(baseline))
  res <-  process(baseline)
}

save(baselineProcLog, file="baselineProcLog.rda")
