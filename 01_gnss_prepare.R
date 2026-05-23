library(ggplot2)
library(sf)
library(tidyverse)
library(ggrepel) # For non-overlapping labels
source("0000_global.R")
## READS A FOLDER for subfolders with stations' rinex OBS files and reads coordinates
## it then creates a TIN network for possible post processing baselines < 100 km
## so that baselines can be processed by rtklib using the next R script
setwd(this.path::this.dir())
extract_rinex_position <- function(file_path) {
  con <- NULL

  # Check if we need to force decompression via system pipe for .Z / .gz files
  if (endsWith(file_path, ".Z") || endsWith(file_path, ".gz")) {

    # Windows native alternative if gzip isn't in PATH, otherwise standard Unix/Mac pipe
    if (.Platform$OS.type == "windows") {
      # Windows 10/11 has 'tar' built-in, which handles .Z decompression flawlessly
      cmd <- sprintf("tar -xf \"%s\" -O", file_path)
    } else {
      # Linux / Mac standard command
      cmd <- sprintf("gzip -dc \"%s\"", file_path)
    }

    # Open a text pipe to catch the uncompressed stream
    con <- pipe(cmd, open = "rt")

  } else if (endsWith(file_path, ".zip")) {
    unzippeds <- unzip(file_path, list = TRUE)
    unzipped <- grep("o$",unzippeds$Name, value = T)
    if(length(unzipped)==0){
      browser()
    }
    con <- unz(file_path, unzipped[[1]], open = "rt")
  } else {
    con <- file(file_path, open = "rt")
  }

  # Ensure connection closes safely
  on.exit(if (!is.null(con)) close(con))

  # Now we can safely read plain text lines because the OS did the heavy unpacking!
  lines <- tryCatch(
    readLines(con, n = 50, warn = FALSE),
    error = function(e) return(NULL)
  )


  if (is.null(lines) || length(lines) == 0) return(NULL)

  # Scan for coordinates
  for (line in lines) {
    if (grepl("APPROX POSITION XYZ", line)) {
      matches <- unlist(regmatches(line, gregexpr("-?\\d+\\.\\d+|-?\\d+", line)))
      coords <- as.numeric(matches[1:3])

      if (!any(is.na(coords)) && length(coords) == 3) {
        return(c(X = coords[1], Y = coords[2], Z = coords[3]))
      }
    }
    if (grepl("END OF HEADER", line)) break
  }

  return(NULL)
}
## 1. READS ALL STATION LOCATIONS -----
# put here the path toe rinex of stations e.g.
# /archivio/shared/gnssVGM/gnss_data if rinex in
# /archivio/shared/gnssVGM/gnss_data/BOCN/30sec/2025

if(!file.exists("stz.rda")){
  stations <- unique(substr(basename(list.files(dir.with.rinex, recursive = T)), 1,4) )

  stz.coord <- list()
  for(station in stations){
    stzname <- station #gsub("gnss_data/", "", station)
    dat <- list.files(pattern = sprintf("%s.*\\.(Z|zip)", stzname),ignore.case = T,
                      full.names = T, recursive = T)

    cli::cli_inform(stzname)
    if(length(dat)==0)   {
      cli::cli_inform("non found!")
      next
    }

    nfile <- 1:length(dat)
    for(i in nfile){
      file <- dat[[i]]
      stz.coord[[stzname]] <- extract_rinex_position(file)
      if(is.null( stz.coord[[stzname]]) ||  as.integer(stz.coord[[stzname]][["Z"]]) < 1e6 ){
        cat(sprintf("\rProcessing item: %d%%", i))
        # Flush the console output immediately so it updates in real-time
        flush.console()
      } else {
        break
      }
    }

    cli::cli_inform(stz.coord[[stzname]])
  }

  # cc <- lapply(stz.coord, function(x) {
  #   ccr <- strsplit(x, ",")
  #   as.numeric(ccr[[1]])
  #   } )

  tab<-as.data.frame(do.call(rbind, stz.coord))
  tab$id <- basename(rownames(tab))
  p_sf <- st_as_sf(tab, coords = c("X", "Y", "Z"), crs = 4978) |> sf::st_transform(4326)
  coords <- sf::st_coordinates(p_sf)
  p_sf$lon <- coords[,1]
  p_sf$lat <- coords[,2]
  save(p_sf, file="stz.rda")

} else {
  load(file="stz.rda")
}

## 2. CREATES A TIN
p_sf.latlong <- p_sf
p_sf <- p_sf |> sf::st_transform(3035)

points_union <- st_union(p_sf)
tin_polygons <- st_triangulate(points_union) |> st_collection_extract() |>  st_cast( "POLYGON")

baselines <- st_cast(tin_polygons, "LINESTRING") %>%
  st_as_sf(sf_column_name ="x") %>%
  distinct(x, .keep_all = TRUE)

baselines.small <- baselines |> filter(as.numeric(sf::st_length(baselines))/1000 < 100)


baselines_named <- baselines.small %>%
  mutate(
    # Get the first point of the line
    start_pt = st_line_sample(x, sample = 0),
    # Get the last point of the line
    end_pt = st_line_sample(x, sample = 1)
  ) %>%
  # Join for the Start Station
  st_join(p_sf %>% select(start_id = id), join = st_intersects, left = TRUE, substitute = start_pt) %>%
  # Join for the End Station
  st_join(p_sf %>% select(end_id = id), join = st_intersects, left = TRUE, substitute = end_pt) %>%
  # Clean up and create a unique baseline label
  mutate(baseline_name = paste0(start_id, "_", end_id)) |>
  filter(start_id != end_id) |>

  mutate(
    # Standardize direction: column 1 gets the smaller ID, column 2 gets the larger ID
    true_start = pmin(start_id, end_id),
    true_end   = pmax(start_id, end_id)
  )|>

  # Group and slice to force unique combinations
  group_by(true_start, true_end) |>
  slice(1) |>
  ungroup() |>

  # Create final label strings
  mutate(baseline_name = paste0(true_start, "_", true_end)) |>

  # Explicitly select attributes, avoiding standard sf rename conflicts
  as_tibble() |>
  select(
    start_id = true_start,
    end_id = true_end,
    baseline_name,
    geom = x
  ) |>
  # Convert back to an official sf object with the correct geometry column assigned
  st_as_sf(sf_column_name = "geom")

plotIt=F
if(plotIt){
  nuts <- sf::read_sf("/archivio/shared/geodati/vector/NUTS_2024_all_4326v2.gpkg")
  p_sf_4326 <- baselines |> sf::st_transform(4326)
  provs <- nuts |> dplyr::filter(CNTR_CODE=="IT",LEVL_CODE==3) |>
    sf::st_filter(p_sf_4326, .predicate = sf::st_intersects)

  plot_limits <- st_bbox(p_sf |> st_buffer(10000) )
  library(ggspatial)
  png("mappedBaselinesNoBG.png", res=300, width=1800, height=1800)
    p<-ggplot(p_sf) +
      layer_spatial(data = provs,
                    aes(fill = substr(NUTS_ID, 1,4)), ,
                    show.legend = FALSE, alpha=1,
                    linewidth = 0.4) +
       # annotation_map_tile(type = "cartolight", zoom = 8) +
    # Draw the TIN Baselines
    geom_sf(data = baselines.small,
            color = "black",
            linewidth = 0.4,
            alpha = 0.8,
            linetype = "dashed") +
    # Draw the Stations
    geom_sf(data = p_sf,
            color = "firebrick",
            size = 3) +
    geom_label_repel(data = cbind(as.data.frame(sf::st_coordinates(p_sf)), id=p_sf$id), aes(x = X, y = Y, label = id),
                     box.padding = 0.5, segment.color = 'grey50',
                     size = 3, fontface = "bold") +
      annotation_scale(location = "bl", width_hint = 0.2) +
      annotation_north_arrow(location = "tr", which_north = "true",
                             style = north_arrow_fancy_orienteering) +
    theme_classic() +
      coord_sf(
        xlim = c(plot_limits["xmin"], plot_limits["xmax"]),
        ylim = c(plot_limits["ymin"], plot_limits["ymax"]),
        expand = TRUE # Set to TRUE to add a tiny padding around points so labels don't cut off
      ) +
    labs(title = "GNSS Network Reg. Veneto/PAT Seasonal VGM",
         subtitle = "Delaunay Triangulation Baselines < 100 km",
         x = "Longitude",
         y = "Latitude") +
    theme(panel.grid.major = element_line(color = "gray90"),
          panel.background = element_rect(fill = "cadetblue1", color = NA) )

    print(p)
  dev.off()

  png("mappedBaselinesNoBG.png", res=300, width=1800, height=1800)
  p<-ggplot(p_sf) +
    # annotation_map_tile(type = "cartolight", zoom = 8) +
    # Draw the TIN Baselines
    geom_sf(data = baselines.small,
            color = "steelblue",
            linewidth = 0.5,
            alpha = 0.7,
            linetype = "dashed") +
    # Draw the Stations
    geom_sf(data = p_sf,
            color = "firebrick",
            size = 3) +
    geom_label_repel(data = cbind(as.data.frame(sf::st_coordinates(p_sf)), id=p_sf$id), aes(x = X, y = Y, label = id),
                     box.padding = 0.5, segment.color = 'grey50',
                     size = 3, fontface = "bold") +
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
    theme_classic() +
    labs(title = "GNSS Network Reg. Veneto/PAT Seasonal VGM",
         subtitle = "Delaunay Triangulation Baselines < 100 km",
         x = "Longitude",
         y = "Latitude")
    # theme(panel.grid.major = element_line(color = "gray90"))

  print(p)
  dev.off()

}
