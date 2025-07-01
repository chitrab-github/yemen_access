# ====================================================================
#  Travel-time to the nearest market (minutes)  ·  Yemen
#  Author   : Chitra Balasubramanian
#  Date     : 2025-06-02   (updated with H3 grid indexing)
#  Purpose  : Generate H3 grid points inside ADM2, compute travel time
# ====================================================================

## 0 · Packages -------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  sf, terra, raster, gdistance, dplyr, readr, h3jsr, progress, parallel,
  igraph, ggplot2, viridis, data.table
)

## 1 · Paths ----------------------------------------------------------

markets_dir   <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - Yemen Poverty Team FY23/Poverty Assessment/data/GIS/1_Original_Data/150_icl_access_to_markets/spatial/Marketpoints"
friction_dir  <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - Yemen Poverty Team FY23/Poverty Assessment/data/GIS/1_Original_Data/150_icl_access_to_markets/Market_access_raster"
shapefile_dir <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - Yemen Poverty Team FY23/Poverty Assessment/data/GIS/1_Original_Data/101_admin"

markets_csv <- file.path(markets_dir, "Marketpoints.csv")
mixed_proj  <- file.path(friction_dir, "mixed_model_raster_projected.tif")
adm2_shp    <- file.path(shapefile_dir, "Admin2.geojson")
adm0_shp    <- file.path(shapefile_dir, "Yemen Admin 0", "Admin0.shp")
out_csv     <- file.path(dirname(friction_dir), "h3_travel_time_output.csv")

crs_utm38 <- "EPSG:32638"

## 2 · Load and Prepare Data ------------------------------------------

adm2 <- st_read(adm2_shp, quiet = TRUE) |> st_transform(crs_utm38)

mkts <- read_csv(markets_csv) |> 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |> 
  st_transform(crs_utm38)

## 3 · Create H3 Grid Points ------------------------------------------

# H3 grid resolution — adjust as needed (e.g., 9 = ~100m)
h3_res <- 9

# Transform ADM2 to WGS84 for H3
adm2_wgs <- st_transform(adm2, 4326)

h3_cells <- polygon_to_cells(adm2_wgs, res = 7, simple = FALSE)
h3_cells <- cell_to_polygon(unlist(h3_cells$h3_addresses), simple = FALSE)


# Convert H3 indexes to center point geometries
grid_points <- h3jsr::cell_to_point(h3_cells, simple = FALSE)

# Convert to sf and reproject to match friction surface
grid_points <- st_as_sf(grid_points, coords = c("lng", "lat"), crs = 4326) |>
  st_transform(crs_utm38)

# Add ID column
grid_points$id <- grid_points$h3_address


## 2b · Reduce Market Points to Unique H3 Cells -----------------------

# Convert to WGS84 for H3 indexing
mkts_wgs <- st_transform(mkts, 4326)

# Assign H3 index to each market point
h3_res <- 9  # Same resolution used for grid_points
mkts_wgs$h3_index <- h3jsr::point_to_cell(mkts_wgs, res = h3_res)

# Keep only one market per H3 cell
# Option 1: Keep the first market per cell
mkts_unique <- mkts_wgs |> 
  group_by(h3_index) |> 
  slice(1) |> 
  ungroup()

# Reproject back to UTM for travel time calculation
mkts <- st_transform(mkts_unique, crs_utm38)



## 4 · Travel Time Function -------------------------------------------

friction_matrix <- function(x, y, friction, temp_file) {
  # Get CRS of the raster
  friction_crs <- raster::crs(friction)
  
  # Align CRS
  # Align CRS
  if (!identical(st_crs(x)$wkt, friction_crs@projargs)) {
    x <- st_transform(x, crs = friction_crs)
  }
  if (!identical(st_crs(y)$wkt, friction_crs@projargs)) {
    y <- st_transform(y, crs = friction_crs)
  }
  
  # Create buffered extent around origin + destination
  coords_all <- rbind(st_coordinates(x), st_coordinates(y))
  
  # Add a tighter buffer (in meters, assuming CRS is projected)
  buffer_m <- 5000  # 5 km
  xmin <- min(coords_all[, 1]) - buffer_m
  xmax <- max(coords_all[, 1]) + buffer_m
  ymin <- min(coords_all[, 2]) - buffer_m
  ymax <- max(coords_all[, 2]) + buffer_m
  

  ext <- raster::extent(xmin, xmax, ymin, ymax)

  # Crop friction surface
  friction <- raster::crop(friction, ext)
  
  # Create cost surface
  T <- gdistance::transition(friction, function(x) 1/mean(x), 8)
  T.GC <- gdistance::geoCorrection(T)
  rm(T); gc()
  
  # Add IDs if not present
  if (!"id" %in% names(x)) x$id <- seq_len(nrow(x)) 
  if (!"id" %in% names(y)) y$id <- seq_len(nrow(y))
  
  # Convert to sp objects
  x_sp <- as(x, "Spatial")
  y_sp <- as(y, "Spatial")
  rm(x, y); gc()
  
  # Write to temporary file
  temp_dir <- dirname(temp_file)
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
  con <- file(temp_file, "w")
  writeLines("id_o,id_d,dur", con)
  
  # Chunked processing
  chunk_size <- 500
  n_chunks <- ceiling(nrow(x_sp) / chunk_size)
  
  message(sprintf("Processing %d chunks...", n_chunks))
  pb <- progress::progress_bar$new(
    format = "  calculating distances [:bar] :percent eta: :eta",
    total = n_chunks, clear = FALSE, width = 80
  )
  
  for (i in seq_len(n_chunks)) {
    start_idx <- ((i - 1) * chunk_size) + 1
    end_idx <- min(i * chunk_size, nrow(x_sp))
    chunk_x <- x_sp[start_idx:end_idx, ]
    
    # Compute cost distance
    dist <- costDistance(T.GC, fromCoords = chunk_x, toCoords = y_sp)
    
    chunk_results <- data.frame(
      id_o = x_sp$id[start_idx:end_idx],
      id_d = y_sp$id[apply(dist, 1, which.min)],
      dur  = apply(dist, 1, min)
    )
    
    write.table(chunk_results, con, sep = ",", row.names = FALSE, 
                col.names = FALSE, append = TRUE)
    
    rm(dist, chunk_results); gc()
    pb$tick()
  }
  
  close(con)
  dist.mat <- data.table::fread(temp_file)
  unlink(temp_file)
  
  return(dist.mat)
}

## 5 · Run the Travel Time Calculation --------------------------------

fric_raster <- raster(mixed_proj)

grid_sample <- grid_points[1:3, ]
mkts_sample <- mkts[1:10, ]

travel_time_df <- friction_matrix(
  x = grid_sample,
  y = mkts_sample,
  friction = fric_raster,
  temp_file = out_csv
)
#####################

travel_time_df <- friction_matrix(
  x = grid_points[1:5,],
  y = mkts,
  friction = fric_raster,
  temp_file = out_csv
)
