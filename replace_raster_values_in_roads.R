# ====================================================================
#  Travel-time to the nearest market (minutes) · Yemen
#  Author   : Chitra Balasubramanian
#  Date     : 2025-06-02 (Updated for fric_mx and fric_ft)
#  Purpose  : Assign high values to blocked roads & adjust AoC for both surfaces
# ====================================================================

## 0 · Packages -------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  sf, terra, raster, gdistance, dplyr, readr, h3jsr, progress, parallel,
  igraph, ggplot2, viridis, data.table
)

## 1 · Paths ----------------------------------------------------------
base_dir <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - 150_icl_access_to_markets/Market_access_raster"

# Define sub-directories
raw_dir <- file.path(base_dir, "raw")
intermediate_dir <- file.path(base_dir, "intermediate")

# Output paths for mixed (mx) and foot (ft)
output_path_mx             <- file.path(intermediate_dir, "fric_mx_roads_updated.tif")
output_path_road_opened_mx <- file.path(intermediate_dir, "fric_mx_roads_opened.tif")

output_path_ft             <- file.path(intermediate_dir, "fric_ft_roads_updated.tif")
output_path_road_opened_ft <- file.path(intermediate_dir, "fric_ft_roads_opened.tif")

## 2 · Read Inputs ----------------------------------------------------
fric_mx        <- rast(file.path(raw_dir, "fric_rast", "ymn_mx"))
fric_ft        <- rast(file.path(raw_dir, "fric_rast", "ymn_ft"))
roads          <- st_read(file.path(raw_dir, "blocked_roads", "yem_roads.shp"))
roads_opened   <- st_read(file.path(raw_dir, "blocked_roads", "non_passable_combined.shp"))
boundary_aoc   <- st_read(file.path(raw_dir, "aoc", "shared_boundary.shp"))

## 3 · Filter & Convert Roads -----------------------------------------
blocked_roads      <- roads %>% filter(Crrnt_s == "Not Passable")
blocked_roads_vect <- vect(blocked_roads)
roads_opened_vect  <- vect(roads_opened)
boundary_aoc_vect  <- vect(boundary_aoc)

# Combine all roads and project
all_roads_vect <- rbind(blocked_roads_vect, roads_opened_vect)

# Project for both rasters
roads_mx    <- project(all_roads_vect, crs(fric_mx))
roads_ft    <- project(all_roads_vect, crs(fric_ft))
boundary_mx <- project(boundary_aoc_vect, crs(fric_mx))
boundary_ft <- project(boundary_aoc_vect, crs(fric_ft))

# ====================================================================
# 4 · Update fric_mx --------------------------------------------------
# ====================================================================

# Buffer roads & rasterize
roads_buff_mx <- buffer(roads_mx, width = 100)
road_mask_path_mx <- file.path(intermediate_dir, "road_mask_mx_temp.tif")
road_mask_mx <- rasterize(roads_buff_mx, fric_mx, field = 1, filename = road_mask_path_mx, overwrite = TRUE)

# Assign max friction for blocked roads
global_max_mx <- global(fric_mx, fun = "max", na.rm = TRUE)[1, 1]
fric_mx_temp <- ifel(!is.na(rast(road_mask_path_mx)), global_max_mx, fric_mx)

# Assign smaller friction value (3) for AoC
boundary_buff_mx <- buffer(boundary_mx, width = 100)
boundary_mask_path_mx <- file.path(intermediate_dir, "boundary_mask_mx_temp.tif")
rasterize(boundary_buff_mx, fric_mx, field = 1, filename = boundary_mask_path_mx, overwrite = TRUE)
fric_mx_updated <- ifel(!is.na(rast(boundary_mask_path_mx)), 3, fric_mx_temp)

# Save updated raster
writeRaster(fric_mx_updated, output_path_mx, overwrite = TRUE)

# Reopen roads and revert to original
roads_opened_buff_mx <- buffer(roads_mx, width = 100)
road_opened_mask_path_mx <- file.path(intermediate_dir, "road_opened_mask_mx_temp.tif")
rasterize(roads_opened_buff_mx, fric_mx, field = 1, filename = road_opened_mask_path_mx, overwrite = TRUE)
fric_mx_roads_opened <- ifel(!is.na(rast(road_opened_mask_path_mx)), fric_mx, fric_mx_updated)

# Save opened version
writeRaster(fric_mx_roads_opened, output_path_road_opened_mx, overwrite = TRUE)


# ====================================================================
# 5 · Update fric_ft --------------------------------------------------
# ====================================================================

# Buffer roads & rasterize
roads_buff_ft <- buffer(roads_ft, width = 100)
road_mask_path_ft <- file.path(intermediate_dir, "road_mask_ft_temp.tif")
road_mask_ft <- rasterize(roads_buff_ft, fric_ft, field = 1, filename = road_mask_path_ft, overwrite = TRUE)

# Assign max friction for blocked roads
global_max_ft <- global(fric_ft, fun = "max", na.rm = TRUE)[1, 1]
fric_ft_temp <- ifel(!is.na(rast(road_mask_path_ft)), global_max_ft, fric_ft)

# Assign smaller friction value (3) for AoC
boundary_buff_ft <- buffer(boundary_ft, width = 100)
boundary_mask_path_ft <- file.path(intermediate_dir, "boundary_mask_ft_temp.tif")
rasterize(boundary_buff_ft, fric_ft, field = 1, filename = boundary_mask_path_ft, overwrite = TRUE)
fric_ft_updated <- ifel(!is.na(rast(boundary_mask_path_ft)), 3, fric_ft_temp)

# Save updated raster
writeRaster(fric_ft_updated, output_path_ft, overwrite = TRUE)

# Reopen roads and revert to original
roads_opened_buff_ft <- buffer(roads_ft, width = 100)
road_opened_mask_path_ft <- file.path(intermediate_dir, "road_opened_mask_ft_temp.tif")
rasterize(roads_opened_buff_ft, fric_ft, field = 1, filename = road_opened_mask_path_ft, overwrite = TRUE)
fric_ft_roads_opened <- ifel(!is.na(rast(road_opened_mask_path_ft)), fric_ft, fric_ft_updated)

# Save opened version
writeRaster(fric_ft_roads_opened, output_path_road_opened_ft, overwrite = TRUE)
