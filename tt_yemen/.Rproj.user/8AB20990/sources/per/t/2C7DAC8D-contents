# ====================================================================
#  Travel-time to the nearest market (minutes)  ·  Yemen
#  Author   : Chitra Balasubramanian
#  Date     : 2025-06-02   (updated with H3 grid indexing)
#  Purpose  : Assign high values to areas of control and blocked roads
# ====================================================================

## 0 · Packages -------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  sf, terra, raster, gdistance, dplyr, readr, h3jsr, progress, parallel,
  igraph, ggplot2, viridis, data.table
)

## 1 · Paths ----------------------------------------------------------
friction_dir  <-  "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - 150_icl_access_to_markets/Market_access_raster"
roads_dir     <-  "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - 150_icl_access_to_markets/blocked_roads"
output_path   <-  file.path(friction_dir, "fric_mx_roads_updated.tif")

## 2 · Read inputs ----------------------------------------------------
fric_mx        <- rast(file.path(friction_dir, "ymn_mx"))
roads          <- st_read(file.path(roads_dir, "yem_roads.shp"))
roads_opened   <- st_read(file.path(roads_dir, "non_passable_combined.shp"))
boundary_aoc   <- st_read(file.path(roads_dir, "shared_boundary.shp"))

## 3 · Filter and convert roads ---------------------------------------
blocked_roads       <- roads %>% filter(Crrnt_s == "Not Passable")
blocked_roads_vect  <- vect(blocked_roads)
roads_opened_vect   <- vect(roads_opened)
boundary_aoc_vect   <- vect(boundary_aoc)

# Combine and project
all_roads_vect <- rbind(blocked_roads_vect, roads_opened_vect)
all_roads_vect <- project(all_roads_vect, crs(fric_mx))

## 4 · Buffer and rasterize to disk -----------------------------------
# Slight buffer to ensure raster overlap
all_roads_buff <- buffer(all_roads_vect, width = 100)

# Save rasterized mask directly to disk
road_mask_path <- file.path(friction_dir, "road_mask_temp.tif")
road_mask <- rasterize(all_roads_buff, fric_mx, field = 1, filename = road_mask_path, overwrite = TRUE)

## 5 · Replace values and write output to disk ------------------------
global_max <- global(fric_mx, fun = "max", na.rm = TRUE)[1, 1]

fric_mx_updated <- ifel(!is.na(rast(road_mask_path)), global_max, fric_mx)
writeRaster(fric_mx_updated, output_path, overwrite = TRUE)

## 6 · Check and visualize --------------------------------------------
# Optional plot to visually confirm
plot(fric_mx_updated, main = "Updated Raster (Roads Assigned Global Max)")
plot(all_roads_buff, col = "red", add = TRUE)

# Optional: Verify replacement
vals <- extract(fric_mx_updated, all_roads_buff)[[2]]
table(round(vals, 5))
