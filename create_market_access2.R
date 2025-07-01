## Load Packages
pacman::p_load(sf, terra, dplyr, h3jsr, progressr, data.table, readr)

## Set CRS
crs_utm38 <- "EPSG:32638"

## Define Paths
markets_dir   <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - Yemen Poverty Team FY23/Poverty Assessment/data/GIS/1_Original_Data/150_icl_access_to_markets/spatial/Marketpoints"
friction_dir  <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - Yemen Poverty Team FY23/Poverty Assessment/data/GIS/1_Original_Data/150_icl_access_to_markets/Market_access_raster"
shapefile_dir <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - Yemen Poverty Team FY23/Poverty Assessment/data/GIS/1_Original_Data/101_admin"

markets_csv <- file.path(markets_dir, "Marketpoints.csv")
mixed_proj  <- file.path(friction_dir, "mixed_model_raster_projected.tif")
adm2_shp    <- file.path(shapefile_dir, "Admin2.geojson")
out_csv     <- file.path(friction_dir, "h3_travel_time_output.csv")

## Load Data
adm2 <- st_read(adm2_shp, quiet = TRUE) |> st_transform(crs_utm38)

mkts <- read_csv(markets_csv) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  st_transform(crs_utm38)

fric_raster <- rast(mixed_proj)

## Create Grid Using H3
adm2_wgs <- st_transform(adm2, 4326)
h3_cells <- polygon_to_cells(adm2_wgs, res = 7, simple = FALSE)
h3_cells <- cell_to_polygon(unlist(h3_cells$h3_addresses), simple = FALSE)

grid_points <- h3jsr::cell_to_point(h3_cells, simple = FALSE) |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326) |>
  st_transform(crs_utm38)

grid_points$id <- grid_points$h3_address
grid_points <- grid_points |> distinct(id, .keep_all = TRUE)

## Deduplicate market points by H3 cell
mkts_wgs <- st_transform(mkts, 4326)
mkts_wgs$h3_index <- h3jsr::point_to_cell(mkts_wgs, res = 9)
mkts_unique <- mkts_wgs |> group_by(h3_index) |> slice(1) |> ungroup()
mkts <- st_transform(mkts_unique, crs_utm38)

## Set up progress bar
handlers(global = TRUE)
handlers("txtprogressbar")

## Compute Travel Time for One ADM2
compute_travel_time_adm2 <- function(adm2_all, adm_index) {
  poly <- adm2_all[adm_index, ]
  poly_id <- poly$admin2Pcod
  
  grid_sub <- grid_points[st_within(grid_points, poly, sparse = FALSE)[, 1], ]
  if (nrow(grid_sub) == 0) return(NULL)
  
  mkts_sub <- mkts[st_within(mkts, poly, sparse = FALSE)[, 1], ]
  if (nrow(mkts_sub) == 0) return(NULL)
  
  poly_ext <- ext(st_bbox(poly)) + 10000
  fric_crop <- crop(fric_raster, poly_ext)
  
  mkts_sub <- st_transform(mkts_sub, crs = crs(fric_crop))
  mkts_coords <- st_coordinates(mkts_sub)
  if (nrow(mkts_coords) == 0) return(NULL)
  
  acc_cost <- accCost(fric_crop, mkts_coords)
  
  grid_sub <- st_transform(grid_sub, crs = crs(fric_crop))
  grid_vect <- vect(grid_sub)
  travel_vals <- extract(acc_cost, grid_vect)
  
  df <- data.frame(
    h3_id = grid_sub$id,
    travel_time_min = travel_vals[, 2],
    ID_ADM = poly_id
  )
  
  return(df)
}

## Run for All ADM2s
progressr::with_progress({
  p <- progressor(along = 1:nrow(adm2))
  results <- lapply(seq_len(nrow(adm2)), function(i) {
    p()
    compute_travel_time_adm2(adm2, i)
  })
})

## Combine & Export
travel_time_all <- data.table::rbindlist(results, fill = TRUE)
fwrite(travel_time_all, out_csv)
