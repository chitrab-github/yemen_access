## Load Packages
pacman::p_load(sf, terra, dplyr, readr)

## Set CRS
crs_utm38 <- "EPSG:32638"

## Define Paths
markets_dir   <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - Yemen Poverty Team FY23/Poverty Assessment/data/GIS/1_Original_Data/150_icl_access_to_markets/spatial/Marketpoints"
friction_dir  <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - Yemen Poverty Team FY23/Poverty Assessment/data/GIS/1_Original_Data/150_icl_access_to_markets/Market_access_raster"
shapefile_dir <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - Yemen Poverty Team FY23/Poverty Assessment/data/GIS/1_Original_Data/101_admin"
population_dir <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - Yemen Poverty Team FY23/Poverty Assessment/data/GIS/1_Original_Data/103_worldpop_population"

adm2_shp    <- file.path(shapefile_dir, "Admin2.geojson")
tt_mixed_100_hrs  <- file.path(friction_dir, "tt_mixed_yem_100_hrs.tif")
pop_rast <- file.path(population_dir, "ppp_2020_1km_Aggregated.tif")

## Load Data
fric_raster <- rast(tt_mixed_100_hrs)
pop2020 <- rast(pop_rast)
adm2 <- st_read(adm2_shp, quiet = TRUE) |> st_transform(crs_utm38)
adm2_vect <- vect(adm2)

# Ensure both rasters are in same CRS
if (crs(fric_raster) != crs_utm38) crs(fric_raster) <- crs_utm38
if (crs(pop2020) != crs_utm38) pop2020 <- project(pop2020, crs_utm38)

## Clip and Mask Travel Time Raster to ADM2 Extent
fric_crop <- crop(fric_raster, adm2_vect)
fric_mask <- mask(fric_crop, adm2_vect)
plot(fric_mask)

## Step 1: Aggregate 100m travel time to 1km resolution using mean
fric_1km_mean <- aggregate(fric_mask, fact = 10, fun = mean, na.rm = TRUE)

## Step 2: Resample population raster to match travel time raster (1km)
pop_resampled <- resample(pop2020, fric_1km_mean, method = "bilinear")

## Step 3: Create mask for cells with 0 < travel time < 2 hour
access_mask <- fric_1km_mean < 2 & fric_1km_mean > 0

## Step 4: Mask population raster using access mask
pop_access <- mask(pop_resampled, access_mask, maskvalues = FALSE)


## Step 6: Extract total and accessible population per ADM2
pop_total_df <- extract(pop_resampled, adm2_vect, fun = sum, na.rm = TRUE, bind = TRUE) |>
  as.data.frame()
names(pop_total_df)[6] <- "total_pop"


pop_access_df <- extract(pop_access, adm2_vect, fun = sum, na.rm = TRUE, bind = TRUE) |>
  as.data.frame()
names(pop_access_df)[6] <- "accessible_pop"

## Step 7: Join and calculate percent access
pop_summary <- left_join(pop_total_df, pop_access_df, by = "admin2Pcod") |>
  mutate(
    percent_access = 100 * (accessible_pop / total_pop),
    percent_access = ifelse(is.nan(percent_access), NA, percent_access)
  )
## Step 8 (Optional): Join back to ADM2 geometry for mapping or export
adm2_summary <- left_join(adm2, pop_summary, by = "ID")

## Step 9 (Optional): View or export
head(adm2_summary[, c("ID", "total_pop", "accessible_pop", "percent_access")])

# Export (optional)
# write.csv(st_drop_geometry(adm2_summary), "adm2_market_access_summary.csv", row.names = FALSE)
# writeVector(vect(adm2_summary), "adm2_market_access_summary.shp")