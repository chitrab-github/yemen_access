# ====================================================================
#  Travel-time to the nearest school (minutes) · Yemen
#  Author   : Chitra Balasubramanian
#  Date     : 2025-06-02 (updated for Pre/Post & Change analysis)
#  Purpose  : Calculate percent of population with school access
# ====================================================================

## 0 · Packages -------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  sf, terra, raster, gdistance, dplyr, readr, h3jsr, progress, parallel,
  igraph, ggplot2, viridis, data.table, exactextractr
)

## 1 · Set Model Type & Education Level -------------------------------
model_type   <- "ft"         # Options: "mx", "ft"
school_level <- "secondary"    # Options: "primary", "secondary"

## 2 · Paths ----------------------------------------------------------
base_dir   <- "C:/Users/wb569257/OneDrive - WBG/Alia Jane Aghajanian's files - 150_icl_access_to_markets/Market_access_raster"
pop_dir    <- file.path(base_dir, "raw", "pop_rast")
final_dir  <- file.path(base_dir, "final", "educ")
admin_dir  <- file.path(base_dir, "raw", "admin")
model_dir  <- file.path(final_dir, model_type)

## 3 · Read Inputs ----------------------------------------------------
tt_pre    <- rast(file.path(model_dir, paste0("tt_pre_", school_level, ".tif")))
tt_post   <- rast(file.path(model_dir, paste0("tt_post_", school_level, ".tif")))
tt_change <- rast(file.path(model_dir, paste0("tt_change_", school_level, ".tif")))
pop_rast  <- rast(file.path(pop_dir, "yem_pop_2025_CN_100m_R2024B_v1.tif"))
adm2      <- st_read(file.path(admin_dir, "Admin2.shp"))

## 4 · Align CRS ------------------------------------------------------
tt_pre    <- project(tt_pre, pop_rast)
tt_post   <- project(tt_post, pop_rast)
tt_change <- project(tt_change, pop_rast)

## 5 · Aggregate Travel Time to 100m ----------------------------------
tt_pre_agg    <- aggregate(tt_pre, fact = 100/30, fun = mean)
tt_post_agg   <- aggregate(tt_post, fact = 100/30, fun = mean)
tt_change_agg <- aggregate(tt_change, fact = 100/30, fun = mean)

## 6 · Resample to Match Population Grid -------------------------------
tt_pre_proj    <- resample(tt_pre_agg, pop_rast, method = "bilinear")
tt_post_proj   <- resample(tt_post_agg, pop_rast, method = "bilinear")
tt_change_proj <- resample(tt_change_agg, pop_rast, method = "bilinear")

## 7 · Create Binary Rasters -------------------------------------------
threshold <- ifelse(model_type == "mx", 60, 120)

tt_pre_bin  <- tt_pre_proj  <= threshold
tt_post_bin <- tt_post_proj <= threshold
tt_gain_bin <- (tt_pre_proj > threshold) & (tt_post_proj <= threshold)

## 8 · Convert to Raster for exactextractr -----------------------------
pop_r        <- raster::raster(pop_rast)
tt_pre_r     <- raster::raster(tt_pre_bin)
tt_post_r    <- raster::raster(tt_post_bin)
tt_gain_r    <- raster::raster(tt_gain_bin)

## 9 · Zonal Statistics (ADM2 Level) -----------------------------------
pop_total <- exact_extract(pop_r, adm2, 'sum')
pop_pre   <- exact_extract(pop_r * tt_pre_r, adm2, 'sum')
pop_post  <- exact_extract(pop_r * tt_post_r, adm2, 'sum')
pop_gain  <- exact_extract(pop_r * tt_gain_r, adm2, 'sum')

## 10 · Create Summary Table -------------------------------------------
summary_df <- adm2 %>%
  st_drop_geometry() %>%
  mutate(
    pop_total = pop_total,
    !!paste0("pop_educ_access_", threshold, "min_pre")  := pop_pre,
    !!paste0("pop_educ_access_", threshold, "min_post") := pop_post,
    !!paste0("pop_educ_access_", threshold, "min_gain") := pop_gain,
    !!paste0("share_educ_access_", threshold, "min_pre")  := (pop_pre / pop_total) * 100,
    !!paste0("share_educ_access_", threshold, "min_post") := (pop_post / pop_total) * 100,
    !!paste0("share_educ_access_", threshold, "min_gain") := (pop_gain / pop_total) * 100
  )

## 11 · Save Output ----------------------------------------------------
output_file <- paste0("adm2_educ_access_pre_post_change_", school_level, "_", model_type, ".csv")
write.csv(summary_df, file.path(model_dir, output_file), row.names = FALSE)
