# Teakettle Experimental Forest NDVI from Sentinel-2 L2A (Planetary Computer)
# install.packages(c("rstac", "terra", "sf"))

library(rstac)
library(terra)
library(sf)
library(googledrive)

#----------------Download Spatial Data ----------------------------------------

#First, outlet (general area of interest shapefile)
folder_id <- "1dDzLAm_ucSO6EAnGyB18iTL5eOdaT5mV"
files <- drive_ls(as_id(folder_id), recursive = TRUE)
out_dir   <- "googledrive"

for (i in seq_len(nrow(files))) {
  drive_download(
    file      = as_id(files$id[i]),
    path      = file.path(out_dir, files$name[i]),
    overwrite = TRUE
  )
}

#Next get the csv with fuel transect GPS coordinates

folder <- as_id("1iy04HMKxdeL4xKv0bJqeZoYHukhzyfDU")
files <- drive_ls(folder)
file <- files[files$name == "Teakettle_Fuels.csv", ]

drive_download(file, 
               path = "Teakettle_Fuels.csv", 
               overwrite = TRUE)

fuel_GPS <- read.csv("Teakettle_Fuels.csv")

fuel_GPS <- fuel_GPS %>%
  mutate(transect = as.numeric(gsub("\\D", "", Name)))   # "FT12" -> 12

#merge GPS with fuel calculations
paired_fuels <- paired_fuels %>%
  mutate(transect = as.numeric(transect)) %>%
  left_join(fuel_GPS, by = "transect")

#Get Teakettle Gridpoints gps coordinates

folder <- as_id("1iy04HMKxdeL4xKv0bJqeZoYHukhzyfDU")
files <- drive_ls(folder)
file <- files[files$name == "Teakettle_Gridpoints.gpx", ]

drive_download(file, 
               path = "Teakettle_Gridpoints.gpx", 
               overwrite = TRUE)

gridpoints <- st_read("Teakettle_Gridpoints.gpx", layer = "waypoints")
gridpoints$Plot <- as.numeric(gridpoints$name)


#---------Get Teakettle Gripoint Carbon/Biomass data for 2022 and 2026----------------
## Note: In 2022 half the plots were re-read using better guidelines for litter/duff/rotting log estimates.  I am using this subset of plots rather than the full plot network for the best data.

folder <- as_id("1AATNCB0taGl6pY7CG60nvgs0pM00k5Dz")
files  <- drive_ls(folder)

if (!dir.exists("googledrive")) dir.create("googledrive")

for (i in seq_len(nrow(files))) {
  drive_download(as_id(files$id[i]),
                 path = file.path("googledrive", files$name[i]),
                 overwrite = TRUE)
}

grid_fuels_2022 <- read.csv("googledrive/fuel.carbon.2022.subset.csv")
grid_fuels_2026 <- read.csv("googledrive/fuel.carbon.2026.csv")

#summarize biomass and carbon, select columns of interest
grid_fuels_2022 <- grid_fuels_2022 %>%
  mutate(
    biomass2022 = CWD.Biomass + Lit.Duff.Biomass + Weight1hour + Weight10hour + Weight100hour,
    carbon2022 = CWD.Carbon + Lit.Duff.Carbon + Carbon1hour + Carbon10hour + Carbon100hour
  ) %>%
  select(Plot, biomass2022, carbon2022)


grid_fuels_2026 <- grid_fuels_2026 %>%
  mutate(
    biomass2026 = CWD.Biomass + Litter.Biomass + Duff.Biomass + Weight1hour + Weight10hour + Weight100hour,
    carbon2026 = CWD.Carbon + Litter.Carbon + Duff.Carbon + Carbon1hour + Carbon10hour + Carbon100hour
  ) %>%
  select(Plot, biomass2026, carbon2026)

#merge 2022 and 2026 data, by just the plots re-read in 2022

grid_fuels <-left_join(grid_fuels_2022, grid_fuels_2026, by = "Plot")

grid_fuels <- grid_fuels %>%
  mutate(biomass_change = biomass2026-biomass2022, carbon_change = carbon2026-carbon2022)

grid_fuels <- left_join(grid_fuels, gridpoints, by = "Plot")

#----- Put together -----------
paired_fuels_sf <- paired_fuels %>%
  filter(!is.na(Longitude), !is.na(Latitude), !is.na(carbon_change)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


# Grid plots: IDs prefixed with "P"
grid_pts <- gridpoints %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(st_crs(subwatersheds_sf)) %>%
  inner_join(grid_fuels %>% select(Plot, carbon_change, carbon2022), by = "Plot") %>%
  filter(!is.na(carbon_change)) %>%
  transmute(plot_id = sprintf("P%03d", Plot),
            carbon_change,
            carbon_pre = carbon2022,                 # NEW: pre-fire carbon
            survey = "Grid plots (2022-2026)")

# Transects: IDs prefixed with "T"
trans_pts <- paired_fuels_sf %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(st_crs(subwatersheds_sf)) %>%
  transmute(plot_id = sprintf("T%03d", transect),
            carbon_change,
            carbon_pre = total_carbon_2025,                # NEW: check this column name
            survey = "Additional transects (2025-2026)") %>%
  filter(!is.na(carbon_change))

all_pts <- rbind(grid_pts, trans_pts) %>%
  filter(!is.na(carbon_change), !is.na(carbon_pre))



# ---- Settings ---------------------------------------------------------------
target_date <- as.Date("2025-07-15")   # <-- Targeting mid-growing season
window_days <- 10      # how far from target_date to look for a usable scene
max_cloud   <- 50      # loose scene-level filter (%); AOI-level check below (make sure its not too cloudy)
min_valid   <- 0.90    # require >= 90% of AOI pixels to be clear
out_dir     <- "teakettle_ndvi"
dir.create(out_dir, showWarnings = FALSE)

# ---- Area of interest -------------------------------------------------------
aoi <- st_read("googledrive/OUTLET_watershed.shp") |> st_transform(32611)
#aoi <- st_sfc(st_point(c(-119.03, 36.96)), crs = 4326) |>
 # st_transform(32611) |>
  # st_buffer(buffer_m) |>
 # st_sf()

 bb <- st_bbox(st_transform(aoi, 4326))

 # ---- Search for scenes near the target date ---------------------------------
 stac_src <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")
 
 items <- stac_src |>
   stac_search(
     collections = "sentinel-2-l2a",
     bbox        = c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]),
     datetime    = paste0(target_date - window_days, "/",
                          target_date + window_days),
     limit       = 100
   ) |>
   post_request() |>
   items_fetch() |>
   items_filter(properties$`eo:cloud_cover` < max_cloud) |>
   items_sign(sign_planetary_computer())
 
 stopifnot("No scenes found: widen window_days or max_cloud" =
             length(items$features) > 0)
 
 # Acquisition date of each scene, then candidate days sorted by closeness
 scene_dates <- as.Date(vapply(items$features,
                               function(f) substr(f$properties$datetime, 1, 10),
                               character(1)))
 cand_days <- unique(scene_dates)
 cand_days <- cand_days[order(abs(as.numeric(cand_days - target_date)))]
 message("Candidate dates: ", paste(cand_days, collapse = ", "))
 
 #------- NDVI for one scene -----------------
 read_asset <- function(item, band) {
   rast(paste0("/vsicurl/", item$assets[[band]]$href))
 }
 
 ndvi_one <- function(item) {
   b4  <- read_asset(item, "B04")
   b8  <- read_asset(item, "B08")
   scl <- read_asset(item, "SCL")
   
   aoi_v <- vect(st_transform(aoi, crs(b4)))
   b4  <- crop(b4,  aoi_v)            # errors if the tile does not overlap AOI
   b8  <- crop(b8,  aoi_v)
   scl <- resample(crop(scl, aoi_v), b4, method = "near")   # 20 m -> 10 m
   
   # Processing baseline 04.00 (after 2022-01-25) added a +1000 DN offset
   offset <- 1000
   red <- (b4 - offset) / 10000
   nir <- (b8 - offset) / 10000
   ndvi <- (nir - red) / (nir + red)
   
   # SCL classes kept: 2 dark/terrain shadow, 4 vegetation, 5 bare soil,
   # 6 water, 7 unclassified. Dropped: no data, saturated, cloud shadow,
   # clouds, cirrus, snow/ice.
   ndvi <- mask(ndvi, scl %in% c(2, 4, 5, 6, 7), maskvalues = FALSE)
   mask(ndvi, aoi_v)
 }
 
 #--- Pick the closest date with enough clear pixels-----------
 result <- NULL
 for (d in as.character(cand_days)) {
   its <- items$features[scene_dates == as.Date(d)]
   lyr <- Filter(Negate(is.null), lapply(its, function(it)
     tryCatch(ndvi_one(it), error = function(e) NULL)))
   if (length(lyr) == 0) next
   
   r <- if (length(lyr) == 1) lyr[[1]] else mosaic(sprc(lyr), fun = "mean")
   
   # Fraction of AOI pixels that are valid (aoi cells vs non-NA cells)
   aoi_v <- vect(st_transform(aoi, crs(r)))
   n_aoi   <- global(rasterize(aoi_v, r, field = 1), "sum", na.rm = TRUE)[1, 1]
   n_valid <- global(!is.na(r), "sum")[1, 1]
   frac <- n_valid / n_aoi
   message(d, ": ", round(100 * frac), "% clear pixels")
   
   if (frac >= min_valid) { result <- r; used_date <- d; break }
 }
 stopifnot("No date met min_valid: lower min_valid or widen window_days" =
             !is.null(result))
 
 names(result) <- paste0("NDVI_", used_date)
 message("Using ", used_date, " (", abs(as.numeric(as.Date(used_date) - target_date)),
         " days from target)")
 
 #--------Outputs -------------------
 tif <- file.path(out_dir, paste0("teakettle_ndvi_", used_date, ".tif"))
 writeRaster(result, tif, overwrite = TRUE)
 
 s <- global(result, c("mean", "min", "max", "sd"), na.rm = TRUE)
 print(round(s, 3))
 
 png(file.path(out_dir, paste0("teakettle_ndvi_", used_date, ".png")),
     width = 1200, height = 1000, res = 150)
 plot(result, range = c(0, 1), col = hcl.colors(50, "YlGn", rev = TRUE),
      main = paste("Teakettle NDVI,", used_date))
 plot(vect(st_transform(aoi, crs(result))), add = TRUE, border = "black", lwd = 1.5)
 dev.off()
 
 
 #-------------Plot distribution -----------------
 
 vals <- values(result, na.rm = TRUE)[, 1]
 vals <- vals[is.finite(vals)]
 
 library(ggplot2)
 ggplot(data.frame(ndvi = vals), aes(ndvi)) +
   geom_histogram(aes(y = after_stat(density)), bins = 60,
                  fill = "#9bc59d", color = "white") +
   geom_density(color = "#1b5e20", linewidth = 1) +
   geom_vline(xintercept = mean(vals), color = "firebrick", linewidth = 1) +
   geom_vline(xintercept = median(vals), color = "navy",
              linetype = "dashed", linewidth = 1) +
   labs(x = "NDVI", y = "Density",
        title = paste("Teakettle NDVI distribution,", used_date)) +
   theme_minimal(base_size = 13)
 
 
 
 #--------- Compare with Transect NDVI distribution -----------------
 
 ndvi <- rast("teakettle_ndvi/teakettle_ndvi_2025-07-14.tif")
 
 # 1. Make sure CRS matches the raster
 all_pts <- st_transform(all_pts, crs(ndvi))
 
 # 2. NDVI at transect points
 all_pts$ndvi <- terra::extract(ndvi, vect(all_pts))[, 2]
 
 # 2b. NDVI averaged within a 15 m buffer around each point
 all_pts$ndvi_buf <- terra::extract(ndvi, vect(st_buffer(all_pts, 30)),
                                    fun = mean, na.rm = TRUE)[, 2]
 
 pts_vals <- na.omit(all_pts$ndvi)
 
 # 3. NDVI across the whole raster (use a sample if the raster is huge)
 all_vals <- values(ndvi, na.rm = TRUE)[, 1]
 # all_vals <- spatSample(ndvi, 1e5, method = "random", na.rm = TRUE)[, 1]
 
 # 4. Summary stats
 summary(all_vals)
 summary(pts_vals)
 
 # 5. Plot
 df <- rbind(
   data.frame(ndvi = all_vals, group = "All pixels"),
   data.frame(ndvi = pts_vals, group = "Transect points")
 )
 
 ggplot(df, aes(ndvi, fill = group, color = group)) +
   geom_density(alpha = 0.3)
 
 ggplot(df, aes(ndvi, color = group)) + stat_ecdf()
 
 # 6. Tests
 ks.test(pts_vals, all_vals)        # do the distributions differ in shape?
 wilcox.test(pts_vals, all_vals)    # do they differ in location (median)?
 
 
 # df already built earlier with columns: ndvi, group
 
 meds <- aggregate(ndvi ~ group, df, median)
 meds$label <- paste0("Median = ", round(meds$ndvi, 3))
 
 bins <- seq(floor(min(df$ndvi) * 20) / 20,
             ceiling(max(df$ndvi) * 20) / 20,
             by = 0.05)
 
 ggplot(df, aes(ndvi, fill = group)) +
   geom_histogram(aes(y = after_stat(density)),
                  breaks = bins, color = "white", show.legend = FALSE) +
   facet_wrap(~ group, nrow = 1) +
   labs(x = "NDVI", y = "Density") +
   geom_text(data = meds, aes(x = ndvi, y = Inf, label = label),
             hjust = -0.1, vjust = 1.5, inherit.aes = FALSE) +
   theme_minimal()+
   # add to the plot:
   geom_vline(data = meds, aes(xintercept = ndvi),
                linetype = "dashed")
 
 #-------------- See what the carbon change/NVDI correlation is --------------------------
 
 dat <- all_pts %>%
   st_drop_geometry() %>%
   filter(!is.na(ndvi), !is.na(carbon_pre))
 
 fit <- lm(carbon_pre ~ ndvi_buf, data = dat)
 s   <- summary(fit)
 
 lab <- sprintf("R² = %.3f\nSlope = %.1f\np = %s\nn = %d",
                s$r.squared, coef(fit)[2],
                ifelse(s$coefficients[2, 4] < 0.001, "< 0.001",
                       sprintf("%.3f", s$coefficients[2, 4])),
                nrow(dat))
 
 ggplot(dat, aes(ndvi, carbon_pre)) +
   geom_point(alpha = 0.6, size = 2) +
   geom_smooth(method = "lm", se = TRUE, color = "firebrick",
               fill = "firebrick", alpha = 0.15) +
   annotate("text", x = -Inf, y = Inf, label = lab,
            hjust = -0.1, vjust = 1.2, size = 4) +
   labs(x = "NDVI (July 2025)", y = "Pre-fire carbon") +
   theme_minimal(base_size = 13)
 
 
 
 ggplot(dat, aes(ndvi_buf, carbon_pre, color = survey)) +
   geom_point(alpha = 0.6) +
   geom_smooth(method = "lm", se = FALSE) +
   theme_minimal(base_size = 13)
 
 dat %>%
   group_by(survey) %>%
   summarise(n = n(),
             r2 = summary(lm(carbon_pre ~ ndvi_buf))$r.squared,
             p  = summary(lm(carbon_pre ~ ndvi_buf))$coefficients[2, 4],
             spearman = cor(ndvi, carbon_pre, method = "spearman"))
 
 
 #### Clean up repo ####
 
 # 1. Free-floating CSVs in the working directory (not in subfolders)
 loose_csvs <- list.files(pattern = "\\.csv$", full.names = TRUE)
 loose_pngs <- list.files(pattern = "\\.png$", full.names = TRUE)
 loose_tifs <- list.files(pattern = "\\.tif$", full.names = TRUE)
 loose_gpx <- list.files(pattern = "\\.gpx$", full.names = TRUE)
 
 file.remove(loose_csvs)
 file.remove(loose_pngs)
 file.remove(loose_tifs)
 file.remove(loose_gpx)
 
 #clear other folders:
 
 unlink(list.files("googledrive", full.names = TRUE), recursive = TRUE)
 unlink("teakettle_ndvi", recursive = TRUE)
 
 