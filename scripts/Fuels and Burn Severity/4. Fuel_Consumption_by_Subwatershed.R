#Additional Brown's Lines (Webster Lab) Calculations
#Author: Carolina May
#Last Updated: 2026-10-28


### Libraries ####


library(dplyr)
library(tidyverse)
library(ggplot2)
library(googledrive)
library(sf)


### Data Download and Merging ####

#Download  fuel data from Google Drive
drive_auth()


#Identify additional fuels outputs folder
folder <- as_id("1kAl5ASxI4XlM5YA3sXrcRfdk0joRFkYC")
files <- drive_ls(folder)
file <- files[files$name == "add.paired.fuel.diff.csv", ]

drive_download(file, 
               path = "add.paired.fuel.diff.csv", 
               overwrite = TRUE)

paired_fuels <- read.csv("add.paired.fuel.diff.csv")



#Download and read-in Spatial Data

#First lets get the csv with fuel transect GPS coordinates

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

# Get Teakettle Gripoint Carbon/Biomass data for 2022 and 2026
## Note: In 2022 half the plots were re-read using better guidelines for litter/duff/rotting log estimates.  I am using this subset of plots rather than the full plot network for the best data.

folder <- as_id("1AATNCB0taGl6pY7CG60nvgs0pM00k5Dz")
files  <- drive_ls(folder)

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


  
  
### Next, read in all subwatershed spatial data

### Pull out all files from the nested subwatershed folders on google drive

#This is the master folder for subwatershed delination
folder_link <- "https://drive.google.com/drive/folders/10oUXtZCdOF1khwH2SD5KxUL0EVSl0GZv"

load_drive_shapefiles_debug <- function(folder_link, download_dir = "gdrive_downloads", parent_path = NULL) {
  
  dir.create(download_dir, showWarnings = FALSE)
  folder_id <- as_id(folder_link)
  
  # List all items
  all_items <- tryCatch(
    drive_ls(
      folder_id,
      corpora = "allDrives",
      include_items_from_all_drives = TRUE
    ),
    error = function(e) {
      message("⚠ Could not list folder: ", folder_link)
      return(data.frame())  # empty df
    }
  )
  
  layers <- list()
  
  # Separate subfolders and files safely
  if (nrow(all_items) == 0) {
    message("⚠ Folder is empty or inaccessible: ", folder_link)
    return(layers)
  }
  
  # Access mimeType safely
  mime_types <- sapply(all_items$drive_resource, function(x) x$mimeType)
  
  subfolders <- all_items[mime_types == "application/vnd.google-apps.folder", ]
  files <- all_items[mime_types != "application/vnd.google-apps.folder", ]
  
  folder_label <- if (!is.null(parent_path)) parent_path else all_items$name[1]
  message("Checking folder: ", folder_label,
          " | files: ", nrow(files),
          " | subfolders: ", nrow(subfolders))
  
  # Process shapefiles
  shp_bases <- unique(tools::file_path_sans_ext(files$name[grepl("\\.shp$", files$name)]))
  
  if (length(shp_bases) == 0) {
    message("⚠ No shapefiles found in folder: ", folder_label)
  }
  
  for (base in shp_bases) {
    components <- files[grepl(paste0("^", base, "\\."), files$name), ]
    
    for (i in seq_len(nrow(components))) {
      drive_download(
        file = as_id(components$id[i]),
        path = file.path(download_dir, components$name[i]),
        overwrite = TRUE
      )
    }
    
    layer_name <- if (!is.null(parent_path)) paste(parent_path, base, sep = "/") else base
    layers[[layer_name]] <- st_read(file.path(download_dir, paste0(base, ".shp")), quiet = TRUE)
  }
  
  # Recurse into subfolders
  for (i in seq_len(nrow(subfolders))) {
    subfolder_name <- subfolders$name[i]
    subfolder_id <- subfolders$id[i]
    
    sub_layers <- load_drive_shapefiles_debug(
      folder_link = subfolder_id,
      download_dir = download_dir,
      parent_path = if (!is.null(parent_path)) paste(parent_path, subfolder_name, sep = "/") else subfolder_name
    )
    
    layers <- c(layers, sub_layers)
  }
  
  return(layers)
}
all_layers <- load_drive_shapefiles_debug(folder_link)

#check they're all there
names(all_layers)

#load in and name these layers
outlet<- st_read("gdrive_downloads/OUTLET_watershed.shp")
TEAK01<- st_read("gdrive_downloads/TEAK01_watershed.shp")
TEAK02<- st_read("gdrive_downloads/TEAK02_watershed.shp")
TEAK03<- st_read("gdrive_downloads/TEAK03_watershed.shp")
TEAK04<- st_read("gdrive_downloads/TEAK04_watershed.shp")
TEAK05<- st_read("gdrive_downloads/TEAK05_watershed.shp")
TEAK06<- st_read("gdrive_downloads/TEAK06_watershed.shp")
TEAK07<- st_read("gdrive_downloads/TEAK07_watershed.shp")
TEAK08<- st_read("gdrive_downloads/TEAK08_watershed.shp")
TEAK09<- st_read("gdrive_downloads/TEAK09_watershed.shp")
TEAK10<- st_read("gdrive_downloads/TEAK10_watershed.shp")
TEAK11<- st_read("gdrive_downloads/TEAK11_watershed.shp")
TEAK12<- st_read("gdrive_downloads/TEAK12_watershed.shp")
TEAK13<- st_read("gdrive_downloads/TEAK13_watershed.shp")
TEAK14<- st_read("gdrive_downloads/TEAK14_watershed.shp")
TECR01<- st_read("gdrive_downloads/TECR01_watershed.shp")
TECR02<- st_read("gdrive_downloads/TECR02_watershed.shp")
TECR03<- st_read("gdrive_downloads/TECR03_watershed.shp")
TECR04<- st_read("gdrive_downloads/TECR04_watershed.shp")
TECR05<- st_read("gdrive_downloads/TECR05_watershed.shp")
TECR06<- st_read("gdrive_downloads/TECR06_watershed.shp")
TECR07<- st_read("gdrive_downloads/TECR07_watershed.shp")
TECR08<- st_read("gdrive_downloads/TECR08_watershed.shp")
TECR09<- st_read("gdrive_downloads/TECR09_watershed.shp")
TECR10<- st_read("gdrive_downloads/TECR10_watershed.shp")
TECR11<- st_read("gdrive_downloads/TECR11_watershed.shp")
TECR12<- st_read("gdrive_downloads/TECR12_watershed.shp")
TECR12B<- st_read("gdrive_downloads/TECR12B_watershed.shp")
TECR13<- st_read("gdrive_downloads/TECR13_watershed.shp")

#make a list of all the subwatersheds and bind into one sf object
# Create a named list
subwatersheds_list <- list(
  outlet = outlet,
  TEAK01 = TEAK01,
  TEAK02 = TEAK02,
  TEAK03 = TEAK03,
  TEAK04 = TEAK04,
  TEAK05 = TEAK05,
  TEAK06 = TEAK06,
  TEAK07 = TEAK07,
  TEAK08 = TEAK08,
  TEAK09 = TEAK09,
  TEAK10 = TEAK10,
  TEAK11 = TEAK11,
  TEAK12 = TEAK12,
  TEAK13 = TEAK13,
  TEAK14 = TEAK14,
  TECR01 = TECR01,
  TECR02 = TECR02,
  TECR03 = TECR03,
  TECR04 = TECR04,
  TECR05 = TECR05,
  TECR06 = TECR06,
  TECR07 = TECR07,
  TECR08 = TECR08,
  TECR09 = TECR09,
  TECR10 = TECR10,
  TECR11 = TECR11,
  TECR12 = TECR12,
  TECR12B = TECR12B,
  TECR13 = TECR13
)

# Bind and keep names
subwatersheds_sf <- bind_rows(subwatersheds_list, .id = "watershed_name")

#Next merge all polygons together based on watershed name (a few had multiple parts for some reason.  I investigated in QGIS and seems the shapefile was just broken up into pieces)
subwatersheds_sf <- subwatersheds_sf %>%
  group_by(watershed_name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

#order by area (smallest watersheds plot on top! So they are visible)
subwatersheds_sf <- subwatersheds_sf %>%
  mutate(area = st_area(.)) %>%
  arrange(desc(area))

# Get stream shapefiles for mapping

#set google drive folder ID
folder_id <- "1dDzLAm_ucSO6EAnGyB18iTL5eOdaT5mV"

#find and download all files to the gdrive_downloads folder
files <- drive_find(
  q = paste0("'", folder_id, "' in parents")
)

for (i in seq_len(nrow(files))) {
  drive_download(
    file = files[i, ],
    path = file.path("gdrive_downloads", files$name[i]),
    overwrite = TRUE
  )
}
#read in streams shapefile
streams<- st_read("gdrive_downloads/OUTLET_stream_network.shp")

#match CRS and clip the outlet watershed
streams <- st_transform(streams, st_crs(outlet))
streams_clipped <- st_intersection(streams, outlet)


#### Mapping ####

# 1. Turn paired_fuels into spatial points
# Replace "Longitude"/"Latitude" with whatever the coordinate columns in
# Teakettle_Fuels.csv are actually called (check names(paired_fuels))
# Use crs = 4326 (WGS84 lat/long)
paired_fuels_sf <- paired_fuels %>%
  filter(!is.na(Longitude), !is.na(Latitude), !is.na(biomass_change)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


# 2. Put all layers in the same CRS (use the watersheds' CRS)
paired_fuels_sf <- st_transform(paired_fuels_sf, st_crs(subwatersheds_sf))
gridpoints      <- st_transform(gridpoints, st_crs(subwatersheds_sf))


#get fuel consumption quantiles
qs <- quantile(paired_fuels_sf$biomass_change, probs = seq(0, 1, 0.25), na.rm = TRUE)

# 3. Map
p <- ggplot() +
  geom_sf(data = subwatersheds_sf,
          fill = "grey95", color = "grey40", linewidth = 0.4) +
  geom_sf(data =streams_clipped, color = "lightblue3", linewidth = 0.2) +
  geom_sf(data = paired_fuels_sf,
          aes(color = biomass_change),
          size = 2) +
  geom_sf(data = gridpoints, color = "grey20", shape = 1, size = 1) + 
  
  scale_color_gradientn(colors = c("red", "orange", "yellow", "green4"),
                        values = scales::rescale(qs),
                        name = "Biomass change (Mg/ha)") +
  theme_minimal() +
  labs(title = "Change in fuel biomass by transect")

ggsave("biomass_change_map.png", plot = p,
       width = 8, height = 8, dpi = 300, bg = "white")

# 2. Upload to the Drive folder
map_folder <- drive_get(as_id("1Wz4wbdnr1I7UyPM5elQTy9NjdFajzHmV"))

drive_put(media = "biomass_change_map.png",
          path = map_folder,
          name = "biomass_change_map.png")



grid_pts <- grid_fuels_sf %>%
  transmute(id = as.character(Plot), biomass_change, survey = "Grid plots (2022-2026)")

trans_pts <- paired_fuels_sf %>%
  st_transform(st_crs(subwatersheds_sf)) %>%
  transmute(id = as.character(transect), biomass_change, survey = "Additional transects (2025-2026)")

all_pts <- rbind(grid_pts, trans_pts) %>%
  filter(!is.na(biomass_change)) %>%
  arrange(abs(biomass_change))

lims <- quantile(all_pts$biomass_change, c(0.05, 0.95), na.rm = TRUE)

# 3. Map
p_map <- ggplot() +
  geom_sf(data = subwatersheds_sf, fill = "grey95", color = "grey40", linewidth = 0.4) +
  geom_sf(data = streams_clipped, color = "lightblue3", linewidth = 0.2) +
  geom_sf(data = all_pts, aes(color = biomass_change, shape = survey), size = 2.5) +
  scale_color_gradient(low = "red", high = "green3",
                       limits = lims, oob = scales::squish,
                       name = "Biomass change (Mg/ha)") +
  scale_shape_manual(values = c("Grid plots (2022-2026)" = 16,
                                "Additional transects (2025-2026)" = 17),
                     name = "Survey") +
  theme_minimal() +
  labs(title = "Change in fuel biomass")

p_map
