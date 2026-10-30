### Teakettle Burn Severity
library(sf)
library(tidyverse)
library(ggplot2)
library(googledrive)
library(dplyr)
library(viridis)
library(terra)
library(ggrepel)



#### Load data ####

#First authorize google drive
drive_auth(scopes = "https://www.googleapis.com/auth/drive")



###Next: Pull out all files from the nested subwatershed folders on google drive...ChatGPT wrote this...seems to work

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


#Alright now we want to bring in the RdNBR Composite Burn Index Raster
#I downloaded this data for the Garnet fire from this link: https://burnseverity.cr.usgs.gov/ravg/data-access
#Relativized differenced Normalized Burn Ratio (RdNBR) – a normalized version of the dNBR that removes the biasing effect of the pre-fire conditions (Miller et al 2009). The RdNBR is calculated as:
#RdNBR = dNBR / SquareRoot(ABS(NBR pre-fire / 1000))
#Composite Burn Index: A numerical, synoptic rating calculated from a field-based estimate of fire effects on individual strata within a plot or site in a burned area (Composite Burn Index | Burn Severity Portal). Estimates the overall impact to a site based on post-fire conditions averaged across the burnable portion of the site

#r<- rast("data/Garnet_RAVG_spatial_data/ca3686111904320250824_20241009_20251012_rdnbr_cbi.tif")
r <- drive_get("ca3686111904320250824_20241009_20251012_rdnbr_cbi.tif")

drive_download(r, path = "temp.tif", overwrite = TRUE)

r <- rast("temp.tif")  # raster

# Mask values (set to NA) These are due to clouds/smoke/fire etc.
r[r %in% c(-9, -9999)] <- NA

# Create classification matrix
# Columns: from, to, class value
#Based of value classifications from here: https://burnseverity.cr.usgs.gov/ravg/background-products-applications
#Slightly negative values are unburned areas with a slight increase in greenness post-fire
m <- matrix(c(
  -1,     0.1,   1,   # unchanged
  0.1,   1.25,  2,   # low severity
  1.25,  2.25,  3,   # moderate severity
  2.25,  3.0,   4    # high severity
), ncol = 3, byrow = TRUE)

# Apply classification
r_class <- classify(r, m)
  

#Now lets crop the raster to the Teakettle watershed outlet layer
#But first we have to make it compatible with terra
outlet_vect <- vect(outlet)
#and then project it to match the raster
outlet_vect <- project(outlet_vect, crs(r_class))

#alright now we crop the raster
r_crop <- crop(r_class, outlet_vect)


#Bring in the streams for mapping!

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


# Convert raster to data frame for ggplot
r_df <- as.data.frame(r_crop, xy = TRUE)
colnames(r_df)[3] <- "CBI_class"

# Map numeric classes to descriptive labels
r_df <- r_df %>%
  mutate(CBI_class = factor(CBI_class,
                            levels = 1:4,
                            labels = c("Unchanged", "Low", "Moderate", "High")))

# Define colors
cbi_colors <- c("Unchanged" = "#CDF2A7",
                "Low" = "#F0E590",
                "Moderate" = "#E89A41",
                "High" = "#C23123")



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


subwatersheds_sf <- st_transform(subwatersheds_sf, crs(r_crop))
subwatersheds_vect <- vect(subwatersheds_sf)

#order by area (smallest watersheds plot on top! So they are visible)
subwatersheds_sf <- subwatersheds_sf %>%
  mutate(area = st_area(.)) %>%
  arrange(desc(area))


#Make a nice plot.  I had labels for the subwatersheds but it looked a mess, even with the ggrepel.  Hard when they are nested
ggplot() +
  # Raster layer
  geom_raster(data = r_df, aes(x = x, y = y, fill = CBI_class)) +
  scale_fill_manual(values = cbi_colors) +
  
  # Watersheds polygons on top
  geom_sf(data = subwatersheds_sf,
          color = "black", alpha = 0, linewidth = 1) +
  geom_sf(data = streams_clipped, color = "#02779e")+
  
  #geom_text_repel(
   # data = st_coordinates(st_centroid(subwatersheds_sf)) %>%
    #  as.data.frame() %>%
     # cbind(watershed_name = subwatersheds_sf$watershed_name),
    #aes(X, Y, label = watershed_name),
    #size = 2
  #) +
  
  theme_minimal() +
  labs(title = "RdNBR Composite Burn Index with Subwatersheds",
       fill = "Composite Burn Index")


# Save the raster CBI map
ggsave(
  filename = "output/Composite_Burn_Index.png",
  plot = last_plot(),      # or assign your ggplot object to a variable
  width = 10,              # width in inches
  height = 8,              # height in inches
  dpi = 300
)

### Alright, what percent of each subwatershed burned at high severity?

# Make sure subwatersheds_sf is in the same CRS as raster
subwatersheds_sf <- st_transform(subwatersheds_sf, crs(r_crop))

# Convert to terra vector for extraction
subwatersheds_vect <- vect(subwatersheds_sf)

# Assign a unique ID for each polygon
subwatersheds_sf <- subwatersheds_sf %>%
  mutate(ID = row_number())

subwatersheds_vect <- vect(subwatersheds_sf)  # make sure the vector has the same IDs

# Extract raster values with coverage fraction (weights).  Allows us to weight cells that are only partially in the subwatershed lower
extracted <- terra::extract(r_crop, subwatersheds_vect, weights = TRUE)

# Rename the raster column to a simple name
colnames(extracted)[2] <- "CBI_class"  # second column is the raster value

# Compute weighted high-severity proportion
percent_high_df <- extracted %>%
  mutate(high_weighted = ifelse(CBI_class == 4, weight, 0)) %>%
  group_by(ID) %>%
  summarise(
    perc_high = sum(high_weighted, na.rm = TRUE) / sum(weight, na.rm = TRUE) * 100
  )

# Join back to the sf object by ID
subwatersheds_sf <- subwatersheds_sf %>%
  left_join(percent_high_df, by = "ID")

# Inspect
subwatersheds_sf %>%
  st_drop_geometry() %>%
  select(watershed_name, perc_high)

subwatersheds_sf <- subwatersheds_sf %>%
  rename(area_m2 = area, percent_high_severity = perc_high)

subwatersheds_table <- st_drop_geometry(subwatersheds_sf)


# Export to CSV
write.csv(subwatersheds_table, file = "output/subwatersheds_summary.csv", row.names = FALSE)

#Make a little histogram of this data
ggplot(subwatersheds_sf, aes(x = percent_high_severity)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  geom_vline(
    xintercept = median(subwatersheds_sf$percent_high_severity, na.rm = TRUE),
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    x = "Percent of watershed burned at high severity",
    y = "Watershed count"
  ) +
  theme_minimal()

ggsave(
  filename = "output/Burn_severity_histogram.png",
  plot = last_plot(),      # or assign your ggplot object to a variable
  width = 10,              # width in inches
  height = 8,              # height in inches
  dpi = 300
)


##Ok now color each watershed by percent burned at high severity:

ggplot() +
  geom_sf(data = subwatersheds_sf,
          aes(fill = percent_high_severity),
          color = "black",
          linewidth = 0.4, alpha = 1) +
  
  scale_fill_gradientn(
    colours = c("yellow", "orange", "red"),
    name = "% High Severity"
  ) +
  geom_sf(data = streams_clipped, color = "#02779e")+
  
  theme_minimal() +
  labs(title = "Percent of Subwatershed Burned at High Severity (CBI)")


# Save the raster CBI map
ggsave(
  filename = "output/Percent_Subwatershed_burned_HS_CBI.png",
  plot = last_plot(),      # or assign your ggplot object to a variable
  width = 10,              # width in inches
  height = 8,              # height in inches
  dpi = 300
)

#alright, now last step is to delete all these files before pushing to Github
if (dir.exists("gdrive_downloads")) {
  unlink("gdrive_downloads", recursive = TRUE, force = TRUE)
}
