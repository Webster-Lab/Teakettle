##==============================================================================
## Project: TEA
## Script to plot and compare the areas of subcatchments
##==============================================================================

##############
## Packages ##
##############

library(dplyr)
# remotes::install_github('r-tmap/tmap')
library(tmap)
library(sf)
# if using spatial points,
library(sp)
# for color palettes
library(viridis)
library(paletteer)
# for plotting with ggplot
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
#library(vapoRwave)
library(tidyverse)
library(tibble)
#google drive
library(googledrive) 
library(googlesheets4)

###############
#### Areas ####
###############

# Load areas
# List of folder names
folder_names <- c("OUTLET",
                  "TEAK01",
                  "TEAK02",
                  "TEAK03",
                  "TEAK04",
                  "TEAK05",
                  "TEAK06",
                  "TEAK07",
                  "TEAK08",
                  "TEAK09",
                  "TEAK10",
                  "TEAK11",
                  "TEAK12",
                  "TEAK13",
                  "TEAK14",
                  "TECR01",
                  "TECR02",
                  "TECR03",
                  "TECR04",
                  "TECR05",
                  'TECR06',
                  "TECR07",
                  "TECR08",
                  "TECR09",
                  "TECR10",
                  "TECR11",
                  'TECR12',
                  'TECR13')

# Create an empty list to store the shapefiles
areas_list <- list()

# Loop through each folder name in the list
for (folder in folder_names) {
  # Construct the file path
  folder_path <- paste0("data/data_geo/", folder, "/", folder, "_watershed.shp")
  
  # Check if the shapefile exists in the folder before loading
  if (file.exists(folder_path)) {
    # Store the shapefile in the list with a name corresponding to the folder
    areas_list[[folder]] <- st_read(folder_path)
  }
}

# Check the loaded shapefiles
print(areas_list)

for(i in folder_names){
  names(areas_list[[i]]) = c("area","geometry")
}

# Combine all areas into a single data frame
all_areas <- do.call(rbind, areas_list)

# Remove duplicate areas
#all_areas <- all_areas %>%
# distinct(Area_ID, .keep_all = TRUE)

#### Plot the combined areas ####
# Extract centroids
all_areas <- all_areas %>%
  mutate(Centroid = st_centroid(geometry)) %>%
  mutate(Latitude = st_coordinates(Centroid)[, 2],
         Longitude = st_coordinates(Centroid)[, 1])

# Calculate representative points
all_areas <- all_areas %>%
  mutate(Latitude = st_coordinates(Centroid)[, 2],
         Longitude = st_coordinates(Centroid)[, 1])

# Calculate area sizes and add a new column
all_areas$Size <- st_area(all_areas)

# Order the areas by size, from smallest to largest
all_areas <- all_areas[order(all_areas$Size, decreasing = TRUE), ]

# Row names into values
all_areas <- tibble::rownames_to_column(all_areas, "Area_ID")

### Plot the combined areas with smaller areas first ###
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Teakettle Watersheds", fill = "Area ID") +
  geom_text(data = all_areas, aes(x = Longitude, y = Latitude, 
                                  label = Area_ID), color = "black", size = 3) +
  theme_minimal()

ggplot() +
  geom_sf(data = all_areas[all_areas$Area_ID %in% c("TEAK01","TECR01"),], aes(fill = Area_ID), alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Teakettle Watersheds", fill = "Area ID") +
  #geom_text(data = all_areas, aes(x = Longitude, y = Latitude, 
                                 # label = Area_ID), color = "black", size = 3) +
  theme_minimal()

hist(all_areas$Size/1000000, breaks=20)
hist(all_areas$Size[2:34]/1000000, breaks=20) # without outlet

all_areas
