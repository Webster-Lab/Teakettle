##==============================================================================
## Project: TEA
## Script to plot and compare the areas of subcatchments
## Author: Alex Webster
## Last update: Nov 20, 2025
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

###################
#### Load Data ####
###################

# Below loades data from Google Drive that was written to Google Drive in the 01_deliniation.R script

#folder link to id
jp_folder = "https://drive.google.com/drive/folders/10oUXtZCdOF1khwH2SD5KxUL0EVSl0GZv"
folder_id = drive_get(as_id(jp_folder))
2

#find files in folder
files = drive_ls(folder_id)

#loop dirs and download ONLY watershed shp (and associated) files inside them
# this avoides downloading the stream network and DEM
for (i in seq_along(files$name)) {
  #list files
  i_dir = drive_ls(files[i, ])
  i_dir = i_dir[1:4,] 
  #you can modify which files are downloaded here, or see below for how to download them all (which takes a long time)
  #note that shapefiles need associated .dbf, .prj, and .shx files to work
  
  #mkdir
  dir.create(file.path("data", "data_geo", files$name[i]))
  
  #download files
  for (file_i in seq_along(i_dir$name)) {
    #fails if already exists
    try({
      drive_download(
        as_id(i_dir$id[file_i]),
        path = str_c("data/data_geo/",files$name[i], "/", i_dir$name[file_i]),
        overwrite = TRUE
      )
    })
  }
}

# #loop dirs and download all files inside them
# for (i in seq_along(files$name)) {
#   #list files
#   i_dir = drive_ls(files[i, ])
#   
#   #mkdir
#   dir.create(file.path("data", "data_geo", files$name[i]))
#   
#   #download files
#   for (file_i in seq_along(i_dir$name)) {
#     #fails if already exists
#     try({
#       drive_download(
#         as_id(i_dir$id[file_i]),
#         path = str_c("data/data_geo/",files$name[i], "/", i_dir$name[file_i]),
#         overwrite = TRUE
#       )
#     })
#   }
# }

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

for(i in folder_names){
  names(areas_list[[i]]) = c("area","geometry")
}

# Check the loaded shapefiles
print(areas_list)
plot(areas_list[["OUTLET"]])

# for some reason, some of these are saving multiple shapefiles into the list:
plot(areas_list[["TEAK10"]][[2]][[1]])
plot(areas_list[["TEAK10"]][[2]][[2]])
plot(areas_list[["TEAK12"]][[2]][[1]])
plot(areas_list[["TEAK12"]][[2]][[2]])
plot(areas_list[["TEAK12"]][[2]][[3]])
plot(areas_list[["TECR06"]][[2]][[3]])
# in every case, the first shapefile appears to be the main subwatershed, and the others are tiny. I suspect something about the delineation caused fragmentation, but I'm surprised since it is only one shapefile. The areas are small enough that I hope it is insignificant to just delete them...

# Combine all areas into a single data frame
all_areas <- do.call(rbind, areas_list)
# delete tiny areas and geometries:
all_areas = all_areas[ !rownames(all_areas) %in% c("TEAK10.2","TEAK12.2","TEAK12.3","TECR06.2","TECR06.3","TECR07.2"),]
# rename siteIDs to get rid of duplicate nomenclature:
rownames(all_areas) = substr(rownames(all_areas), start = 1, stop = 6)

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
  theme_minimal()

ggplot() +
  geom_sf(data = all_areas[all_areas$Area_ID %in% c("TEAK04"),], aes(fill = Area_ID), alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Teakettle Watersheds", fill = "Area ID") +
  theme_minimal()
# this appears to have deliniated poorly because, while it is a trib, it shouldn't be this tiny. I will go back to the deliniation script to fix. 

hist(all_areas$Size/1000000, breaks=20)
hist(all_areas$Size[2:34]/1000000, breaks=20) # without outlet

all_areas


#### Save area data ####

all_areas_r = all_areas[,c(1,7)]
units(all_areas_r$Size)=NULL
all_areas_r$Size_km2 = all_areas_r$Size/1000000

all_areas_r = st_drop_geometry(all_areas_r)
all_areas_r = all_areas_r[,-2]

write.csv(all_areas_r, "data/script_output/02_subcatareas/subcatareas_all.csv", row.names = FALSE)

#### Clear spatial data from folders before pushing changes to GitHub ####


