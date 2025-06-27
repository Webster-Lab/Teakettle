##==============================================================================
## Project: TEA
## Script to calculate subcatchment area using a digital elevation model
##==============================================================================

##############
## PACKAGES ##
##############
library(tidyverse)
library(dplyr)
library(raster)
library(sf)
#if using spatial points,
library(sp)
library(elevatr)
library(mapview)
library(stars)
library("vroom")
#whitebox::install_whitebox()
library(whitebox)
library(tmaptools)
library(googledrive)

###############
## Load data ##
###############
#this is a csv file with at least the site name and lat long info for that site
June2025 = read_csv("data/spatial_June2025/Teakettle_Synoptics.csv")

names(June2025) = c("Lon","Lat","Z", "siteID","Elev_ft",
                    "LatLon","Notes","TimeCreated") 
June2025_r = June2025[,c(4,1,2)]

# adjustments to snap to correct locations (hopefully) and remove "extra" sites for now
June2025_r = June2025_r %>% filter(siteID !="TECR-extra")
## TEAK
# TEAK01
June2025_r$Lat[June2025_r$siteID=="TEAK01"] = 36.96097
June2025_r$Lon[June2025_r$siteID=="TEAK01"] = -119.02789
# TEAK03
June2025_r$Lat[June2025_r$siteID=="TEAK03"] = 36.96196
June2025_r$Lon[June2025_r$siteID=="TEAK03"] = -119.02963
# TEAK05
June2025_r$Lat[June2025_r$siteID=="TEAK05"] = 36.96444
June2025_r$Lon[June2025_r$siteID=="TEAK05"] = -119.03655
# TEAK06
June2025_r$Lat[June2025_r$siteID=="TEAK06"] = 36.96440
June2025_r$Lon[June2025_r$siteID=="TEAK06"] = -119.03538
# TEAK07
June2025_r$Lat[June2025_r$siteID=="TEAK07"] = 36.96671
June2025_r$Lon[June2025_r$siteID=="TEAK07"] = -119.03875
# TEAK10
June2025_r$Lat[June2025_r$siteID=="TEAK10"] = 36.96952
June2025_r$Lon[June2025_r$siteID=="TEAK10"] = -119.04138
# TEAK14
June2025_r$Lat[June2025_r$siteID=="TEAK14"] = 36.97699
June2025_r$Lon[June2025_r$siteID=="TEAK14"] = -119.04828
## TECR
# TECR01
June2025_r$Lat[June2025_r$siteID=="TECR01"] = 36.95583
June2025_r$Lon[June2025_r$siteID=="TECR01"] = -119.02641
# TECR03
June2025_r$Lat[June2025_r$siteID=="TECR03"] = 36.95618
June2025_r$Lon[June2025_r$siteID=="TECR03"] = -119.03287
# TECR05 
June2025_r$Lat[June2025_r$siteID=="TECR05"] = 36.95638
June2025_r$Lon[June2025_r$siteID=="TECR05"] = -119.03779
# TECR06 
June2025_r$Lat[June2025_r$siteID=="TECR06"] = 36.95591
June2025_r$Lon[June2025_r$siteID=="TECR06"] = -119.03926
# TECR07
June2025_r$Lat[June2025_r$siteID=="TECR07"] = 36.95633
June2025_r$Lon[June2025_r$siteID=="TECR07"] = -119.03875
# TECR12
June2025_r$Lat[June2025_r$siteID=="TECR12"] = 36.95725
June2025_r$Lon[June2025_r$siteID=="TECR12"] = -119.04305


#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
June2025_sf = st_as_sf(June2025_r, coords = c("Lon", "Lat"), 
                  crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
June2025_sf = st_transform(June2025_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()

###################################
## Clear folders that we will use ##
###################################
# List and delete all files in the folder
files <- list.files(path = "data/data_geo", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)

################
## PULL A DEM ##
################
### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(June2025_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 2000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again ####
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_tea.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)

############################
## PREP DEM AND DELINEATE ##
############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and outlet to temp
#1.2 fill cell pits
#1.3 breach depressions
#1.4 write flow direction raster
#1.5.2 write flow accumulation raster
#1.5.2 write stream layer
#1.6 snap pour point
#1.7 delineate
#1.8 read back into main
#1.9 convert to polygons

#whitebox functions do not work just using project directory, so you have to set the working directory
#we are using all the files that are stored in the temp directory, so:
getwd()
#copy and paste that directory if that's where you are working from and make it a temp
temp <- "Users/awebster2/Library/CloudStorage/Dropbox/Teakettle/Project Phase/R/TEA"

# These next lines are pre-processing steps in digital elevation model (DEM) - they are creating intermediate files
#1.1 -----
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_tea.tif"), overwrite = T)
st_write(June2025_sf, paste0("data/temp/synoptics_tea.shp"), delete_layer = T)

#1.2 -----
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_tea.tif",
  output = "data/temp/dem_tea_fill.tif",
  wd = temp)

#1.3 -----
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_tea_fill.tif",
  output = "data/temp/dem_tea_fill_breach.tif",
  wd = temp)


#1.4 -----
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_tea_fill_breach.tif",
  output = "data/temp/flowdir_tea.tif",
  wd = temp)


#1.5 -----
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_tea_fill_breach.tif",
  output = "data/temp/flowaccum_tea.tif",
  wd = temp
)

#1.6 -----
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_tea.shp",
  flow_accum = "data/temp/flowaccum_tea.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_tea.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_tea.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_tea.tif",
  output = "data/temp/streams_tea.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_tea.tif",
  d8_pntr = "data/temp/flowdir_tea.tif",
  output = "data/temp/streams_tea.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_tea.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_tea.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7 -----
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_tea.tif",
  pour_pts = "data/temp/snap_pour_tea.shp",
  output = "data/temp/shed_tea.tif",
  wd = temp
)

#1.8 -----
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
tea_ws <- raster(paste0("data/temp/shed_tea.tif"))

mapview(tea_ws, maxpixels =  2970452)

#1.9 -----
#converts newmex_ws into a stars object, it is a multi-dimensional array that represents raster data.
tea_ws <- st_as_stars(tea_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(tea_ws)
#writes shapefile to data folder
st_write(tea_ws, paste0("data/data_geo/tea_watersheds.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(tea_ws) + mapview(dem) + mapview(pour_sf)

################################
## Crop the DEM and run again ##
################################
#read the shape file defining the extent to crop the DEM
crop_extent <- st_read("data/data_geo/tea_watersheds.shp")
#crop the DEM to the specified extent
cropped_DEM <- raster::crop(dem, crop_extent)

#plotting
plot(cropped_DEM)
plot(tea_ws, add = TRUE)

#Run the entire WBT series again to try making streams off of the smaller DEM
writeRaster(cropped_DEM, paste0("data/temp/cropped_dem_tea.tif"), overwrite=T)
#didn't change
wbt_fill_single_cell_pits(
  dem = "data/temp/cropped_dem_tea.tif",
  output = "data/temp/cropped_dem_tea_fill.tif",
  wd = temp)

wbt_breach_depressions(
  dem = "data/temp/cropped_dem_tea_fill.tif",
  output = "data/temp/cropped_dem_tea_breach.tif",
  wd = temp)

wbt_d8_pointer(
  dem = "data/temp/cropped_dem_tea_breach.tif",
  output = "data/temp/cropped_flowdir_tea.tif",
  wd = temp)

wbt_d8_flow_accumulation(
  input = "data/temp/cropped_dem_tea_breach.tif",
  output = "data/temp/cropped_flowaccum_tea.tif",
  wd = temp
)

#### Rerun with cropped streams ####
#read stream raster
streams <- raster(paste0("data/temp/cropped_flowaccum_tea.tif")) #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/cropped_flowaccum_tea.tif",
  output = "data/temp/cropped_streams_tea.tif",
  threshold = 20,
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/cropped_streams_tea.tif",
  d8_pntr = "data/temp/cropped_flowdir_tea.tif",
  output = "data/temp/cropped_streams_tea.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/cropped_streams_tea.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(cropped_DEM)
#crop to the watershed
streams <- streams[tea_ws,]
plot(streams)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + mapview(tea_ws) + 
  mapview(streams) + mapview(pour_sf) 
# a few are off bust since the watersheds are already delinated, I am not going to edit them again

#export all of these so we have them!
st_write(tea_ws, paste0("data/data_geo/area.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/area_stream_network.shp"), delete_layer = T)
writeRaster(cropped_DEM, paste0("data/data_geo/croppedDEM_area.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
sum(st_area(tea_ws))

#Check area is ok with flowdir
flowdir = raster('data/temp/flowdir_tea.tif')
plot(flowdir) + plot(streams)
mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(tea_ws)



