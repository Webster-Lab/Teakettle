##==============================================================================
## Project: TEA
## Script to calculate subcatchment area using a digital elevation model
## This script delineates each subcatchment separately and writes the resulting spatial files to Google Drive.
## NOTE that if you create new deliniations of previously-saved delinations, you need to delete the old fies on Google Drive because they do not get overwritten with the current code (ideally, someone will add code to overwrite in future!).
## Author: Alex Webster
## Last update: Nov 20, 2025
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

# The below adjustments snap subcatchment outlets to correct locations (hopefully) and remove "extra" sites
# Adjustments were made by selecting alternative waypoints in Gaia GPS + trial and error that things snapped correctly
June2025_r = June2025_r %>% filter(siteID !="TECR-extra")
## TEAK
# TEAK01
June2025_r$Lat[June2025_r$siteID=="TEAK01"] = 36.96097
June2025_r$Lon[June2025_r$siteID=="TEAK01"] = -119.02789
# TEAK03
June2025_r$Lat[June2025_r$siteID=="TEAK03"] = 36.96196
June2025_r$Lon[June2025_r$siteID=="TEAK03"] = -119.02963
# TEAK04
June2025_r$Lat[June2025_r$siteID=="TEAK04"] = 36.96144
June2025_r$Lon[June2025_r$siteID=="TEAK04"] = -119.03320
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


###############
## Make folders in directory ##
###############

newfolders <- c("OUTLET",
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
for (i in newfolders){
  dir.create(file.path("data", "data_geo", i), recursive = TRUE)
}


##############
## OUTLET ##
##############

# isolate lat lon
OUTLET = June2025_r[June2025_r$siteID=="OUTLET",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
OUTLET_sf = st_as_sf(OUTLET, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
OUTLET_sf = st_transform(OUTLET_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/OUTLET/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(OUTLET_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_OUTLET.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and OUTLET to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_OUTLET.tif"), overwrite = T)
st_write(OUTLET_sf, paste0("data/temp/synoptics_OUTLET.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_OUTLET.tif",
  output = "data/temp/dem_OUTLET_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_OUTLET_fill.tif",
  output = "data/temp/dem_OUTLET_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_OUTLET_fill_breach.tif",
  output = "data/temp/flowdir_OUTLET.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_OUTLET_fill_breach.tif",
  output = "data/temp/flowaccum_OUTLET.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_OUTLET.shp",
  flow_accum = "data/temp/flowaccum_OUTLET.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_OUTLET.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_OUTLET.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_OUTLET.tif",
  output = "data/temp/streams_OUTLET.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_OUTLET.tif",
  d8_pntr = "data/temp/flowdir_OUTLET.tif",
  output = "data/temp/streams_OUTLET.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_OUTLET.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_OUTLET.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_OUTLET.tif",
  pour_pts = "data/temp/snap_pour_OUTLET.shp",
  output = "data/temp/shed_OUTLET.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
OUTLET_ws <- raster(paste0("data/temp/shed_OUTLET.tif"))

mapview(OUTLET_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
OUTLET_ws <- st_as_stars(OUTLET_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(OUTLET_ws)
#writes shapefile to data folder
#st_write(OUTLET_ws, paste0("data/data_geo/OUTLET/OUTLET_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(OUTLET_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(OUTLET_ws, paste0("data/data_geo/OUTLET/OUTLET_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/OUTLET/OUTLET_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/OUTLET/OUTLET_DEM.tif"), overwrite=T)

#CHECK THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
# this is a gut check
(sum(st_area(OUTLET_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_OUTLET.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(OUTLET_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1dDzLAm_ucSO6EAnGyB18iTL5eOdaT5mV")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/OUTLET/OUTLET_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/OUTLET/OUTLET_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/OUTLET/OUTLET_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/OUTLET/OUTLET_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/OUTLET/OUTLET_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/OUTLET/OUTLET_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/OUTLET/OUTLET_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/OUTLET/OUTLET_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/OUTLET/OUTLET_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/OUTLET//", full.names = TRUE)
file.remove(files)



##############
## TEAK01 ##
##############

# isolate lat lon
TEAK01 = June2025_r[June2025_r$siteID=="TEAK01",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK01_sf = st_as_sf(TEAK01, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK01_sf = st_transform(TEAK01_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK01/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK01_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK01.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK01 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK01.tif"), overwrite = T)
st_write(TEAK01_sf, paste0("data/temp/synoptics_TEAK01.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK01.tif",
  output = "data/temp/dem_TEAK01_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK01_fill.tif",
  output = "data/temp/dem_TEAK01_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK01_fill_breach.tif",
  output = "data/temp/flowdir_TEAK01.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK01_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK01.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK01.shp",
  flow_accum = "data/temp/flowaccum_TEAK01.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK01.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK01.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK01.tif",
  output = "data/temp/streams_TEAK01.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK01.tif",
  d8_pntr = "data/temp/flowdir_TEAK01.tif",
  output = "data/temp/streams_TEAK01.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK01.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK01.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK01.tif",
  pour_pts = "data/temp/snap_pour_TEAK01.shp",
  output = "data/temp/shed_TEAK01.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK01_ws <- raster(paste0("data/temp/shed_TEAK01.tif"))

mapview(TEAK01_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK01_ws <- st_as_stars(TEAK01_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK01_ws)
#writes shapefile to data folder
#st_write(TEAK01_ws, paste0("data/data_geo/TEAK01/TEAK01_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK01_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK01_ws, paste0("data/data_geo/TEAK01/TEAK01_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK01/TEAK01_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK01/TEAK01_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK01_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK01.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK01_ws)

## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1l5XJo90giXThufbVZx3DTM3TtfyPv0a7")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK01/TEAK01_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK01/TEAK01_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK01/TEAK01_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK01/TEAK01_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK01/TEAK01_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK01/TEAK01_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK01/TEAK01_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK01/TEAK01_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK01/TEAK01_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK01/", full.names = TRUE)
file.remove(files)



##############
## TEAK02 ##
##############

# isolate lat lon
TEAK02 = June2025_r[June2025_r$siteID=="TEAK02",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK02_sf = st_as_sf(TEAK02, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK02_sf = st_transform(TEAK02_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK02/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK02_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK02.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK02 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK02.tif"), overwrite = T)
st_write(TEAK02_sf, paste0("data/temp/synoptics_TEAK02.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK02.tif",
  output = "data/temp/dem_TEAK02_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK02_fill.tif",
  output = "data/temp/dem_TEAK02_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK02_fill_breach.tif",
  output = "data/temp/flowdir_TEAK02.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK02_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK02.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK02.shp",
  flow_accum = "data/temp/flowaccum_TEAK02.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK02.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK02.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK02.tif",
  output = "data/temp/streams_TEAK02.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK02.tif",
  d8_pntr = "data/temp/flowdir_TEAK02.tif",
  output = "data/temp/streams_TEAK02.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK02.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK02.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK02.tif",
  pour_pts = "data/temp/snap_pour_TEAK02.shp",
  output = "data/temp/shed_TEAK02.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK02_ws <- raster(paste0("data/temp/shed_TEAK02.tif"))

mapview(TEAK02_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK02_ws <- st_as_stars(TEAK02_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK02_ws)
#writes shapefile to data folder
#st_write(TEAK02_ws, paste0("data/data_geo/TEAK02/TEAK02_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK02_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK02_ws, paste0("data/data_geo/TEAK02/TEAK02_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK02/TEAK02_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK02/TEAK02_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK02_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK02.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK02_ws)

## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1j_pdhnprKrySokBBnGg3CTKaFeor6Zj1")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK02/TEAK02_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK02/TEAK02_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK02/TEAK02_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK02/TEAK02_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK02/TEAK02_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK02/TEAK02_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK02/TEAK02_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK02/TEAK02_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK02/TEAK02_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK02/", full.names = TRUE)
file.remove(files)


##############
## TEAK03 ##
##############

# isolate lat lon
TEAK03 = June2025_r[June2025_r$siteID=="TEAK03",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK03_sf = st_as_sf(TEAK03, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK03_sf = st_transform(TEAK03_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK03/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK03_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK03.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK03 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK03.tif"), overwrite = T)
st_write(TEAK03_sf, paste0("data/temp/synoptics_TEAK03.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK03.tif",
  output = "data/temp/dem_TEAK03_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK03_fill.tif",
  output = "data/temp/dem_TEAK03_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK03_fill_breach.tif",
  output = "data/temp/flowdir_TEAK03.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK03_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK03.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK03.shp",
  flow_accum = "data/temp/flowaccum_TEAK03.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK03.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK03.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK03.tif",
  output = "data/temp/streams_TEAK03.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK03.tif",
  d8_pntr = "data/temp/flowdir_TEAK03.tif",
  output = "data/temp/streams_TEAK03.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK03.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK03.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK03.tif",
  pour_pts = "data/temp/snap_pour_TEAK03.shp",
  output = "data/temp/shed_TEAK03.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK03_ws <- raster(paste0("data/temp/shed_TEAK03.tif"))

mapview(TEAK03_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK03_ws <- st_as_stars(TEAK03_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK03_ws)
#writes shapefile to data folder
#st_write(TEAK03_ws, paste0("data/data_geo/TEAK03/TEAK03_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK03_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK03_ws, paste0("data/data_geo/TEAK03/TEAK03_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK03/TEAK03_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK03/TEAK03_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK03_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK03.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK03_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1jLzVqEnFAMH5ZfvbMnU1t83WVbafwwzN")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK03/TEAK03_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK03/TEAK03_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK03/TEAK03_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK03/TEAK03_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK03/TEAK03_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK03/TEAK03_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK03/TEAK03_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK03/TEAK03_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK03/TEAK03_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK03/", full.names = TRUE)
file.remove(files)

##############
## TEAK03B ##
#############
# this site was added upstream of TEAK03 due to stream drying. The move was significant enough to warrant naming it something different. Need to delineate.

##############
## TEAK04 ##
##############

# isolate lat lon
TEAK04 = June2025_r[June2025_r$siteID=="TEAK04",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK04_sf = st_as_sf(TEAK04, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK04_sf = st_transform(TEAK04_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK04/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK04_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK04.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK04 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK04.tif"), overwrite = T)
st_write(TEAK04_sf, paste0("data/temp/synoptics_TEAK04.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK04.tif",
  output = "data/temp/dem_TEAK04_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK04_fill.tif",
  output = "data/temp/dem_TEAK04_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK04_fill_breach.tif",
  output = "data/temp/flowdir_TEAK04.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK04_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK04.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK04.shp",
  flow_accum = "data/temp/flowaccum_TEAK04.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK04.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK04.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK04.tif",
  output = "data/temp/streams_TEAK04.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK04.tif",
  d8_pntr = "data/temp/flowdir_TEAK04.tif",
  output = "data/temp/streams_TEAK04.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK04.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK04.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK04.tif",
  pour_pts = "data/temp/snap_pour_TEAK04.shp",
  output = "data/temp/shed_TEAK04.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK04_ws <- raster(paste0("data/temp/shed_TEAK04.tif"))

mapview(TEAK04_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK04_ws <- st_as_stars(TEAK04_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK04_ws)
#writes shapefile to data folder
#st_write(TEAK04_ws, paste0("data/data_geo/TEAK04/TEAK04_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK04_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK04_ws, paste0("data/data_geo/TEAK04/TEAK04_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK04/TEAK04_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK04/TEAK04_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK04_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK04.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK04_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1m_u7enjbUFurwT6-lkRgHJYbPCFo-KSS")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK04/TEAK04_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK04/TEAK04_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK04/TEAK04_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK04/TEAK04_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK04/TEAK04_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK04/TEAK04_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK04/TEAK04_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK04/TEAK04_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK04/TEAK04_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK04/", full.names = TRUE)
file.remove(files)

##############
## TEAK05 ##
##############

# isolate lat lon
TEAK05 = June2025_r[June2025_r$siteID=="TEAK05",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK05_sf = st_as_sf(TEAK05, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK05_sf = st_transform(TEAK05_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK05/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK05_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK05.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK05 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK05.tif"), overwrite = T)
st_write(TEAK05_sf, paste0("data/temp/synoptics_TEAK05.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK05.tif",
  output = "data/temp/dem_TEAK05_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK05_fill.tif",
  output = "data/temp/dem_TEAK05_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK05_fill_breach.tif",
  output = "data/temp/flowdir_TEAK05.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK05_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK05.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK05.shp",
  flow_accum = "data/temp/flowaccum_TEAK05.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK05.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK05.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK05.tif",
  output = "data/temp/streams_TEAK05.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK05.tif",
  d8_pntr = "data/temp/flowdir_TEAK05.tif",
  output = "data/temp/streams_TEAK05.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK05.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK05.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK05.tif",
  pour_pts = "data/temp/snap_pour_TEAK05.shp",
  output = "data/temp/shed_TEAK05.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK05_ws <- raster(paste0("data/temp/shed_TEAK05.tif"))

mapview(TEAK05_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK05_ws <- st_as_stars(TEAK05_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK05_ws)
#writes shapefile to data folder
#st_write(TEAK05_ws, paste0("data/data_geo/TEAK05/TEAK05_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK05_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK05_ws, paste0("data/data_geo/TEAK05/TEAK05_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK05/TEAK05_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK05/TEAK05_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK05_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK05.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK05_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1RadCWGMWOCxkSz2tCcIXzXHMAn1OU2Im")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK05/TEAK05_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK05/TEAK05_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK05/TEAK05_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK05/TEAK05_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK05/TEAK05_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK05/TEAK05_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK05/TEAK05_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK05/TEAK05_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK05/TEAK05_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK05/", full.names = TRUE)
file.remove(files)



##############
## TEAK06 ##
##############

# isolate lat lon
TEAK06 = June2025_r[June2025_r$siteID=="TEAK06",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK06_sf = st_as_sf(TEAK06, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK06_sf = st_transform(TEAK06_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK06/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK06_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK06.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK06 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK06.tif"), overwrite = T)
st_write(TEAK06_sf, paste0("data/temp/synoptics_TEAK06.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK06.tif",
  output = "data/temp/dem_TEAK06_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK06_fill.tif",
  output = "data/temp/dem_TEAK06_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK06_fill_breach.tif",
  output = "data/temp/flowdir_TEAK06.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK06_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK06.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK06.shp",
  flow_accum = "data/temp/flowaccum_TEAK06.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK06.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK06.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK06.tif",
  output = "data/temp/streams_TEAK06.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK06.tif",
  d8_pntr = "data/temp/flowdir_TEAK06.tif",
  output = "data/temp/streams_TEAK06.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK06.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK06.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK06.tif",
  pour_pts = "data/temp/snap_pour_TEAK06.shp",
  output = "data/temp/shed_TEAK06.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK06_ws <- raster(paste0("data/temp/shed_TEAK06.tif"))

mapview(TEAK06_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK06_ws <- st_as_stars(TEAK06_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK06_ws)
#writes shapefile to data folder
#st_write(TEAK06_ws, paste0("data/data_geo/TEAK06/TEAK06_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK06_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK06_ws, paste0("data/data_geo/TEAK06/TEAK06_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK06/TEAK06_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK06/TEAK06_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK06_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK06.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK06_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1t7FmEOgzbOnXyYleiaHx3caVw3E2gtwR")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK06/TEAK06_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK06/TEAK06_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK06/TEAK06_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK06/TEAK06_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK06/TEAK06_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK06/TEAK06_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK06/TEAK06_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK06/TEAK06_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK06/TEAK06_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK06/", full.names = TRUE)
file.remove(files)


##############
## TEAK07 ##
##############

# isolate lat lon
TEAK07 = June2025_r[June2025_r$siteID=="TEAK07",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK07_sf = st_as_sf(TEAK07, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK07_sf = st_transform(TEAK07_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK07/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK07_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK07.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK07 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK07.tif"), overwrite = T)
st_write(TEAK07_sf, paste0("data/temp/synoptics_TEAK07.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK07.tif",
  output = "data/temp/dem_TEAK07_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK07_fill.tif",
  output = "data/temp/dem_TEAK07_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK07_fill_breach.tif",
  output = "data/temp/flowdir_TEAK07.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK07_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK07.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK07.shp",
  flow_accum = "data/temp/flowaccum_TEAK07.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK07.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK07.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK07.tif",
  output = "data/temp/streams_TEAK07.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK07.tif",
  d8_pntr = "data/temp/flowdir_TEAK07.tif",
  output = "data/temp/streams_TEAK07.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK07.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK07.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK07.tif",
  pour_pts = "data/temp/snap_pour_TEAK07.shp",
  output = "data/temp/shed_TEAK07.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK07_ws <- raster(paste0("data/temp/shed_TEAK07.tif"))

mapview(TEAK07_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK07_ws <- st_as_stars(TEAK07_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK07_ws)
#writes shapefile to data folder
#st_write(TEAK07_ws, paste0("data/data_geo/TEAK07/TEAK07_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK07_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK07_ws, paste0("data/data_geo/TEAK07/TEAK07_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK07/TEAK07_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK07/TEAK07_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK07_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK07.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK07_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1x2LkDNaB595HAb_P5pK2yx47jLrxebUb")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK07/TEAK07_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK07/TEAK07_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK07/TEAK07_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK07/TEAK07_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK07/TEAK07_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK07/TEAK07_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK07/TEAK07_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK07/TEAK07_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK07/TEAK07_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK07/", full.names = TRUE)
file.remove(files)


##############
## TEAK08 ##
##############

# isolate lat lon
TEAK08 = June2025_r[June2025_r$siteID=="TEAK08",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK08_sf = st_as_sf(TEAK08, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK08_sf = st_transform(TEAK08_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK08/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK08_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK08.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK08 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK08.tif"), overwrite = T)
st_write(TEAK08_sf, paste0("data/temp/synoptics_TEAK08.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK08.tif",
  output = "data/temp/dem_TEAK08_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK08_fill.tif",
  output = "data/temp/dem_TEAK08_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK08_fill_breach.tif",
  output = "data/temp/flowdir_TEAK08.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK08_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK08.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK08.shp",
  flow_accum = "data/temp/flowaccum_TEAK08.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK08.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK08.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK08.tif",
  output = "data/temp/streams_TEAK08.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK08.tif",
  d8_pntr = "data/temp/flowdir_TEAK08.tif",
  output = "data/temp/streams_TEAK08.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK08.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK08.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK08.tif",
  pour_pts = "data/temp/snap_pour_TEAK08.shp",
  output = "data/temp/shed_TEAK08.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK08_ws <- raster(paste0("data/temp/shed_TEAK08.tif"))

mapview(TEAK08_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK08_ws <- st_as_stars(TEAK08_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK08_ws)
#writes shapefile to data folder
#st_write(TEAK08_ws, paste0("data/data_geo/TEAK08/TEAK08_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK08_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK08_ws, paste0("data/data_geo/TEAK08/TEAK08_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK08/TEAK08_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK08/TEAK08_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK08_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK08.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK08_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1MOprQJxeIQjiRJyw4VcKB30rvWU78Qw_")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK08/TEAK08_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK08/TEAK08_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK08/TEAK08_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK08/TEAK08_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK08/TEAK08_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK08/TEAK08_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK08/TEAK08_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK08/TEAK08_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK08/TEAK08_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK08/", full.names = TRUE)
file.remove(files)


##############
## TEAK09 ##
##############

# isolate lat lon
TEAK09 = June2025_r[June2025_r$siteID=="TEAK09",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK09_sf = st_as_sf(TEAK09, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK09_sf = st_transform(TEAK09_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK09/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK09_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK09.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK09 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK09.tif"), overwrite = T)
st_write(TEAK09_sf, paste0("data/temp/synoptics_TEAK09.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK09.tif",
  output = "data/temp/dem_TEAK09_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK09_fill.tif",
  output = "data/temp/dem_TEAK09_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK09_fill_breach.tif",
  output = "data/temp/flowdir_TEAK09.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK09_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK09.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK09.shp",
  flow_accum = "data/temp/flowaccum_TEAK09.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK09.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK09.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK09.tif",
  output = "data/temp/streams_TEAK09.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK09.tif",
  d8_pntr = "data/temp/flowdir_TEAK09.tif",
  output = "data/temp/streams_TEAK09.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK09.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK09.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK09.tif",
  pour_pts = "data/temp/snap_pour_TEAK09.shp",
  output = "data/temp/shed_TEAK09.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK09_ws <- raster(paste0("data/temp/shed_TEAK09.tif"))

mapview(TEAK09_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK09_ws <- st_as_stars(TEAK09_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK09_ws)
#writes shapefile to data folder
#st_write(TEAK09_ws, paste0("data/data_geo/TEAK09/TEAK09_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK09_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK09_ws, paste0("data/data_geo/TEAK09/TEAK09_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK09/TEAK09_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK09/TEAK09_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK09_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK09.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK09_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1x36RuryxXATrNHO_GxWqiBwqL9jnyCMn")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK09/TEAK09_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK09/TEAK09_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK09/TEAK09_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK09/TEAK09_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK09/TEAK09_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK09/TEAK09_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK09/TEAK09_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK09/TEAK09_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK09/TEAK09_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK09/", full.names = TRUE)
file.remove(files)


##############
## TEAK10 ##
##############

# isolate lat lon
TEAK10 = June2025_r[June2025_r$siteID=="TEAK10",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK10_sf = st_as_sf(TEAK10, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK10_sf = st_transform(TEAK10_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK10/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK10_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK10.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK10 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK10.tif"), overwrite = T)
st_write(TEAK10_sf, paste0("data/temp/synoptics_TEAK10.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK10.tif",
  output = "data/temp/dem_TEAK10_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK10_fill.tif",
  output = "data/temp/dem_TEAK10_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK10_fill_breach.tif",
  output = "data/temp/flowdir_TEAK10.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK10_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK10.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK10.shp",
  flow_accum = "data/temp/flowaccum_TEAK10.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK10.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK10.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK10.tif",
  output = "data/temp/streams_TEAK10.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK10.tif",
  d8_pntr = "data/temp/flowdir_TEAK10.tif",
  output = "data/temp/streams_TEAK10.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK10.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK10.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK10.tif",
  pour_pts = "data/temp/snap_pour_TEAK10.shp",
  output = "data/temp/shed_TEAK10.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK10_ws <- raster(paste0("data/temp/shed_TEAK10.tif"))

mapview(TEAK10_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK10_ws <- st_as_stars(TEAK10_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK10_ws)
#writes shapefile to data folder
#st_write(TEAK10_ws, paste0("data/data_geo/TEAK10/TEAK10_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK10_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK10_ws, paste0("data/data_geo/TEAK10/TEAK10_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK10/TEAK10_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK10/TEAK10_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK10_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK10.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK10_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1Hzx9gu4jJU8XQDYkEhNIZaMgcen5dnNC")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK10/TEAK10_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK10/TEAK10_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK10/TEAK10_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK10/TEAK10_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK10/TEAK10_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK10/TEAK10_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK10/TEAK10_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK10/TEAK10_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK10/TEAK10_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK10/", full.names = TRUE)
file.remove(files)


##############
## TEAK11 ##
##############

# isolate lat lon
TEAK11 = June2025_r[June2025_r$siteID=="TEAK11",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK11_sf = st_as_sf(TEAK11, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK11_sf = st_transform(TEAK11_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK11/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK11_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK11.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK11 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK11.tif"), overwrite = T)
st_write(TEAK11_sf, paste0("data/temp/synoptics_TEAK11.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK11.tif",
  output = "data/temp/dem_TEAK11_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK11_fill.tif",
  output = "data/temp/dem_TEAK11_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK11_fill_breach.tif",
  output = "data/temp/flowdir_TEAK11.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK11_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK11.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK11.shp",
  flow_accum = "data/temp/flowaccum_TEAK11.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK11.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK11.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK11.tif",
  output = "data/temp/streams_TEAK11.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK11.tif",
  d8_pntr = "data/temp/flowdir_TEAK11.tif",
  output = "data/temp/streams_TEAK11.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK11.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK11.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK11.tif",
  pour_pts = "data/temp/snap_pour_TEAK11.shp",
  output = "data/temp/shed_TEAK11.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK11_ws <- raster(paste0("data/temp/shed_TEAK11.tif"))

mapview(TEAK11_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK11_ws <- st_as_stars(TEAK11_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK11_ws)
#writes shapefile to data folder
#st_write(TEAK11_ws, paste0("data/data_geo/TEAK11/TEAK11_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK11_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK11_ws, paste0("data/data_geo/TEAK11/TEAK11_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK11/TEAK11_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK11/TEAK11_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK11_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK11.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK11_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1m5N-lBJRNQcAJcoPigm7BAnZcuGjWjDx")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK11/TEAK11_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK11/TEAK11_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK11/TEAK11_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK11/TEAK11_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK11/TEAK11_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK11/TEAK11_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK11/TEAK11_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK11/TEAK11_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK11/TEAK11_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK11/", full.names = TRUE)
file.remove(files)


##############
## TEAK12 ##
##############

# isolate lat lon
TEAK12 = June2025_r[June2025_r$siteID=="TEAK12",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK12_sf = st_as_sf(TEAK12, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK12_sf = st_transform(TEAK12_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK12/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK12_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK12.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK12 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK12.tif"), overwrite = T)
st_write(TEAK12_sf, paste0("data/temp/synoptics_TEAK12.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK12.tif",
  output = "data/temp/dem_TEAK12_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK12_fill.tif",
  output = "data/temp/dem_TEAK12_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK12_fill_breach.tif",
  output = "data/temp/flowdir_TEAK12.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK12_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK12.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK12.shp",
  flow_accum = "data/temp/flowaccum_TEAK12.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK12.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK12.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK12.tif",
  output = "data/temp/streams_TEAK12.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK12.tif",
  d8_pntr = "data/temp/flowdir_TEAK12.tif",
  output = "data/temp/streams_TEAK12.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK12.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK12.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK12.tif",
  pour_pts = "data/temp/snap_pour_TEAK12.shp",
  output = "data/temp/shed_TEAK12.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK12_ws <- raster(paste0("data/temp/shed_TEAK12.tif"))

mapview(TEAK12_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK12_ws <- st_as_stars(TEAK12_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK12_ws)
#writes shapefile to data folder
#st_write(TEAK12_ws, paste0("data/data_geo/TEAK12/TEAK12_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK12_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK12_ws, paste0("data/data_geo/TEAK12/TEAK12_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK12/TEAK12_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK12/TEAK12_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK12_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK12.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK12_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1WkCZ7zEXIvEbx-3Tfq0TS4kmb6OKzvb2")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK12/TEAK12_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK12/TEAK12_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK12/TEAK12_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK12/TEAK12_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK12/TEAK12_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK12/TEAK12_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK12/TEAK12_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK12/TEAK12_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK12/TEAK12_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK12/", full.names = TRUE)
file.remove(files)


##############
## TEAK13 ##
##############

# isolate lat lon
TEAK13 = June2025_r[June2025_r$siteID=="TEAK13",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK13_sf = st_as_sf(TEAK13, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK13_sf = st_transform(TEAK13_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK13/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK13_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK13.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK13 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK13.tif"), overwrite = T)
st_write(TEAK13_sf, paste0("data/temp/synoptics_TEAK13.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK13.tif",
  output = "data/temp/dem_TEAK13_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK13_fill.tif",
  output = "data/temp/dem_TEAK13_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK13_fill_breach.tif",
  output = "data/temp/flowdir_TEAK13.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK13_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK13.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK13.shp",
  flow_accum = "data/temp/flowaccum_TEAK13.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK13.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK13.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK13.tif",
  output = "data/temp/streams_TEAK13.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK13.tif",
  d8_pntr = "data/temp/flowdir_TEAK13.tif",
  output = "data/temp/streams_TEAK13.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK13.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK13.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK13.tif",
  pour_pts = "data/temp/snap_pour_TEAK13.shp",
  output = "data/temp/shed_TEAK13.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK13_ws <- raster(paste0("data/temp/shed_TEAK13.tif"))

mapview(TEAK13_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK13_ws <- st_as_stars(TEAK13_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK13_ws)
#writes shapefile to data folder
#st_write(TEAK13_ws, paste0("data/data_geo/TEAK13/TEAK13_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK13_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK13_ws, paste0("data/data_geo/TEAK13/TEAK13_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK13/TEAK13_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK13/TEAK13_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK13_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK13.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK13_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1Usvm66mlII56fHCQt9Lw9vM3r_NwDxkQ")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK13/TEAK13_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK13/TEAK13_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK13/TEAK13_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK13/TEAK13_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK13/TEAK13_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK13/TEAK13_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK13/TEAK13_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK13/TEAK13_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK13/TEAK13_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK13/", full.names = TRUE)
file.remove(files)


##############
## TEAK14 ##
##############

# isolate lat lon
TEAK14 = June2025_r[June2025_r$siteID=="TEAK14",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TEAK14_sf = st_as_sf(TEAK14, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TEAK14_sf = st_transform(TEAK14_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TEAK14/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TEAK14_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TEAK14.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TEAK14 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TEAK14.tif"), overwrite = T)
st_write(TEAK14_sf, paste0("data/temp/synoptics_TEAK14.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TEAK14.tif",
  output = "data/temp/dem_TEAK14_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TEAK14_fill.tif",
  output = "data/temp/dem_TEAK14_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TEAK14_fill_breach.tif",
  output = "data/temp/flowdir_TEAK14.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TEAK14_fill_breach.tif",
  output = "data/temp/flowaccum_TEAK14.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TEAK14.shp",
  flow_accum = "data/temp/flowaccum_TEAK14.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TEAK14.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TEAK14.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TEAK14.tif",
  output = "data/temp/streams_TEAK14.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TEAK14.tif",
  d8_pntr = "data/temp/flowdir_TEAK14.tif",
  output = "data/temp/streams_TEAK14.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TEAK14.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TEAK14.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TEAK14.tif",
  pour_pts = "data/temp/snap_pour_TEAK14.shp",
  output = "data/temp/shed_TEAK14.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TEAK14_ws <- raster(paste0("data/temp/shed_TEAK14.tif"))

mapview(TEAK14_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TEAK14_ws <- st_as_stars(TEAK14_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TEAK14_ws)
#writes shapefile to data folder
#st_write(TEAK14_ws, paste0("data/data_geo/TEAK14/TEAK14_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TEAK14_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TEAK14_ws, paste0("data/data_geo/TEAK14/TEAK14_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TEAK14/TEAK14_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TEAK14/TEAK14_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TEAK14_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TEAK14.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TEAK14_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1g2y3SNkMMvLLY1IFopn_qZnxFox7Rl0c")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TEAK14/TEAK14_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TEAK14/TEAK14_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK14/TEAK14_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK14/TEAK14_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK14/TEAK14_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TEAK14/TEAK14_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TEAK14/TEAK14_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TEAK14/TEAK14_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TEAK14/TEAK14_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TEAK14/", full.names = TRUE)
file.remove(files)


##############
## TECR01 ##
##############

# isolate lat lon
TECR01 = June2025_r[June2025_r$siteID=="TECR01",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR01_sf = st_as_sf(TECR01, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR01_sf = st_transform(TECR01_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR01/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR01_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR01.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR01 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR01.tif"), overwrite = T)
st_write(TECR01_sf, paste0("data/temp/synoptics_TECR01.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR01.tif",
  output = "data/temp/dem_TECR01_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR01_fill.tif",
  output = "data/temp/dem_TECR01_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR01_fill_breach.tif",
  output = "data/temp/flowdir_TECR01.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR01_fill_breach.tif",
  output = "data/temp/flowaccum_TECR01.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR01.shp",
  flow_accum = "data/temp/flowaccum_TECR01.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR01.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR01.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR01.tif",
  output = "data/temp/streams_TECR01.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR01.tif",
  d8_pntr = "data/temp/flowdir_TECR01.tif",
  output = "data/temp/streams_TECR01.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR01.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR01.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR01.tif",
  pour_pts = "data/temp/snap_pour_TECR01.shp",
  output = "data/temp/shed_TECR01.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR01_ws <- raster(paste0("data/temp/shed_TECR01.tif"))

mapview(TECR01_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR01_ws <- st_as_stars(TECR01_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR01_ws)
#writes shapefile to data folder
#st_write(TECR01_ws, paste0("data/data_geo/TECR01/TECR01_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR01_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR01_ws, paste0("data/data_geo/TECR01/TECR01_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR01/TECR01_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR01/TECR01_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR01_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR01.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR01_ws)

## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/12aLuGibQChrLTg2kMwANlT5B-Cf8PRzd")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR01/TECR01_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR01/TECR01_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR01/TECR01_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR01/TECR01_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR01/TECR01_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR01/TECR01_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR01/TECR01_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR01/TECR01_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR01/TECR01_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR01/", full.names = TRUE)
file.remove(files)



##############
## TECR02 ##
##############

# isolate lat lon
TECR02 = June2025_r[June2025_r$siteID=="TECR02",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR02_sf = st_as_sf(TECR02, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR02_sf = st_transform(TECR02_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR02/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR02_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR02.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR02 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR02.tif"), overwrite = T)
st_write(TECR02_sf, paste0("data/temp/synoptics_TECR02.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR02.tif",
  output = "data/temp/dem_TECR02_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR02_fill.tif",
  output = "data/temp/dem_TECR02_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR02_fill_breach.tif",
  output = "data/temp/flowdir_TECR02.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR02_fill_breach.tif",
  output = "data/temp/flowaccum_TECR02.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR02.shp",
  flow_accum = "data/temp/flowaccum_TECR02.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR02.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR02.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR02.tif",
  output = "data/temp/streams_TECR02.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR02.tif",
  d8_pntr = "data/temp/flowdir_TECR02.tif",
  output = "data/temp/streams_TECR02.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR02.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR02.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR02.tif",
  pour_pts = "data/temp/snap_pour_TECR02.shp",
  output = "data/temp/shed_TECR02.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR02_ws <- raster(paste0("data/temp/shed_TECR02.tif"))

mapview(TECR02_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR02_ws <- st_as_stars(TECR02_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR02_ws)
#writes shapefile to data folder
#st_write(TECR02_ws, paste0("data/data_geo/TECR02/TECR02_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR02_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR02_ws, paste0("data/data_geo/TECR02/TECR02_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR02/TECR02_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR02/TECR02_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR02_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR02.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR02_ws)

## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1AmiQaxxj6nQyNlq0jR3t8UAgJUK6jXGm")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR02/TECR02_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR02/TECR02_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR02/TECR02_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR02/TECR02_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR02/TECR02_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR02/TECR02_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR02/TECR02_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR02/TECR02_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR02/TECR02_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR02/", full.names = TRUE)
file.remove(files)



##############
## TECR03 ##
##############

# isolate lat lon
TECR03 = June2025_r[June2025_r$siteID=="TECR03",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR03_sf = st_as_sf(TECR03, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR03_sf = st_transform(TECR03_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR03/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR03_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR03.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR03 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR03.tif"), overwrite = T)
st_write(TECR03_sf, paste0("data/temp/synoptics_TECR03.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR03.tif",
  output = "data/temp/dem_TECR03_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR03_fill.tif",
  output = "data/temp/dem_TECR03_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR03_fill_breach.tif",
  output = "data/temp/flowdir_TECR03.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR03_fill_breach.tif",
  output = "data/temp/flowaccum_TECR03.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR03.shp",
  flow_accum = "data/temp/flowaccum_TECR03.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR03.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR03.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR03.tif",
  output = "data/temp/streams_TECR03.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR03.tif",
  d8_pntr = "data/temp/flowdir_TECR03.tif",
  output = "data/temp/streams_TECR03.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR03.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR03.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR03.tif",
  pour_pts = "data/temp/snap_pour_TECR03.shp",
  output = "data/temp/shed_TECR03.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR03_ws <- raster(paste0("data/temp/shed_TECR03.tif"))

mapview(TECR03_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR03_ws <- st_as_stars(TECR03_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR03_ws)
#writes shapefile to data folder
#st_write(TECR03_ws, paste0("data/data_geo/TECR03/TECR03_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR03_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR03_ws, paste0("data/data_geo/TECR03/TECR03_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR03/TECR03_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR03/TECR03_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR03_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR03.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR03_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1u9ch4Ik7pdZb0fr_eUFi7yZfIQrndnuP")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR03/TECR03_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR03/TECR03_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR03/TECR03_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR03/TECR03_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR03/TECR03_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR03/TECR03_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR03/TECR03_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR03/TECR03_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR03/TECR03_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR03/", full.names = TRUE)
file.remove(files)


##############
## TECR04 ##
##############

# isolate lat lon
TECR04 = June2025_r[June2025_r$siteID=="TECR04",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR04_sf = st_as_sf(TECR04, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR04_sf = st_transform(TECR04_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR04/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR04_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR04.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR04 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR04.tif"), overwrite = T)
st_write(TECR04_sf, paste0("data/temp/synoptics_TECR04.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR04.tif",
  output = "data/temp/dem_TECR04_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR04_fill.tif",
  output = "data/temp/dem_TECR04_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR04_fill_breach.tif",
  output = "data/temp/flowdir_TECR04.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR04_fill_breach.tif",
  output = "data/temp/flowaccum_TECR04.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR04.shp",
  flow_accum = "data/temp/flowaccum_TECR04.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR04.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR04.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR04.tif",
  output = "data/temp/streams_TECR04.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR04.tif",
  d8_pntr = "data/temp/flowdir_TECR04.tif",
  output = "data/temp/streams_TECR04.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR04.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR04.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR04.tif",
  pour_pts = "data/temp/snap_pour_TECR04.shp",
  output = "data/temp/shed_TECR04.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR04_ws <- raster(paste0("data/temp/shed_TECR04.tif"))

mapview(TECR04_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR04_ws <- st_as_stars(TECR04_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR04_ws)
#writes shapefile to data folder
#st_write(TECR04_ws, paste0("data/data_geo/TECR04/TECR04_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR04_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR04_ws, paste0("data/data_geo/TECR04/TECR04_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR04/TECR04_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR04/TECR04_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR04_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR04.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR04_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1A9fRh55FaKiYcibYyQgIx1wypkzJm5Yq")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR04/TECR04_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR04/TECR04_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR04/TECR04_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR04/TECR04_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR04/TECR04_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR04/TECR04_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR04/TECR04_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR04/TECR04_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR04/TECR04_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR04/", full.names = TRUE)
file.remove(files)


##############
## TECR05 ##
##############

# isolate lat lon
TECR05 = June2025_r[June2025_r$siteID=="TECR05",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR05_sf = st_as_sf(TECR05, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR05_sf = st_transform(TECR05_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR05/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR05_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR05.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR05 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR05.tif"), overwrite = T)
st_write(TECR05_sf, paste0("data/temp/synoptics_TECR05.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR05.tif",
  output = "data/temp/dem_TECR05_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR05_fill.tif",
  output = "data/temp/dem_TECR05_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR05_fill_breach.tif",
  output = "data/temp/flowdir_TECR05.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR05_fill_breach.tif",
  output = "data/temp/flowaccum_TECR05.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR05.shp",
  flow_accum = "data/temp/flowaccum_TECR05.tif",
  snap_dist = 50,
  output = "data/temp/snap_pour_TECR05.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR05.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR05.tif",
  output = "data/temp/streams_TECR05.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR05.tif",
  d8_pntr = "data/temp/flowdir_TECR05.tif",
  output = "data/temp/streams_TECR05.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR05.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR05.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR05.tif",
  pour_pts = "data/temp/snap_pour_TECR05.shp",
  output = "data/temp/shed_TECR05.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR05_ws <- raster(paste0("data/temp/shed_TECR05.tif"))

mapview(TECR05_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR05_ws <- st_as_stars(TECR05_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR05_ws)
#writes shapefile to data folder
#st_write(TECR05_ws, paste0("data/data_geo/TECR05/TECR05_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR05_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR05_ws, paste0("data/data_geo/TECR05/TECR05_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR05/TECR05_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR05/TECR05_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR05_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR05.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR05_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1Scmuk_HN8-3CrfCgu_KMKICF0oYWFhjH")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR05/TECR05_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR05/TECR05_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR05/TECR05_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR05/TECR05_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR05/TECR05_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR05/TECR05_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR05/TECR05_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR05/TECR05_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR05/TECR05_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR05/", full.names = TRUE)
file.remove(files)


##############
## TECR06 ##
##############

# isolate lat lon
TECR06 = June2025_r[June2025_r$siteID=="TECR06",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR06_sf = st_as_sf(TECR06, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR06_sf = st_transform(TECR06_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR06/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR06_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR06.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR06 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR06.tif"), overwrite = T)
st_write(TECR06_sf, paste0("data/temp/synoptics_TECR06.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR06.tif",
  output = "data/temp/dem_TECR06_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR06_fill.tif",
  output = "data/temp/dem_TECR06_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR06_fill_breach.tif",
  output = "data/temp/flowdir_TECR06.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR06_fill_breach.tif",
  output = "data/temp/flowaccum_TECR06.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR06.shp",
  flow_accum = "data/temp/flowaccum_TECR06.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR06.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR06.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR06.tif",
  output = "data/temp/streams_TECR06.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR06.tif",
  d8_pntr = "data/temp/flowdir_TECR06.tif",
  output = "data/temp/streams_TECR06.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR06.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR06.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR06.tif",
  pour_pts = "data/temp/snap_pour_TECR06.shp",
  output = "data/temp/shed_TECR06.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR06_ws <- raster(paste0("data/temp/shed_TECR06.tif"))

mapview(TECR06_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR06_ws <- st_as_stars(TECR06_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR06_ws)
#writes shapefile to data folder
#st_write(TECR06_ws, paste0("data/data_geo/TECR06/TECR06_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR06_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR06_ws, paste0("data/data_geo/TECR06/TECR06_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR06/TECR06_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR06/TECR06_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR06_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR06.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR06_ws)

## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/109YaLvPPTASNtuyjyNQQEqtqQpyBeALV")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR06/TECR06_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR06/TECR06_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR06/TECR06_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR06/TECR06_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR06/TECR06_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR06/TECR06_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR06/TECR06_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR06/TECR06_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR06/TECR06_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR06/", full.names = TRUE)
file.remove(files)



##############
## TECR12 ##
##############

# isolate lat lon
TECR12 = June2025_r[June2025_r$siteID=="TECR12",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR12_sf = st_as_sf(TECR12, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR12_sf = st_transform(TECR12_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR12/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR12_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR12.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR12 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR12.tif"), overwrite = T)
st_write(TECR12_sf, paste0("data/temp/synoptics_TECR12.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR12.tif",
  output = "data/temp/dem_TECR12_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR12_fill.tif",
  output = "data/temp/dem_TECR12_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR12_fill_breach.tif",
  output = "data/temp/flowdir_TECR12.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR12_fill_breach.tif",
  output = "data/temp/flowaccum_TECR12.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR12.shp",
  flow_accum = "data/temp/flowaccum_TECR12.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR12.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR12.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR12.tif",
  output = "data/temp/streams_TECR12.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR12.tif",
  d8_pntr = "data/temp/flowdir_TECR12.tif",
  output = "data/temp/streams_TECR12.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR12.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR12.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR12.tif",
  pour_pts = "data/temp/snap_pour_TECR12.shp",
  output = "data/temp/shed_TECR12.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR12_ws <- raster(paste0("data/temp/shed_TECR12.tif"))

mapview(TECR12_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR12_ws <- st_as_stars(TECR12_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR12_ws)
#writes shapefile to data folder
#st_write(TECR12_ws, paste0("data/data_geo/TECR12/TECR12_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR12_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR12_ws, paste0("data/data_geo/TECR12/TECR12_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR12/TECR12_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR12/TECR12_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR12_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR12.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR12_ws)

## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/12ZUW7C1B6GorLIuCQ23IZ0B_JWMNbEcR")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR12/TECR12_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR12/TECR12_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR12/TECR12_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR12/TECR12_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR12/TECR12_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR12/TECR12_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR12/TECR12_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR12/TECR12_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR12/TECR12_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR12/", full.names = TRUE)
file.remove(files)



##############
## TECR12B ##
#############
# this site was added upstream of TECR12 (upstream of an ephemeral trib) because we were concerned that TECR12 was redundant with TECR06. The move was significant enough to warrant naming it something different. Need to delineate.

##############
## TECR07 ##
##############

# isolate lat lon
TECR07 = June2025_r[June2025_r$siteID=="TECR07",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR07_sf = st_as_sf(TECR07, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR07_sf = st_transform(TECR07_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR07/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR07_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR07.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR07 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR07.tif"), overwrite = T)
st_write(TECR07_sf, paste0("data/temp/synoptics_TECR07.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR07.tif",
  output = "data/temp/dem_TECR07_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR07_fill.tif",
  output = "data/temp/dem_TECR07_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR07_fill_breach.tif",
  output = "data/temp/flowdir_TECR07.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR07_fill_breach.tif",
  output = "data/temp/flowaccum_TECR07.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR07.shp",
  flow_accum = "data/temp/flowaccum_TECR07.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR07.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR07.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR07.tif",
  output = "data/temp/streams_TECR07.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR07.tif",
  d8_pntr = "data/temp/flowdir_TECR07.tif",
  output = "data/temp/streams_TECR07.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR07.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR07.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR07.tif",
  pour_pts = "data/temp/snap_pour_TECR07.shp",
  output = "data/temp/shed_TECR07.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR07_ws <- raster(paste0("data/temp/shed_TECR07.tif"))

mapview(TECR07_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR07_ws <- st_as_stars(TECR07_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR07_ws)
#writes shapefile to data folder
#st_write(TECR07_ws, paste0("data/data_geo/TECR07/TECR07_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR07_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR07_ws, paste0("data/data_geo/TECR07/TECR07_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR07/TECR07_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR07/TECR07_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR07_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR07.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR07_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1l648HXQ_-QHkTDQ69g3I8zGFt9CoR7H7")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR07/TECR07_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR07/TECR07_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR07/TECR07_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR07/TECR07_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR07/TECR07_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR07/TECR07_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR07/TECR07_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR07/TECR07_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR07/TECR07_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR07/", full.names = TRUE)
file.remove(files)


##############
## TECR08 ##
##############

# isolate lat lon
TECR08 = June2025_r[June2025_r$siteID=="TECR08",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR08_sf = st_as_sf(TECR08, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR08_sf = st_transform(TECR08_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR08/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR08_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR08.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR08 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR08.tif"), overwrite = T)
st_write(TECR08_sf, paste0("data/temp/synoptics_TECR08.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR08.tif",
  output = "data/temp/dem_TECR08_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR08_fill.tif",
  output = "data/temp/dem_TECR08_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR08_fill_breach.tif",
  output = "data/temp/flowdir_TECR08.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR08_fill_breach.tif",
  output = "data/temp/flowaccum_TECR08.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR08.shp",
  flow_accum = "data/temp/flowaccum_TECR08.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR08.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR08.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR08.tif",
  output = "data/temp/streams_TECR08.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR08.tif",
  d8_pntr = "data/temp/flowdir_TECR08.tif",
  output = "data/temp/streams_TECR08.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR08.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR08.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR08.tif",
  pour_pts = "data/temp/snap_pour_TECR08.shp",
  output = "data/temp/shed_TECR08.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR08_ws <- raster(paste0("data/temp/shed_TECR08.tif"))

mapview(TECR08_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR08_ws <- st_as_stars(TECR08_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR08_ws)
#writes shapefile to data folder
#st_write(TECR08_ws, paste0("data/data_geo/TECR08/TECR08_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR08_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR08_ws, paste0("data/data_geo/TECR08/TECR08_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR08/TECR08_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR08/TECR08_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR08_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR08.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR08_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1meoNl4vOTPyTU68J7LFnD-CSbRBcUn-b")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR08/TECR08_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR08/TECR08_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR08/TECR08_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR08/TECR08_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR08/TECR08_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR08/TECR08_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR08/TECR08_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR08/TECR08_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR08/TECR08_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR08/", full.names = TRUE)
file.remove(files)


##############
## TECR09 ##
##############

# isolate lat lon
TECR09 = June2025_r[June2025_r$siteID=="TECR09",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR09_sf = st_as_sf(TECR09, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR09_sf = st_transform(TECR09_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR09/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR09_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR09.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR09 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR09.tif"), overwrite = T)
st_write(TECR09_sf, paste0("data/temp/synoptics_TECR09.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR09.tif",
  output = "data/temp/dem_TECR09_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR09_fill.tif",
  output = "data/temp/dem_TECR09_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR09_fill_breach.tif",
  output = "data/temp/flowdir_TECR09.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR09_fill_breach.tif",
  output = "data/temp/flowaccum_TECR09.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR09.shp",
  flow_accum = "data/temp/flowaccum_TECR09.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR09.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR09.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR09.tif",
  output = "data/temp/streams_TECR09.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR09.tif",
  d8_pntr = "data/temp/flowdir_TECR09.tif",
  output = "data/temp/streams_TECR09.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR09.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR09.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR09.tif",
  pour_pts = "data/temp/snap_pour_TECR09.shp",
  output = "data/temp/shed_TECR09.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR09_ws <- raster(paste0("data/temp/shed_TECR09.tif"))

mapview(TECR09_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR09_ws <- st_as_stars(TECR09_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR09_ws)
#writes shapefile to data folder
#st_write(TECR09_ws, paste0("data/data_geo/TECR09/TECR09_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR09_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR09_ws, paste0("data/data_geo/TECR09/TECR09_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR09/TECR09_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR09/TECR09_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR09_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR09.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR09_ws)

## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/13Lj1Rx4wYZFwJGzqw1XeBk8qR7hnoaIl")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR09/TECR09_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR09/TECR09_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR09/TECR09_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR09/TECR09_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR09/TECR09_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR09/TECR09_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR09/TECR09_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR09/TECR09_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR09/TECR09_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR09/", full.names = TRUE)
file.remove(files)



##############
## TECR10 ##
##############

# isolate lat lon
TECR10 = June2025_r[June2025_r$siteID=="TECR10",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR10_sf = st_as_sf(TECR10, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR10_sf = st_transform(TECR10_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR10/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR10_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR10.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR10 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR10.tif"), overwrite = T)
st_write(TECR10_sf, paste0("data/temp/synoptics_TECR10.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR10.tif",
  output = "data/temp/dem_TECR10_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR10_fill.tif",
  output = "data/temp/dem_TECR10_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR10_fill_breach.tif",
  output = "data/temp/flowdir_TECR10.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR10_fill_breach.tif",
  output = "data/temp/flowaccum_TECR10.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR10.shp",
  flow_accum = "data/temp/flowaccum_TECR10.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR10.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR10.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR10.tif",
  output = "data/temp/streams_TECR10.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR10.tif",
  d8_pntr = "data/temp/flowdir_TECR10.tif",
  output = "data/temp/streams_TECR10.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR10.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR10.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR10.tif",
  pour_pts = "data/temp/snap_pour_TECR10.shp",
  output = "data/temp/shed_TECR10.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR10_ws <- raster(paste0("data/temp/shed_TECR10.tif"))

mapview(TECR10_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR10_ws <- st_as_stars(TECR10_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR10_ws)
#writes shapefile to data folder
#st_write(TECR10_ws, paste0("data/data_geo/TECR10/TECR10_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR10_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR10_ws, paste0("data/data_geo/TECR10/TECR10_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR10/TECR10_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR10/TECR10_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR10_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR10.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR10_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1U9pnO09-nNeUsIVLmILsU4pgnZxR_maQ")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR10/TECR10_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR10/TECR10_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR10/TECR10_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR10/TECR10_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR10/TECR10_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR10/TECR10_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR10/TECR10_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR10/TECR10_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR10/TECR10_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR10/", full.names = TRUE)
file.remove(files)


##############
## TECR11 ##
##############

# isolate lat lon
TECR11 = June2025_r[June2025_r$siteID=="TECR11",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR11_sf = st_as_sf(TECR11, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR11_sf = st_transform(TECR11_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR11/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR11_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR11.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR11 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR11.tif"), overwrite = T)
st_write(TECR11_sf, paste0("data/temp/synoptics_TECR11.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR11.tif",
  output = "data/temp/dem_TECR11_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR11_fill.tif",
  output = "data/temp/dem_TECR11_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR11_fill_breach.tif",
  output = "data/temp/flowdir_TECR11.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR11_fill_breach.tif",
  output = "data/temp/flowaccum_TECR11.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR11.shp",
  flow_accum = "data/temp/flowaccum_TECR11.tif",
  snap_dist = 50,
  output = "data/temp/snap_pour_TECR11.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR11.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR11.tif",
  output = "data/temp/streams_TECR11.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR11.tif",
  d8_pntr = "data/temp/flowdir_TECR11.tif",
  output = "data/temp/streams_TECR11.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR11.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR11.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR11.tif",
  pour_pts = "data/temp/snap_pour_TECR11.shp",
  output = "data/temp/shed_TECR11.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR11_ws <- raster(paste0("data/temp/shed_TECR11.tif"))

mapview(TECR11_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR11_ws <- st_as_stars(TECR11_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR11_ws)
#writes shapefile to data folder
#st_write(TECR11_ws, paste0("data/data_geo/TECR11/TECR11_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR11_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR11_ws, paste0("data/data_geo/TECR11/TECR11_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR11/TECR11_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR11/TECR11_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR11_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR11.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR11_ws)


## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/174n19pF5_7UEvwAlW670QaeeQpjNS2jE")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR11/TECR11_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR11/TECR11_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR11/TECR11_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR11/TECR11_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR11/TECR11_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR11/TECR11_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR11/TECR11_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR11/TECR11_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR11/TECR11_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR11/", full.names = TRUE)
file.remove(files)


##############
## TECR13 ##
##############

# isolate lat lon
TECR13 = June2025_r[June2025_r$siteID=="TECR13",]

#convert it into bare bones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
TECR13_sf = st_as_sf(TECR13, coords = c("Lon", "Lat"), 
                     crs = '+proj=longlat +datum=WGS84 +no_defs')

#reproject to utm 16
TECR13_sf = st_transform(TECR13_sf, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()


## Clear folders that we will use ##

# List and delete all files in the folder
files <- list.files(path = "data/data_geo/TECR13/", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)


## PULL A DEM ##

### (digital elevation model) ###
## DEM - by AJS ##
pour = as_Spatial(TECR13_sf) # make pour points = spatial object
pour # check dataset
#convert Spatial Points to sf (simple features)
pour_sf = st_as_sf(pour) 
#use the sf object in get_elev_raster
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
dem = get_elev_raster(pour_sf, z = 12, clip = "bbox", expand = 3000) # if area getting cut play around with expand number
res(dem) # resolution in meters

#### If det_elev_raster says: Please connect to the internet and try again
#curl::has_internet()
# and this is FALSE
# Try this:
#assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

#plot the elevation
plot(dem)

#save the elevation raster in a folder called temp
writeRaster(dem, paste0("data/temp/dem_TECR13.tif"), overwrite = T)

#plot with mapview to check
mapview(dem) + mapview(pour_sf)


## PREP DEM AND DELINEATE ##

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and TECR13 to temp
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
#1.1
#Prepares the DEM and delineates the watershed through a series of steps:
writeRaster(dem, paste0("data/temp/dem_TECR13.tif"), overwrite = T)
st_write(TECR13_sf, paste0("data/temp/synoptics_TECR13.shp"), delete_layer = T)

#1.2
#Fills single-cell pits (small depressions or pits in the elevation data are filled in)
wbt_fill_single_cell_pits(
  dem = "data/temp/dem_TECR13.tif",
  output = "data/temp/dem_TECR13_fill.tif",
  wd = temp)

#1.3
#Breaches depressions (remove artificial depressions or flat areas in the elevation data)
wbt_breach_depressions(
  dem = "data/temp/dem_TECR13_fill.tif",
  output = "data/temp/dem_TECR13_fill_breach.tif",
  wd = temp)


#1.4
#Assigns flow direction
wbt_d8_pointer(
  dem = "data/temp/dem_TECR13_fill_breach.tif",
  output = "data/temp/flowdir_TECR13.tif",
  wd = temp)


#1.5
#Computes flow accumulation
wbt_d8_flow_accumulation(
  input = "data/temp/dem_TECR13_fill_breach.tif",
  output = "data/temp/flowaccum_TECR13.tif",
  wd = temp
)

#1.6
#Snaps pour points to the nearest flow path 
wbt_snap_pour_points(
  pour_pts = "data/temp/synoptics_TECR13.shp",
  flow_accum = "data/temp/flowaccum_TECR13.tif",
  snap_dist = 30,
  output = "data/temp/snap_pour_TECR13.shp",
  wd = temp
)

###+++++ AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###
# NOTE: The pour point MUST be on the right flow accumulation stream for the watershed to delineate correctly. 
#If the point is not on the correct stream after snapping, the only way to fix it is to play around with moving the lat/lon closer to the target stream, 
#then editing the lat/lon into the lat/lon csv, then rerunning the code to this point and checking that it got on the right stream, then delineating. 
#You can also try changing the snap_dist in the snapping function, but that won't work if the point isn't closer to the right stream 
#(it will likely end up on a different stream, or get stuck in non-stream land).

#read stream raster
streams <- raster("data/temp/flowaccum_TECR13.tif") #flow accumulation, indicating the number of cells that contribute flow to each cell in the landscape.
#filter out low-flow areas or noise in the flow accumulation raster
#streams[streams<20] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
#writes the modified streams raster to a new raster
#writeRaster(streams, paste0("temp/cropped_streams_newmex.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "data/temp/flowaccum_TECR13.tif",
  output = "data/temp/streams_TECR13.tif",
  threshold = 20, # specifies the number of cells that contribute flow to each cell in the landscape. If small streams aren't showing up, lower this number!
  wd = temp
)

wbt_raster_streams_to_vector(
  streams = "data/temp/streams_TECR13.tif",
  d8_pntr = "data/temp/flowdir_TECR13.tif",
  output = "data/temp/streams_TECR13.shp",
  wd = temp
)

#input into R 
#read shapefile containing stream data
streams <- st_read(paste0("data/temp/streams_TECR13.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)

#input snapped pour pt
pour_pt_snap <- st_read(paste0("data/temp/snap_pour_TECR13.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(pour_pt_snap) <- st_crs(dem)

#### check if points are on stream
mapview(dem, maxpixels = 742182) + 
  mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color="red")

###+++++ end AJW code added to check that snapped pour point is on the correct flow accumulation stream ++++++++###

#1.7
#Delineates the watershed 
wbt_watershed(
  d8_pntr = "data/temp/flowdir_TECR13.tif",
  pour_pts = "data/temp/snap_pour_TECR13.shp",
  output = "data/temp/shed_TECR13.tif",
  wd = temp
)

#1.8
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
TECR13_ws <- raster(paste0("data/temp/shed_TECR13.tif"))

mapview(TECR13_ws, maxpixels =  2970452)

#1.9
#converts into a stars object, it is a multi-dimensional array that represents raster data.
TECR13_ws <- st_as_stars(TECR13_ws) %>% st_as_sf(merge=T) #it says to skip but it works with this one

#plots watershed shapefile
mapview(TECR13_ws)
#writes shapefile to data folder
#st_write(TECR13_ws, paste0("data/data_geo/TECR13/TECR13_watershed.shp"), delete_layer = T)

#plots dem raster with newmex shapefile
mapview(TECR13_ws) + mapview(dem) + mapview(pour_sf)

#export all of these so we have them!
st_write(TECR13_ws, paste0("data/data_geo/TECR13/TECR13_watershed.shp"), delete_layer = T)
st_write(streams, paste0("data/data_geo/TECR13/TECR13_stream_network.shp"), delete_layer = T)
writeRaster(dem, paste0("data/data_geo/TECR13/TECR13_DEM.tif"), overwrite=T)

#GET THE AREA OF YOUR WATERSHED POLYGONS (it has to be in sf format)
(sum(st_area(TECR13_ws)))/1000000 # in square km

# #Check area is ok with flowdir
# flowdir = raster('data/temp/flowdir_TECR13.tif')
# plot(flowdir) + plot(streams)
# mapview(flowdir)+mapview(streams)+mapview(pour_sf)+mapview(TECR13_ws)



## WRITE DATA TO GOOGLE DRIVE ##

# authenticate connection to Google Drive
drive_auth() 
2

# save Google Drive folder ID
folder_id = as_id("https://drive.google.com/drive/folders/1YBbhiwn4YUm22kSIxForvG44Wg1N3jkK")

# upload files one by one (there is probs a better way)
drive_upload(media = "data/data_geo/TECR13/TECR13_DEM.tif", path = folder_id)
drive_upload(media = "data/data_geo/TECR13/TECR13_stream_network.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR13/TECR13_stream_network.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR13/TECR13_stream_network.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR13/TECR13_stream_network.shx", path = folder_id)
drive_upload(media = "data/data_geo/TECR13/TECR13_watershed.dbf", path = folder_id)
drive_upload(media = "data/data_geo/TECR13/TECR13_watershed.prj", path = folder_id)
drive_upload(media = "data/data_geo/TECR13/TECR13_watershed.shp", path = folder_id)
drive_upload(media = "data/data_geo/TECR13/TECR13_watershed.shx", path = folder_id)

# Clear local folders #
files <- list.files(path = "data/temp", full.names = TRUE)
file.remove(files)
files <- list.files(path = "data/data_geo/TECR13/", full.names = TRUE)
file.remove(files)

