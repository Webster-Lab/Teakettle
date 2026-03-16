
#### READ ME ####
# This purpose of this script is to clean and consolidate NEON data.
# It utilizes the "merged" datasets from the "Merged datasets" folder in Google Drive. 
# The only data not included here is the salt-based discharge dataset. See notes 
# in "08_saltdischargeNEON.R" for an explanation.

#### Libraries ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(googledrive) 


#### Download data ####
# Replace with your folder ID
folder_id <- "1p9D19AD-kVP1evKlDuaRglwtxDMNj7iM"

# List all files inside that folder
files_in_folder <- drive_ls(as_id(folder_id))
files_in_folder


# 3️ Loop through all files and download them
for (i in seq_len(nrow(files_in_folder))) {
  file <- files_in_folder[i, ]
  drive_download(
    as_id(file$id),
    path = file$name,   # saves with the same name as in Drive
    overwrite = TRUE
  )
}

#### Load data into R ####
# omitted rating curve because it's... different
gases <- read.csv("all_dissolvedgases_data.csv")
elevation <- read.csv("all_elevation_data.csv")
fieldQ <- read.csv("all_fielddischarge_data.csv")
gaugeheight <- read.csv("all_gaugeheight_data.csv")
isotopes <- read.csv("all_isotope_data.csv")
nitrate <- read.csv("all_nitrate_data.csv")
temp <- read.csv("all_temp_data.csv")
chem <- read.csv("all_waterchem_data.csv")
wq <- read.csv("all_waterquality_data.csv")

#remove the downloaded files
file.remove(files_in_folder$name)

#### Date-Time Formatting ####

gases$collectDate_PT <- as.POSIXct(gases$collectDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
elevation$startDateTime_PT <- as.POSIXct(elevation$startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific" )
elevation$endDateTime_PT <- as.POSIXct(elevation$endDateTime, tz = "US/Pacific")
fieldQ$collectDate_PT <- as.POSIXct(fieldQ$collectDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
gaugeheight$startDate_PT <- as.POSIXct(gaugeheight$startDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
gaugeheight$endDate_PT <- as.POSIXct(gaugeheight$startDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
isotopes$collectDate_PT = as.POSIXct(isotopes$collectDate, format="%Y-%m-%d", tz="US/Pacific") 
chem$startDate_PT <- as.POSIXct(chem$startDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific" )
chem$collectDate_PT <- as.POSIXct(chem$collectDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
wq$startDateTime_PT <- as.POSIXct(wq$startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific" )


#nitrate and temp have some date-times and some that are just dates 
# use parse_date_time() instead of as.POSIXct() to specify multiple formats
nitrate$startDateTime_PT = 
  parse_date_time(nitrate$startDateTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d" ), exact = T, tz="US/Pacific")
nitrate$endDateTime_PT = 
  parse_date_time(nitrate$endDateTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d" ), exact = T, tz="US/Pacific")
temp$startDateTime_PT = 
  parse_date_time(temp$startDateTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d" ), exact = T, tz="US/Pacific")


#### Data Cleaning ####

###How to merge?? Sampling intervals are not consistent across data. Could do an append and have them as separate entries -- feels messy
#Rounding to 15 minute intervals does not appear to solve this

#Which columns to keep, which columns to toss? I'm not sure which ones are important

#How do I identify outliers/gaps/problem data?

#Am I going for wide data or long data?


#Next steps: Identify which types of data are collected every 15 minutes and can actually be merged

# Water Chemistry : 2018 to 2023  - collected monthly to weekly
# Elevation: 2018-to 2024 - collected every 5 minutes
# Field Q: 2017 to 2023 - collected monthly to weekly
# Gases: 2018 to 2023 - collected monthly to weekly
# Gauge height: 2017 to 2023 - collected monthly to weekly
# Isotopes: 2018 to 2023 - collected monthly to weekly (NO HH:MM:SS)
# Nitrate: 2018 to 2024 - collected every 15 minutes
# Temperature: 2018 to 2024 - collected every 15 minutes
# Water Quality: 2018 to 2021 - collected every minute





