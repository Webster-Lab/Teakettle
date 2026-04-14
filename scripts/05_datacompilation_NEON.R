##==============================================================================
## Project: TEA
## Author: Carolina May
## This script uses the neonUtilites package to download & merge multiple types of NEON data across the timeframe of interest
## This script can be used in substitution of the individual scripts in the NEON data compilation folder, and can be updated as new data becomes available
## No provisional data were included in compilation 
## As needed, use neonUtilities package to pull issue log, maintenance log, variable info, etc
## Last update: 2026-04-02
##===========================================================================

#### Load libraries ####
library(googledrive)
library(dplyr)
library(lubridate)
library(tidyverse)
library(neonUtilities)


#### Surface Water Elevation ####

tecr_elevation <- loadByProduct(
  dpID = "DP1.20016.001",
  site = "TECR",
  startdate = "2018-01",
  enddate   = "2025-12",
  package   = "expanded",
  check.size = FALSE
)

#select the 5 minute data
elevation <- tecr_elevation$EOS_5_min

#select the lower site (there are two sites being monitored for TECR surface water elevation)
elevation_lower <- elevation %>%
  filter(horizontalPosition == 102 | horizontalPosition == 110)

#format Date/Time
elevation_lower$startDateTime <- as.POSIXct(elevation_lower$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

elevation_lower$endDateTime <- as.POSIXct(elevation_lower$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

write.csv(elevation_lower, "all_elevation_data.csv", row.names = F)

#Upload to Drive
drive_upload(
  media = "all_elevation_data.csv",
  path = as_id("https://drive.google.com/drive/folders/1Ja-yVniIp6-bhnYdkTp-C-ga-2ctfHhI"),
  name = "all_elevation_data.csv"
)



#### Nitrate ####

nitrates <- loadByProduct(
  dpID = "DP1.20033.001",
  site = "TECR",
  startdate = "2018-01",
  enddate   = "2025-12",
  package   = "expanded",
  check.size = FALSE
)

nitrates <- nitrates$NSW_15_minute

# reformat Date and Time
nitrates$startDateTime <- as.POSIXct(nitrates$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
nitrates$endDateTime <- as.POSIXct(nitrates$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# Since this data is in NO3-N, we will be using the molecular weight of Nitrogen: 14, divided by 1000

nitrates$adj_N_mean <- (nitrates$surfWaterNitrateMean * 0.014)

write.csv(nitrates, "all_nitrate_data.csv", row.names = F)

#Upload to Drive
drive_upload(
  media = "all_nitrate_data.csv",
  path = as_id("https://drive.google.com/drive/folders/1Ja-yVniIp6-bhnYdkTp-C-ga-2ctfHhI"),
  name = "all_nitrate_data.csv"
)


#### Water Chemistry ####

chem <- loadByProduct(
  dpID = "DP1.20093.001",
  site = "TECR",
  startdate = "2018-01",
  enddate   = "2025-12",
  package   = "expanded",
  check.size = FALSE
)

chem <- chem$swc_externalLabDataByAnalyte

# reformat Date and Time
chem$startDate <- as.POSIXct(chem$startDate, format = "%Y-%m-%dT%H:%MZ")
chem$collectDate <- as.POSIXct(chem$collectDate, format = "%Y-%m-%dT%H:%MZ")


write.csv(chem, "all_waterchem_data.csv", row.names = F)

#Upload to Drive
drive_upload(
  media = "all_waterchem_data.csv",
  path = as_id("https://drive.google.com/drive/folders/1Ja-yVniIp6-bhnYdkTp-C-ga-2ctfHhI"),
  name = "all_waterchem_data.csv"
)


#### Continuous Discharge ####

cont_discharge <- loadByProduct(
  dpID = "DP4.00130.001",
  site = "TECR",
  startdate = "2018-01",
  enddate   = "2025-12",
  package   = "expanded",
  check.size = FALSE
)

#data split into 2 different datasets and column names differ.  Needs some work. 
cont_discharge_2021_2024 <- cont_discharge$csd_15_min
cont_discharge_2018_2021 <- cont_discharge$csd_continuousDischarge

contq_all <- bind_rows(cont_discharge_2018_2021, cont_discharge_2021_2024)

#OK lots of problems still.  Once we decide what columns are needed, match up the old columns with the new ones.
#Let's match up the dates
#Coalescing

contq_all <- contq_all %>%
  mutate(
    endDateTime = as.POSIXct(endDateTime,
                             format = "%Y-%m-%dT%H:%M:%SZ",
                             tz = "UTC"),
    
    endDate = as.POSIXct(endDate, tz = "UTC")  # important!
  ) %>%
  mutate(DateTime_UTC = coalesce(endDateTime, endDate))


#woo, okay, so now we need to coalesce the discharge data
contq_all <- contq_all %>%
  mutate(dischargeContinuous_merged = coalesce(maxpostDischarge, dischargeContinuous))


#### Write and rename the dataframe as a CSV ####
write.csv(contq_all, "all_contdischarge_data.csv", row.names = F)

#Upload to Drive
drive_upload(
  media = "all_contdischarge_data.csv",
  path = as_id("https://drive.google.com/drive/folders/1Ja-yVniIp6-bhnYdkTp-C-ga-2ctfHhI"),
  name = "all_contdischarge_data.csv"
)


#### Field Discharge ####

fieldQ <- loadByProduct(
  dpID = "DP1.20048.001",
  site = "TECR",
  startdate = "2018-01",
  enddate   = "2025-12",
  package   = "expanded",
  check.size = FALSE
)

fieldQ <- fieldQ$dsc_fieldData

# reformat Date and Time
fieldQ$collectDate <- as.POSIXct(fieldQ$collectDate, format = "%Y-%m-%dT%H:%MZ")


write.csv(fieldQ, "all_fielddischarge_data.csv", row.names = F)

#Upload to Drive
drive_upload(
  media = "all_fielddischarge_data.csv",
  path = as_id("https://drive.google.com/drive/folders/1Ja-yVniIp6-bhnYdkTp-C-ga-2ctfHhI"),
  name = "all_fielddischarge_data.csv"
)

#### Stable Isotopes ####

isotopes <- loadByProduct(
  dpID = "DP1.20206.001",
  site = "TECR",
  startdate = "2018-01",
  enddate   = "2025-12",
  package   = "expanded",
  check.size = FALSE
)

isotopes <- isotopes$asi_externalLabH2OIsotopes

# reformat Date and Time
isotopes$collectDate <- as.POSIXct(isotopes$collectDate, format = "%Y-%m-%d")

write.csv(isotopes, "all_isotope_data.csv", row.names = F)

#Upload to Drive
drive_upload(
  media = "all_isotope_data.csv",
  path = as_id("https://drive.google.com/drive/folders/1Ja-yVniIp6-bhnYdkTp-C-ga-2ctfHhI"),
  name = "all_isotope_data.csv"
)

#### Water Temperature ####
temp <- loadByProduct(
  dpID = "DP1.20054.001",
  site = "TECR",
  startdate = "2018-01",
  enddate   = "2025-12",
  package   = "expanded",
  check.size = FALSE
)

temp <- temp$TOSW_5_min

# reformat Date and Time
temp$startDateTime <- as.POSIXct(temp$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temp$endDateTime <- as.POSIXct(temp$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")


write.csv(temp, "all_temp_data.csv", row.names = F)

#Upload to Drive
drive_upload(
  media = "all_temp_data.csv",
  path = as_id("https://drive.google.com/drive/folders/1Ja-yVniIp6-bhnYdkTp-C-ga-2ctfHhI"),
  name = "all_temp_data.csv"
)



#### Water Quality ####


wq <- loadByProduct(
  dpID = "DP1.20288.001",
  site = "TECR",
  startdate = "2018-01",
  enddate   = "2025-12",
  package   = "expanded",
  check.size = FALSE
)

wq <- wq$waq_instantaneous

# reformat Date and Time
wq$startDateTime <- as.POSIXct(wq$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq$endDateTime <- as.POSIXct(wq$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

write.csv(wq, "all_waterquality_data.csv", row.names = F)

#Upload to Drive
drive_upload(
  media = "all_waterquality_data.csv",
  path = as_id("https://drive.google.com/drive/folders/1Ja-yVniIp6-bhnYdkTp-C-ga-2ctfHhI"),
  name = "all_waterquality_data.csv"
)



#Alrighty delete all the csvs we downloaded to clean up the repo before committing and pushing
# List all CSV files in the current working directory (repo root)
csv_files <- list.files(path = ".", pattern = "\\.csv$", full.names = TRUE)

# Remove them
file.remove(csv_files)