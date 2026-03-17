
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
#specify google drive folder
folder_id <- "1p9D19AD-kVP1evKlDuaRglwtxDMNj7iM"

# Create local folder "csvs" if it doesn't exist
csv_folder <- "csvs"
if (!dir.exists(csv_folder)) {
  dir.create(csv_folder)
}

# Step 1: list all files inside that Drive folder
files_in_folder <- drive_ls(as_id(folder_id))

# Step 2: loop through all files and download into csvs folder
for (i in seq_len(nrow(files_in_folder))) {
  file <- files_in_folder[i, ]
  
  # Build the path inside the csvs folder
  local_path <- file.path(csv_folder, file$name)
  
  drive_download(
    as_id(file$id),
    path = local_path,   # saves into csvs folder
    overwrite = TRUE
  )
}


#### Load data into R ####
# omitted rating curve because it's... different
gases <- read.csv("csvs/all_dissolvedgases_data.csv")
elevation <- read.csv("csvs/all_elevation_data.csv")
fieldQ <- read.csv("csvs/all_fielddischarge_data.csv")
gaugeheight <- read.csv("csvs/all_gaugeheight_data.csv")
isotopes <- read.csv("csvs/all_isotope_data.csv")
nitrate <- read.csv("csvs/all_nitrate_data.csv")
temp <- read.csv("csvs/all_temp_data.csv")
chem <- read.csv("csvs/all_waterchem_data.csv")
wq <- read.csv("csvs/all_waterquality_data.csv")





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

#### 1) Nitrates ####
#We want the adjusted N mean column (which is in milligrams/L of NO3-N)
#But for any data that is flagged (FinalQF = 1), we want to put a NA in the adjusted N mean column so we don't use bad data
 
nitrate <- nitrate %>%
mutate(adj_N_mean = case_when(
  finalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1
  TRUE ~ adj_N_mean          # otherwise keep the original value
))

#Okay, now lets just select start/end date (in POSIX) and the adjusted N mean column
nitrate <- nitrate %>%
  select(startDateTime_PT, adj_N_mean)


#### 2) Water Temperature ####

#We probably want mean Surface Water Temp mean
#But for any data that is flagged (FinalQF = 1), we want to put a NA in the mean temp column so we don't use bad data
temp <- temp %>%
  mutate(surfacewaterTempMean = case_when(
    sWatTempFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ surfacewaterTempMean          # otherwise keep the original value
  ))

#Now select the columns of interest
temp <- temp %>%
  select(startDateTime_PT, surfacewaterTempMean)

#Alright we have temp data every 5 minutes but we want to summarize that to every 15 minutes

temp_15 <- temp %>%
  # Step 1: round down to nearest 15 minutes
  mutate(startDateTime_PT = floor_date(startDateTime_PT, "15 minutes")) %>%
  
  # Step 2: group by the new 15-min timestamp.  This one removes NAs from the mean, but ask Alex if any 15 minute period with a NA should be summarized to NA. 
  group_by(startDateTime_PT) %>%
  summarize(
    surfacewaterTempMean = mean(surfacewaterTempMean, na.rm = TRUE))



#### 3) Water Quality ####
#ok this one has a number of different variables...first lets go through and make any flagged data NA

wq <- wq %>%
  mutate(specificConductance = case_when(
    specificCondFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ specificConductance          # otherwise keep the original value
  ))

wq <- wq %>%
  mutate(dissolvedOxygen = case_when(
    dissolvedOxygenFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ dissolvedOxygen         # otherwise keep the original value
  ))

wq <- wq %>%
  mutate(seaLevelDissolvedOxygenSat = case_when(
    seaLevelDOSatFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ seaLevelDissolvedOxygenSat         # otherwise keep the original value
  ))

wq <- wq %>%
  mutate(localDissolvedOxygenSat = case_when(
    localDOSatFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ localDissolvedOxygenSat        # otherwise keep the original value
  ))

wq <- wq %>%
  mutate(pH = case_when(
    pHFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ pH       # otherwise keep the original value
  ))

wq <- wq %>%
  mutate(chlorophyll = case_when(
    chlorophyllFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ chlorophyll       # otherwise keep the original value
  ))

wq <- wq %>%
  mutate(turbidity = case_when(
    turbidityFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ turbidity      # otherwise keep the original value
  ))

wq <- wq %>%
  mutate(fDOM = case_when(
    fDOMFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ fDOM     # otherwise keep the original value
  ))


#Ok going to hazard a guess of what columns we may be interested in here:
wq <- wq %>%
  select(startDateTime_PT, specificConductance, dissolvedOxygen, seaLevelDissolvedOxygenSat, localDissolvedOxygenSat, pH, chlorophyll, turbidity, fDOM)


#Ok so this data is collected every minute! Woah. Let's summarize down to the 15 minute level by taking a mean of the variables we are interested in over the 15 minute period

wq_15 <- wq %>%
  # Step 1: round down to nearest 15 minutes
  mutate(startDateTime_PT = floor_date(startDateTime_PT, "15 minutes")) %>%
  
  # Step 2: group by the new 15-min timestamp.  This one removes NAs from the mean, but ask Alex if any 15 minute period with a NA should be summarized to NA. 
  group_by(startDateTime_PT) %>%
  summarize(
    specificConductance = mean(specificConductance, na.rm = TRUE),
    dissolvedOxygen = mean(dissolvedOxygen, na.rm = TRUE),
    seaLevelDissolvedOxygenSat = mean(seaLevelDissolvedOxygenSat, na.rm = TRUE),
    localDissolvedOxygenSat = mean(localDissolvedOxygenSat, na.rm = TRUE),
    pH = mean(pH, na.rm = TRUE),
    chlorophyll = mean(chlorophyll, na.rm = TRUE),
    turbidity = mean(turbidity, na.rm = TRUE),
    fDOM = mean(fDOM, na.rm = TRUE)
  )

#Date-time formatting got a little messed up -- lets fix it
wq_15$startDateTime_PT <- as.POSIXct(wq_15$startDateTime_PT, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific" )
  
# Got a lot of NaNs by taking the mean of NAs.  Not sure it matters ,but replace all NaNs with NA in the entire data frame
wq_15 <- wq_15 %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))








#try a little join

#nitrate to temp
df_joined <- nitrate %>%
  full_join(temp_15, by = "startDateTime_PT")

#nitrate + temp to water quality
df_joined <- df_joined %>%
  full_join(wq_15, by = "startDateTime_PT")








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
# Temperature: 2018 to 2024 - collected every 5 minutes
# Water Quality: 2018 to 2021 - collected every minute




#We can finish by deleting the folder of csvs before pushing to Github
unlink("csvs", recursive = TRUE, force = TRUE)

