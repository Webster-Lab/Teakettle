
#### READ ME ####
# This purpose of this script is to clean and consolidate NEON data 
# It utilizes the "merged" datasets from the "Merged datasets" folder in Google Drive. 
# It includes date-time formatting, aggregating/rounding to 15 min intervals, changing QC-flagged data to NA, joining data into a single long dataframe

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

gases <- read.csv("csvs/all_dissolvedgases_data.csv")
elevation <- read.csv("csvs/all_elevation_data.csv")
fieldQ <- read.csv("csvs/all_fielddischarge_data.csv")
gaugeheight <- read.csv("csvs/all_gaugeheight_data.csv")
isotopes <- read.csv("csvs/all_isotope_data.csv")
nitrate <- read.csv("csvs/all_nitrate_data.csv")
temp <- read.csv("csvs/all_temp_data.csv")
chem <- read.csv("csvs/all_waterchem_data.csv")
wq <- read.csv("csvs/all_waterquality_data.csv")
contQ <- read.csv("csvs/all_contdischarge_data.csv")





#### Date-Time Formatting ####

###Fix these! These are actually in UTC from Elliot's code -- if we want Pacific Time need to convert (which is better?)

gases$DateTime_PT <- as.POSIXct(gases$collectDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
fieldQ$DateTime_PT <- as.POSIXct(fieldQ$collectDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
gaugeheight$DateTime_PT <- as.POSIXct(gaugeheight$startDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
isotopes$DateTime_PT = as.POSIXct(isotopes$collectDate, format="%Y-%m-%d", tz="US/Pacific") 
chem$DateTime_PT <- as.POSIXct(chem$startDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific" )
chem$DateTime_PT <- as.POSIXct(chem$collectDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
wq$DateTime_PT <- as.POSIXct(wq$startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific" )
contQ$DateTime_PT<- as.POSIXct(contQ$DateTime_PT, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific" )


#nitrate and temp have some date-times and some that are just dates 
# use parse_date_time() instead of as.POSIXct() to specify multiple formats
nitrate$DateTime_PT = 
  parse_date_time(nitrate$startDateTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d" ), exact = T, tz="US/Pacific")
temp$DateTime_PT = 
  parse_date_time(temp$startDateTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d" ), exact = T, tz="US/Pacific")
elevation$DateTime_PT = 
  parse_date_time(elevation$startDateTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d" ), exact = T, tz="US/Pacific")


#### Data Cleaning ####
#change flagged data to NA and select columns of interest

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
  select(DateTime_PT, adj_N_mean)


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
  select(DateTime_PT, surfacewaterTempMean)

#Alright we have temp data every 5 minutes but we want to summarize that to every 15 minutes

temp_15 <- temp %>%
  # Step 1: round down to nearest 15 minutes
  mutate(DateTime_PT = floor_date(DateTime_PT, "15 minutes")) %>%
  
  # Step 2: group by the new 15-min timestamp.  This one removes NAs from the mean, but ask Alex if any 15 minute period with a NA should be summarized to NA. 
  group_by(DateTime_PT) %>%
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
  select(DateTime_PT, specificConductance, dissolvedOxygen, seaLevelDissolvedOxygenSat, localDissolvedOxygenSat, pH, chlorophyll, turbidity, fDOM)


#Ok so this data is collected every minute! Woah. Let's summarize down to the 15 minute level by taking a mean of the variables we are interested in over the 15 minute period

wq_15 <- wq %>%
  # Step 1: round down to nearest 15 minutes
  mutate(DateTime_PT = floor_date(DateTime_PT, "15 minutes")) %>%
  
  # Step 2: group by the new 15-min timestamp.  This one removes NAs from the mean, but ask Alex if any 15 minute period with a NA should be summarized to NA. 
  group_by(DateTime_PT) %>%
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
  
# Got a lot of NaNs by taking the mean of NAs.  Not sure it matters ,but replace all NaNs with NAs for anything numeric (doing the whole dataframe breaks the datetime)
wq_15 <- wq_15 %>%
  mutate(across(where(is.numeric), ~ na_if(., NaN)))



#### 4) Isotope Data ####
#this data is collected weekly to monthly, and only includes a date (no time)
#lets go through and make any flagged data NA

isotopes <- isotopes %>%
  mutate(d18OWater = case_when( 
    isotopeH2OExternalLabQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ d18OWater),
    d2HWater = case_when(
      isotopeH2OExternalLabQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
      TRUE ~ d2HWater   # otherwise keep the original value
  ))
#pick out the columns we're probably interested in
isotopes <- isotopes %>%
  select(DateTime_PT, d18OWater, d2HWater)

#### 5) Gases ####
#lets go through and make any flagged data NA for all three gases
gases <- gases %>%
  mutate(concentrationCO2 = case_when(
    CO2CheckStandardQF == 1 ~ NA_real_, # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ concentrationCO2),
    concentrationCH4 = case_when(
    CH4CheckStandardQF == 1 ~ NA_real_, # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ concentrationCH4),
    concentrationN2O = case_when(
    N2OCheckStandardQF == 1 ~ NA_real_, # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ concentrationN2O,
  ))
#pick out the columns of interest
gases <- gases %>%
  select(DateTime_PT, concentrationCO2, concentrationCH4, concentrationN2O, sampleID)
### Some of these samples are air and some are water samples! Do we just want to keep the water samples, or are we interested in the difference? 

#Since this data was collected at odd times, lets round it to the nearest 15 minutes
gases$DateTime_PT = round_date(gases$DateTime_PT, "15 minute")



#### 6) Elevation ####

#We probably want mean elevation
#But for any data that is flagged (FinalQF = 1), we want to put a NA in the mean temp column so we don't use bad data
elevation <- elevation %>%
  mutate(surfacewaterElevMean = case_when(
    sWatElevFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1.  NA_real specifies numeric NA
    TRUE ~ surfacewaterElevMean          # otherwise keep the original value
  ))

#Now select the columns of interest
elevation <- elevation %>%
  select(DateTime_PT, surfacewaterElevMean)

#Alright we have temp data every 5 minutes but we want to summarize that to every 15 minutes

elev_15 <- elevation %>%
  # Step 1: round down to nearest 15 minutes
  mutate(DateTime_PT = floor_date(DateTime_PT, "15 minutes")) %>%
  
  # Step 2: group by the new 15-min timestamp.  This one removes NAs from the mean, but ask Alex if any 15 minute period with a NA should be summarized to NA. 
  group_by(DateTime_PT) %>%
  summarize(
    surfacewaterElevMean = mean(surfacewaterElevMean, na.rm = TRUE))

#### 7) Field Q ####
#This data was collected weekly to monthly

#QF field seems to be full on NAs -- not helpful

#First we'll round to the nearest 15 minutes
fieldQ$DateTime_PT = round_date(fieldQ$DateTime_PT, "15 minute")

#Next, select columns of interest
fieldQ <- fieldQ %>%
  select(DateTime_PT, finalDischarge)



#### 8) Continuous Q ####
# This data was a little messy since NEON changed its sampling frequency and variables in 2021.
# Coalesce the two different discharge variables
contQ <- contQ %>%
mutate(dischargeContinuous_merged = coalesce(maxpostDischarge, dischargeContinuous))


# Lets set any flagged data to NA:
contQ <- contQ %>%
  mutate(dischargeContinuous_merged = case_when(
    dischargeFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1
    TRUE ~ dischargeContinuous_merged          # otherwise keep the original value
  ))

#Summarize all data by 15 min intervals (they sampled every minute until 2021, then switched to every 15 minutes)

contQ_15 <- contQ %>%
  # Step 1: round down to nearest 15 minutes
  mutate(DateTime_PT = floor_date(DateTime_PT, "15 minutes")) %>%
  
  # Step 2: group by the new 15-min timestamp.  This one removes NAs from the mean, but ask Alex if any 15 minute period with a NA should be summarized to NA. 
  group_by(DateTime_PT) %>%
  summarize(
    dischargeContinuous_merged = mean(dischargeContinuous_merged, na.rm = TRUE))


# Got a lot of NaNs by taking the mean of NAs.  Not sure it matters ,but replace all NaNs with NAs for anything numeric (doing the whole dataframe breaks the datetime)
contQ_15 <- contQ_15 %>%
  mutate(across(where(is.numeric), ~ na_if(., NaN)))

#### Joining & pivoting to long data ####

#try a little join

#nitrate to temp
df_joined <- nitrate %>%
  full_join(temp_15, by = "DateTime_PT")

#add water quality
df_joined <- df_joined %>%
  full_join(wq_15, by = "DateTime_PT")

#add isotopes
df_joined <- df_joined %>%
  full_join(isotopes, by = "DateTime_PT")
#defaults to joining at midnight since there is no time...I presume this is okay

#add elevation
df_joined <-df_joined %>%
  full_join(elev_15, by = "DateTime_PT")

#add field discharge 
df_joined <-df_joined %>%
  full_join(fieldQ, by = "DateTime_PT")

df_joined <- df_joined %>%
  full_join(contQ_15, by = "DateTime_PT")


#lets pivot longer 
df_long <- df_joined %>%
  pivot_longer(
    cols = -DateTime_PT,
    names_to = "variable",
    values_to = "value"
  )

#Ok better keep track of our units up in here
#Pulled from NEON variables csv

df_long <- df_long %>%
  mutate(units = case_when(
    variable == "adj_N_mean" ~ "mg/L N03-N",
    variable == "surfacewaterTempMean" ~ "°C",
    variable == "specificConductance" ~ "µS/cm",
    variable == "dissolvedOxygen" ~ "mg/L",
    variable == "pH" ~ " ",
    variable == "chlorophyll" ~"µg/L",
    variable == "seaLevelDissolvedOxygenSat" ~ "%",
    variable == "localDissolvedOxygenSat" ~ "%",
    variable == "turbidity" ~ "Formazin Nephelometric Units",
    variable == "fDOM" ~ "Quinine Sulfate Unit",
    variable == "d18OWater" ~ "‰ (per mil)",
    variable == "d2HWater" ~ "‰ (per mil)",
    variable == "surfacewaterElevMean" ~ "meters",
    variable == "finalDischarge" ~ "Liters/sec",
    variable == "dischargeContinuous_merged" ~ "Liters/sec"
    ))



#Let's make a faceted plot

df_long <- df_long %>%
  mutate(var_label = paste0(variable, " (", units, ")"))

ggplot(df_long, aes(x = DateTime_PT, y = value))+
  geom_point()+
  facet_wrap(~ var_label, scales = "free_y")+
  theme_minimal()
#yikes. too many facets


#We can finish by deleting the folder of csvs before pushing to Github
unlink("csvs", recursive = TRUE, force = TRUE)


#### Questions for Alex ####

#Leave Date-Time format in UTC or convert to Pacific Time?

#Which columns to keep, which columns to remove?
  #Removed flag columns after I made flagged data NA
  #Not currently keeping other metadata in this dataframe (who analyzed it, analysis dates etc)
  #Not currently keeping standard error, variance, min/max, or exp uncertainty columns

#Is there a protocol for identifying outliers/problem data?
#Gap filling? Use interpolation?
 
#For the monthly/weekly variables: do we want NA's every 15 minutes when they weren't collected?
  
#Gases includes both air and water samples -- do we want both or do we want some sort of difference of the two?

#What's the most useful way to plot this data? 


# Water Chemistry : 2018 to 2023  - collected monthly to weekly
# Elevation: 2018-to 2024 - collected every 5 minutes
# Field Q: 2017 to 2023 - collected monthly to weekly
# Gases: 2018 to 2023 - collected monthly to weekly
# Gauge height: 2017 to 2023 - collected monthly to weekly
# Isotopes: 2018 to 2023 - collected monthly to weekly (NO HH:MM:SS)
# Nitrate: 2018 to 2024 - collected every 15 minutes
# Temperature: 2018 to 2024 - collected every 5 minutes
# Water Quality: 2018 to 2021 - collected every minute
# Continuous discharge data -- switches from every minute to every 15 minutes in 2021. 



