#### READ ME ####
# This purpose of this script is to clean, consolidate, plot, and aggregate NEON data from the TECR site
# It utilizes the "merged" datasets from the "Merged datasets" folder in Google Drive. 
# It includes date-time formatting, aggregating/rounding to 15 min intervals, changing QC-flagged data to NA, joining data into a single long dataframe
# Time periods with erratic or suspicious discharge data were changed to NA values
# Discharge data is then merged with USFS T001 data, and NEON gaps are filled with a linear model based on relationship to T001
# All NEON data is aggregated to daily values (chem and discharge) and uploaded to the GoogleDrive in the Cleaned & Merged folder

#### Libraries ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(googledrive)
library(neonUtilities)
library(plotly)


#### Download data ####
#specify google drive folder
folder_id <- "1Ja-yVniIp6-bhnYdkTp-C-ga-2ctfHhI"

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

elevation <- read.csv("csvs/all_elevation_data.csv")
fieldQ <- read.csv("csvs/all_fielddischarge_data.csv")
isotopes <- read.csv("csvs/all_isotope_data.csv")
nitrate <- read.csv("csvs/all_nitrate_data.csv")
temp <- read.csv("csvs/all_temp_data.csv")
chem <- read.csv("csvs/all_waterchem_data.csv")
wq <- read.csv("csvs/all_waterquality_data.csv")
contQ <- read.csv("csvs/all_contdischarge_data.csv")
gauge_height<- read.csv("csvs/all_gaugeheight_data.csv")





#### Date-Time Formatting ####

#Everything is in UTC's at this point

fieldQ$DateTime_UTC <- as.POSIXct(fieldQ$collectDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
isotopes$DateTime_UTC = as.POSIXct(isotopes$collectDate, format="%Y-%m-%d", tz="UTC") 
chem$DateTime_UTC <- as.POSIXct(chem$collectDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
wq$DateTime_UTC <- as.POSIXct(wq$startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC" )
contQ$DateTime_UTC<- as.POSIXct(contQ$DateTime_UTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC" )
gauge_height$DateTime_UTC <- as.POSIXct(gauge_height$startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC" )


#nitrate and temp have some date-times and some that are just dates 
# use parse_date_time() instead of as.POSIXct() to specify multiple formats
nitrate$DateTime_UTC = 
  parse_date_time(nitrate$startDateTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d" ), exact = T, tz="UTC")
temp$DateTime_UTC = 
  parse_date_time(temp$startDateTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d" ), exact = T, tz="UTC")
elevation$DateTime_UTC = 
  parse_date_time(elevation$startDateTime, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d" ), exact = T, tz="UTC")


#Convert everything to Pacific Time...should probably write a loop for this
fieldQ$DateTime_PT <- with_tz(fieldQ$DateTime_UTC, tzone = "America/Los_Angeles")
isotopes$DateTime_PT <- with_tz(isotopes$DateTime_UTC, tzone = "America/Los_Angeles")
chem$DateTime_PT <- with_tz(chem$DateTime_UTC, tzone = "America/Los_Angeles")
wq$DateTime_PT <- with_tz(wq$DateTime_UTC, tzone = "America/Los_Angeles")
contQ$DateTime_PT <- with_tz(contQ$DateTime_UTC, tzone = "America/Los_Angeles")
nitrate$DateTime_PT <- with_tz(nitrate$DateTime_UTC, tzone = "America/Los_Angeles")
temp$DateTime_PT <- with_tz(temp$DateTime_UTC, tzone = "America/Los_Angeles")
elevation$DateTime_PT <- with_tz(elevation$DateTime_UTC, tzone = "America/Los_Angeles")
gauge_height$DateTime_PT <- with_tz(gauge_height$DateTime_UTC, tzone = "America/Los_Angeles")





#### Initial Data Cleaning ####
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


#### 5) Elevation ####

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


#### 6) Field Q ####
#This data was collected weekly to monthly

#QF field seems to be full on NAs -- not helpful

#First we'll round to the nearest 15 minutes
fieldQ$DateTime_PT = round_date(fieldQ$DateTime_PT, "15 minute")

#Next, select columns of interest
fieldQ <- fieldQ %>%
  select(DateTime_PT, finalDischarge)


#### 7) Continuous Q ####
# This data was a little messy since NEON changed its sampling frequency and variables in 2021.
# Coalesce the two different discharge variables
contQ <- contQ %>%
  mutate(dischargeContinuous_merged = coalesce(maxpostDischarge, dischargeContinuous))

# Hold off on getting rid of flagged data until we plot it

# Lets set any flagged data to NA:
#contQ <- contQ %>%
# mutate(dischargeContinuous_merged = case_when(
#  dischargeFinalQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1
# TRUE ~ dischargeContinuous_merged          # otherwise keep the original value
#))

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


#### 8) Gauge Height ####

# Lets set any flagged data to NA:
gauge_height <- gauge_height %>%
  mutate(initialStageHeight = case_when(
    dataQF == 1 ~ NA_real_,   # set to NA if finalQF equals 1
    TRUE ~ initialStageHeight         # otherwise keep the original value
  ))

#Round to 15 minute measurements
gauge_height_15 <- gauge_height %>%
  mutate(DateTime_PT = floor_date(DateTime_PT, "15 minutes"))


#Next, select columns of interest
gauge_height_15 <- gauge_height_15 %>%
  select(DateTime_PT, initialStageHeight)

#### 9) Joining & pivoting to long data ####

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

df_joined <- df_joined %>%
  full_join(gauge_height_15, by = "DateTime_PT")


#Lets make sure nothing is duplicated
duplicates <- df_joined[duplicated(df_joined$DateTime_PT), ]
#there are 20 duplicates, but all the data is the same.  Lets remove the first duplicate
df_joined <- df_joined[!duplicated(df_joined[, "DateTime_PT"]), ]


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
    variable == "dischargeContinuous_merged" ~ "Liters/sec",
    variable == "initialStageHeight" ~ "meters"
  ))



#Let's make a faceted plot

df_long <- df_long %>%
  mutate(var_label = paste0(variable, " (", units, ")"))

ggplot(df_long, aes(x = DateTime_PT, y = value))+
  geom_point()+
  facet_wrap(~ var_label, scales = "free_y")+
  theme_minimal()
#yikes. too many facets




#### 10) Discharge Data Plotting ####
#Lets make sure everything is looking fine by plotting surface water elevation, field Q, and continuous Q together

#Since I left in the quality flagged data, putting a box where the data was flagged in continuous discharge

df_Q <- df_long %>%
  filter(variable == "finalDischarge" | variable == "surfacewaterElevMean" | variable == "dischargeContinuous_merged"| variable == "initialStageHeight")

ggplot(df_Q, aes(x = DateTime_PT, y = value))+
  geom_rect(
    xmin = as.POSIXct("2019-06-01"),
    xmax = as.POSIXct("2021-02-01"),
    ymin = -Inf,
    ymax = Inf,
    fill = "tomato",
    alpha = 0.01
  ) +
  geom_point()+
  facet_wrap(~ var_label, scales = "free_y", ncol = 1)+
  theme_minimal()







#We can finish by deleting the folder of csvs before pushing to Github
unlink("csvs", recursive = TRUE, force = TRUE)



#### 11) Bring in USFS data to compare ####

drive_find(n_max = 10) 

# Download the desired file to the working directory
drive_download("t003.2003.2025.discharge.15min.csv", path = "t003.2003.2025.discharge.15min.csv", overwrite = TRUE)

USFS_discharge <- read.csv("t003.2003.2025.discharge.15min.csv")

#create columns needed
USFS_discharge$lps <- USFS_discharge$T003_lps
USFS_discharge$DateTime_PT <- USFS_discharge$Date_time_PT
USFS_discharge$Stream <- "T001 USFS"

#fix datetime
USFS_discharge$DateTime_PT <- ymd_hms(USFS_discharge$DateTime_PT, tz = "America/Los_Angeles")

#select just columns needed
USFS_discharge <- USFS_discharge %>%
  select(DateTime_PT, lps, Stream)



#select continuous discharge from NEON data and get columns in shape

df_joined$lps <- df_joined$dischargeContinuous_merged

NEON_cont_discharge <- df_joined %>%
  select(DateTime_PT, lps)

NEON_cont_discharge$Stream <- "TECR NEON"



merged <- rbind(USFS_discharge, NEON_cont_discharge)



#lets cut off where the NEON data goes crazy...1500 lps seems reasonable?
#lets just look at the years where we have NEON data
merged_cleaned <- merged %>%
  filter(lps < 1500) %>%
  filter(DateTime_PT > "2018-10-01 01:00:00")


#### 12) Plotting USFS/NEON discharge data together ####

merged_plot <- ggplot(merged_cleaned, aes(DateTime_PT, lps, color = Stream)) + geom_line(size = 0.125, alpha = 0.8) +
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 7000, by = 100)) +
  scale_x_datetime(name = "Year", date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
merged_plot

#save
folder <-drive_get("T003_NEON_Discharge_Figures")

ggsave(
  "merged_plot_unclean.png",
  plot = merged_plot,
  width = 10,
  height = 5,
  dpi = 300
)

# Upload to Google Drive
drive_upload(
  media = "merged_plot_unclean.png",
  path = folder,
  name = "merged_plot_unclean.png",
  overwrite = TRUE
)


#Make plots by year to be able to see better

#make year column
merged_cleaned$year <- year(merged_cleaned$DateTime_PT)

#specify folder to upload to
folder <-drive_get("T003_NEON_Discharge_Figures")

#loop to create & upload annual discharge plots

years <- 2018:2025

for (y in years) {
  
  # Subset data
  df_year <- merged_cleaned %>% filter(year == y)
  
  # Skip if no data
  if (nrow(df_year) == 0) next
  
  # Create plot 
  p <- ggplot(df_year, aes(x = DateTime_PT, y = lps, color = Stream)) +
    geom_line(size = 0.5, alpha = 0.8) +
    labs(
      title = paste("Year:", y),
      x = "Date",
      y = "Liters/sec"
    ) +
    theme_minimal()
  
  # Save locally
  filename <- paste0("t003_neon_", y, ".png")
  
  ggsave(
    filename,
    plot = p,
    width = 10,
    height = 5,
    dpi = 300
  )
  
  # Upload to Google Drive
  drive_upload(
    media = filename,
    path = folder,
    name = filename,
    overwrite = TRUE
  )
  
  # Optional: remove local file after upload
  file.remove(filename)
}



#Lets get a rolling monthly correlation between USFS & NEON

#pivot data wide
wide <- merged %>%
  select(DateTime_PT, Stream, lps) %>%
  pivot_wider(names_from = Stream, values_from = lps)

#pull out time series that overlap
wide <- wide %>%
  filter(DateTime_PT >= as.POSIXct("2018-11-01"),
         DateTime_PT <= as.POSIXct("2023-09-30"))




library(zoo)

mat <- as.matrix(wide[, c("T001 USFS", "TECR NEON")])

wide$roll_cor <- rollapply(
  mat,
  width = 2880,
  FUN = function(x) {
    
    x <- x[complete.cases(x), , drop = FALSE]
    
    # if nothing left after removing NA
    if (length(x) == 0 || nrow(x) < 50) return(NA_real_)
    
    cor(x[,1], x[,2])
  },
  by.column = FALSE,
  align = "right",
  fill = NA
)


#Plot it!
ggplot(wide, aes(DateTime_PT, roll_cor)) +
  geom_line(color = "darkgreen", linewidth = 0.7, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(
    x = "Date",
    y = "Rolling Correlation (USFS vs NEON)"
  ) +
  theme_classic()



#### 13) More NEON Discharge Data Cleaning & Replotting ####

#Alright, now we try to clean up the NEON data a little more with this information


# Lets set any flagged data to NA, except for the areas of the data we're trying to salvage (between Oct 2020 and mid-March 2021 -- plots look, decent)

contQ_clean <- contQ %>%
  mutate(dischargeContinuous_merged = case_when(
    dischargeFinalQF == 1 & 
      !(DateTime_PT >= as.POSIXct("2020-05-01") & 
          DateTime_PT <= as.POSIXct("2021-03-15")) ~ NA_real_,
    
    TRUE ~ dischargeContinuous_merged
  ))

#get rid of unrealistic spikes in flow.  TECR never exceeds 1000 lps during this time frame.
contQ_clean <- contQ_clean %>%
  mutate(
    dischargeContinuous_merged = case_when(
      dischargeContinuous_merged > 1000 ~ NA_real_,
      TRUE ~ dischargeContinuous_merged
    ))


#get rid of any 0's or negative values.
contQ_clean <- contQ_clean %>%
  mutate(
    dischargeContinuous_merged = case_when(
      dischargeContinuous_merged <= 0 ~ NA_real_,
      TRUE ~ dischargeContinuous_merged
    ))


#lets cut out where the system seems to break in 2023, as well as this messy bit when it is first set up in 2018. We're also cutting a weird spike in Jan/Feb 2019 that is not replicated in the USFS data.
contQ_clean <- contQ_clean %>%
  mutate(dischargeContinuous_merged = case_when(
    DateTime_PT <= as.POSIXct("2018-12-10") ~ NA_real_,
    TRUE ~ dischargeContinuous_merged
  ))

contQ_clean <- contQ_clean %>%
  mutate(dischargeContinuous_merged = case_when(
    DateTime_PT >= as.POSIXct("2023-03-10") & DateTime_PT <= as.POSIXct("2023-04-01") ~ NA_real_,
    TRUE ~ dischargeContinuous_merged
  ))

contQ_clean <- contQ_clean %>%
  mutate(dischargeContinuous_merged = case_when(
    DateTime_PT >= as.POSIXct("2019-01-01") & DateTime_PT <= as.POSIXct("2019-02-12") ~ NA_real_,
    TRUE ~ dischargeContinuous_merged
  ))


#Let's also cut out the region that has both poor gauge-height NSE and poor correlation with USFS data -- something's probably up here
contQ_clean <- contQ_clean %>%
  mutate(dischargeContinuous_merged = case_when(
    DateTime_PT >= as.POSIXct("2021-05-01") & DateTime_PT <= as.POSIXct("2021-07-15") ~ NA_real_,
    TRUE ~ dischargeContinuous_merged
  ))






#Summarize all data by 15 min intervals (they sampled every minute until 2021, then switched to every 15 minutes)

contQ_clean_15 <- contQ_clean %>%
  # Step 1: round down to nearest 15 minutes
  mutate(DateTime_PT = floor_date(DateTime_PT, "15 minutes")) %>%
  
  # Step 2: group by the new 15-min timestamp.  This one removes NAs from the mean, but ask Alex if any 15 minute period with a NA should be summarized to NA. 
  group_by(DateTime_PT) %>%
  summarize(
    dischargeContinuous_merged = mean(dischargeContinuous_merged, na.rm = TRUE))


# Got a lot of NaNs by taking the mean of NAs.  Not sure it matters,but replace all NaNs with NAs for anything numeric (doing the whole dataframe breaks the datetime)
contQ_clean_15 <- contQ_clean_15 %>%
  mutate(across(where(is.numeric), ~ na_if(., NaN)))




contQ_clean_15$lps <- contQ_clean_15$dischargeContinuous_merged

contQ_clean_15 <- contQ_clean_15 %>%
  select(DateTime_PT, lps)

contQ_clean_15$Stream <- "TECR NEON"


merged_cleaned <- rbind(USFS_discharge, contQ_clean_15)



#lets just look at the years where we have NEON data
merged_cleaned <- merged_cleaned %>%
  filter(DateTime_PT > "2018-10-01 01:00:00")

merged_plot <- ggplot(merged_cleaned, aes(DateTime_PT, lps, color = Stream)) + geom_line(size = 0.5, alpha = 0.5, na.rm = TRUE) +
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 7000, by = 100)) +
  scale_x_datetime(name = "Year", date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)+
  theme_minimal()
merged_plot

#save
folder <-drive_get("T003_NEON_Discharge_Figures_Cleaned")

ggsave(
  "merged_plot_clean.png",
  plot = merged_plot,
  width = 12,
  height = 5,
  dpi = 300
)

# Upload to Google Drive
drive_upload(
  media = "merged_plot_clean.png",
  path = folder,
  name = "merged_plot_clean.png",
  overwrite = TRUE
)

#### Add NSE values to plot ####
#read in NEON regression analysis doc (NSE values)
NSE <- read.csv("https://docs.google.com/spreadsheets/d/17Nfbq686zDeYc97goKtkwnV44qK37Wluq7a7U0GViz8/export?format=csv")

NSE$month_year <- as.POSIXct(
  make_date(NSE$year, NSE$month, 1))

NSE <- NSE %>%
  select(month_year, regression_NSE, drift_status)

merged_cleaned <- merged_cleaned %>%
  mutate(month_year = floor_date(DateTime_PT, "month") %>% as.Date())

merged_NSE <- merged_cleaned %>%
  left_join(NSE, by = "month_year")


NSE_plot <- ggplot() +

  #add red lines for potential drift
  geom_vline(
    data = merged_NSE %>% filter(drift_status == "potential_drift"),
    aes(xintercept = DateTime_PT),
    color = "#ffb09c",
    alpha = 0.01
  ) +
  
  # USFS: fixed color
  geom_line(
    data = subset(merged_NSE, Stream == "T001 USFS"),
    aes(DateTime_PT, lps),
    color = "black",
    size = 0.3,
    alpha = 0.6,
    na.rm = TRUE
  ) +
  
  # NEON: colored by NSE
  geom_line(
    data = subset(merged_NSE, Stream == "TECR NEON"),
    aes(DateTime_PT, lps, color = regression_NSE),
    size = 0.3,
    alpha = 0.8,
    na.rm = TRUE
  ) +
  
  scale_color_gradient(low = "red", high = "blue", na.value = "lightgreen") +
  
  scale_y_continuous(name = "Discharge (lps)") +
  scale_x_datetime(name = "Year", date_breaks = "1 year", date_labels = "%Y") +
  
  theme_minimal()


#save
folder <-drive_get("T003_NEON_Discharge_Figures_Cleaned")

ggsave(
  "merged_plot_clean_NSE.png",
  plot = NSE_plot,
  width = 15,
  height = 5,
  dpi = 300
)

# Upload to Google Drive
drive_upload(
  media = "merged_plot_clean_NSE.png",
  path = folder,
  name = "merged_plot_clean_NSE.png",
  overwrite = TRUE
)





#Make plots by year to be able to see better

#make year column
merged_cleaned$year <- year(merged_cleaned$DateTime_PT)
#specify folder to upload to
folder <-drive_get("T003_NEON_Discharge_Figures_Cleaned")

#loop to create & upload annual discharge plots

years <- 2018:2025

for (y in years) {
  
  # Subset data
  df_year <- merged_cleaned %>% filter(year == y)
  
  # Skip if no data
  if (nrow(df_year) == 0) next
  
  # Create plot 
  p <- ggplot(df_year, aes(x = DateTime_PT, y = lps, color = Stream)) +
    geom_line(size = 0.5, alpha = 0.5) +
    labs(
      title = paste("Year:", y),
      x = "Date",
      y = "Liters/sec"
    ) +
    theme_minimal()
  
  # Save locally
  filename <- paste0("t003_neon_", y, ".png")
  
  ggsave(
    filename,
    plot = p,
    width = 10,
    height = 5,
    dpi = 300
  )
  
  # Upload to Google Drive
  drive_upload(
    media = filename,
    path = folder,
    name = filename,
    overwrite = TRUE
  )
  
  # Optional: remove local file after upload
  file.remove(filename)
}




#Check correlation


#pivot data wide
wide <- merged_cleaned %>%
  select(DateTime_PT, Stream, lps) %>%
  pivot_wider(names_from = Stream, values_from = lps)

#pull out time series that overlap
wide <- wide %>%
  filter(DateTime_PT >= as.POSIXct("2018-11-01"),
         DateTime_PT <= as.POSIXct("2023-09-30"))




library(zoo)

mat <- as.matrix(wide[, c("T001 USFS", "TECR NEON")])

wide$roll_cor <- rollapply(
  mat,
  width = 5760, #two month rolling window,
  FUN = function(x) {
    
    x <- x[complete.cases(x), , drop = FALSE]
    
    # if nothing left after removing NA
    if (length(x) == 0 || nrow(x) < 50) return(NA_real_)
    
    cor(x[,1], x[,2])
  },
  by.column = FALSE,
  align = "right",
  fill = NA
)


#Plot it!
corr.plot <- ggplot(wide, aes(DateTime_PT, roll_cor)) +
  geom_line(color = "darkgreen", linewidth = 0.7, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(
    x = "Date",
    y = "Rolling 60 Day Correlation"
  ) +
  theme_classic()


#save
folder <-drive_get("T003_NEON_Discharge_Figures_Cleaned")

ggsave(
  "correlation_plot_clean.png",
  plot = corr.plot,
  width = 12,
  height = 5,
  dpi = 300
)

# Upload to Google Drive
drive_upload(
  media = "correlation_plot_clean.png",
  path = folder,
  name = "correlation_plot_clean.png",
  overwrite = TRUE
)



#Make a combined plot
library(patchwork)

#set axes the same
x_limits <- range(merged_cleaned$DateTime_PT, na.rm = TRUE)
corr.plot <- corr.plot  + coord_cartesian(xlim = x_limits)
NSE_plot <- NSE_plot + coord_cartesian(xlim = x_limits)

combined_plot <- corr.plot / NSE_plot +
  plot_layout(heights = c(1, 2))  # top smaller than bottom

combined_plot


#save
folder <-drive_get("T003_NEON_Discharge_Figures_Cleaned")

ggsave(
  "combined_NSE_corr_plot.png",
  plot = combined_plot,
  width = 12,
  height = 5,
  dpi = 300
)

# Upload to Google Drive
drive_upload(
  media = "combined_NSE_corr_plot.png",
  path = folder,
  name = "combined_NSE_corr_plot.png",
  overwrite = TRUE
)





#### Aggregate to Daily Means ####
#now that we've sorted out cleaning the discharge data, lets get all the NEON data aggregated to daily means 

NEON.discharge.daily <- contQ_clean_15 %>%
  
  mutate(date = as_date(DateTime_PT)) %>% 
  group_by(date) %>%
  summarise(
    mean_lps = mean(lps, na.rm = TRUE),
    .groups = "drop"
  )

#change NaN to NA
NEON.discharge.daily <- NEON.discharge.daily %>%
mutate(across(where(is.numeric), ~ na_if(., NaN)))

#### Build model that predicts TECR (NEON) flow from T001 (USFS) data


#read in USFS daily aggregated data
# Download the desired file to the working directory
drive_download("t003.daily.discharge.csv", path = "t003.daily.discharge.csv", overwrite = TRUE)

USFS.discharge.daily <- read.csv("t003.daily.discharge.csv")


#first, some housekeeping to get everything in the right format and named nicely
USFS.discharge.daily$date <- as.Date(USFS.discharge.daily$date)

USFS.discharge.daily <- USFS.discharge.daily %>%
 rename(T001_lps = mean_lps)

NEON.discharge.daily <- NEON.discharge.daily %>%
  rename(TECR_lps = mean_lps)

#set date cutoffs
start <- as.Date("2018-11-15")
end <- as.Date("2025-07-09")

#Join TECR & T001 data into the same dataframe
merged_daily_discharge <- full_join(USFS.discharge.daily, NEON.discharge.daily, by = "date") %>%
  filter(date >= start, date <= end)

#filter only for the data that doesn't have NAs
model_data <- merged_daily_discharge %>%
  filter(!is.na(TECR_lps), !is.na(T001_lps))


#### Create and check models to fit relationship between TECR & T001 ####

#First, just a linear model
model <- lm(TECR_lps ~ T001_lps, data = model_data)
summary(model)

#Next, lets try the log-log model 
log_model <- lm(log(TECR_lps) ~ log(T001_lps), data = model_data)
summary(log_model)

#create predictions
merged_daily_pred <- merged_daily_discharge %>%
  mutate(
    TECR_pred = predict(model, newdata = merged_daily_discharge),
    TECR_pred_log = exp(predict(log_model, newdata = merged_daily_discharge))
  )

#create predicted values and fill gaps, using both models.
merged_daily_pred <- merged_daily_pred %>%
  mutate(
    predicted = if_else(is.na(TECR_lps) & !is.na(T001_lps),
                        "yes", "no"),
    
    TECR_filled = if_else(predicted == "yes", TECR_pred, TECR_lps),
    TECR_filled_log = if_else(predicted == "yes", TECR_pred_log, TECR_lps)
  )

merged_daily_pred <- merged_daily_pred %>%
  mutate(
    TECR_lin_plot = if_else(predicted == "yes", TECR_filled, NA_real_),
    TECR_log_plot = if_else(predicted == "yes", TECR_filled_log, NA_real_)
  )
    
  
#arrange by date in case something funky happens
merged_daily_pred <- merged_daily_pred %>%
  arrange(date)

#okay lets make a plotly plot that we zoom around in and look at how these models did
p <- ggplot(merged_daily_pred, aes(x = date)) +
  
  # T001
  geom_line(aes(y = T001_lps, color = "T001"), linewidth = 0.7, alpha = 0.7) +
  
  # observed TECR (no connections across gaps)
  geom_line(
    aes(y = if_else(predicted == "no", TECR_lps, NA_real_), color = "Observed TECR"),
    linewidth = 0.7, alpha = 0.5
  ) +
  
  # linear model (already broken by NA)
  geom_line(
    aes(y = TECR_lin_plot, color = "Linear fill"),
    linewidth = 0.7, alpha = 0.7
  ) +
  
  # log model (already broken by NA)
  geom_line(
    aes(y = TECR_log_plot, color = "Log fill"),
    linewidth = 0.7, alpha = 0.7
  ) +

   scale_color_manual(
    values = c(
      "T001" = "black",
      "Observed TECR" = "green4",
      "Linear fill" = "red3",
      "Log fill" = "orange"
    )
  )+
  
  scale_x_date( #set to monthly x axis ticks and start in late 2018 when NEON data starts
    limits = c(as.Date("2018-10-01"), NA),
    date_breaks = "1 month",
    date_labels = "%Y-%m"
  ) +
  
  
  labs(x = "Date", y = "Discharge (L/s)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

p <- ggplotly(p)
p <- config(p, scrollZoom = TRUE)



#save it as a html widget & upload to drive
library(htmlwidgets)
saveWidget(p, "model_predicted_discharge.html", selfcontained = TRUE)

drive_upload(
  "model_predicted_discharge.html",
  path = as_id("1C9UR5dXNSTNFGqfY5giEvWz3ZVE1nxYF")
)

#### Model Comparisons ####

#lets compare the linear vs log-log model

#first just take timepoints that has real values 
eval_data <- merged_daily_pred %>%
  filter(!is.na(TECR_lps))

#Check the Root Mean Square Error for each model.  The lower the number the better the model fit. The linear model is slightly better at this.
#This means the typical error in the model is about 18 L/S.  It is sensitive to flood peaks and extreme flows. 
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))

rmse_lin <- rmse(eval_data$TECR_lps, eval_data$TECR_pred)
rmse_log <- rmse(eval_data$TECR_lps, eval_data$TECR_pred_log)

#Check the NSE for each model. This is the Nash-Sutcliffe Efficiency. 1 is perfect fit, close to 0 is weak, negative is worse than the mean
#NSE mostly evaluates how well the model captures high-flow dynamics. Linear model is slightly better at this.

nse <- function(obs, pred) {
  ok <- complete.cases(obs, pred)
  obs <- obs[ok]
  pred <- pred[ok]
  
  1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
}

nse_lin <- nse(eval_data$TECR_lps, eval_data$TECR_pred)
nse_log <- nse(eval_data$TECR_lps, eval_data$TECR_pred_log)

#Next, check log NSE. This allows us to evaluate how well the model predicts lower or baseline flows

nse_logscale_lin <- nse(log(eval_data$TECR_lps),
                    log(eval_data$TECR_pred))
nse_logscale_log <- nse(log(eval_data$TECR_lps),
                    log(eval_data$TECR_pred_log))

#make labels of all these values for the plot
label_lin <- paste0(
  "Root Mean Square Error = ", round(rmse_lin, 2), "\n",
  "Nash-Sutcliffe Efficiency = ", round(nse_lin, 3), "\n",
  "log NSE (for low flows) = ", round(nse_logscale_lin, 3)
)

label_log <- paste0(
  "Root Mean Square Error = ", round(rmse_log, 2), "\n",
  "Nash-Sutcliffe Efficiency = ", round(nse_log, 3), "\n",
  "log NSE (for low flows) = ", round(nse_logscale_log, 3)
)


#next let's plot a simple little comparison plot


png("model_comparison.png", width = 1200, height = 600)

par(mfrow = c(1, 2))

# Linear model
plot(eval_data$TECR_lps, eval_data$TECR_pred,
     col = "red", pch = 16,
     xlab = "Observed", ylab = "Predicted",
     main = "Linear model")

abline(0, 1)

legend("topleft",
       legend = label_lin,
       bty = "n",
       cex = 0.8)

# Log model
plot(eval_data$TECR_lps, eval_data$TECR_pred_log,
     col = "orange", pch = 16,
     xlab = "Observed", ylab = "Predicted",
     main = "Log model")

abline(0, 1)

legend("topleft",
       legend = label_log,
       bty = "n",
       cex = 0.8)

dev.off()


#upload to googledrive
drive_upload(
  "model_comparison.png",
  path = as_id("1C9UR5dXNSTNFGqfY5giEvWz3ZVE1nxYF")
)






#### Chem Time Series Plots ####
#Okay, let's make time series plots of all the chem data!

#Get a common column to merge
merged_daily_pred$DateTime_PT <- as.POSIXct(merged_daily_pred$date, tz = "America/Los_Angeles")

#do a full merge by DateTimePT. Note, we're using the daily discharge data but still just plotting chem at 15 minutes, or whatever interval it was collected on
chem_discharge_merge <- full_join(merged_daily_pred, df_joined, by = "DateTime_PT")

#establish analytes of interest
analytes <- c("specificConductance", "dissolvedOxygen", "adj_N_mean", "surfacewaterTempMean", "pH", "chlorophyll", "turbidity", "fDOM", "d18OWater", "d2HWater", "finalDischarge")

#establish drive folder to save plots in
drive_folder <- "1LQD3xHxpFOwrU6m1H4Xkp27ch77qFgEU"


#Alrighty pop that loop in to make a bunch of figs featuring analytes + discharge data

for (analyte in analytes) {
  
  # Filter out NA values for this analyte
  plot_data_top <- chem_discharge_merge %>%
    filter(!is.na(.data[[analyte]])) %>%
    arrange(DateTime_PT)
  
  plot_data_bottom <- chem_discharge_merge %>%
    filter(!is.na(TECR_filled)) %>%
    arrange(DateTime_PT)
  
  # Top plot (the analyte of choice)
  p_top <- ggplot(plot_data_top, aes(x = DateTime_PT, y = .data[[analyte]])) +
    geom_line(color = "firebrick") +
    geom_point(size = 1, color = "firebrick") +
    labs(title = paste(analyte, "vs Discharge"), y = analyte) +
    theme_minimal()
  
  # Bottom plot (NEON discharge data (the TECR filled data using the linear model))
  p_bottom <- ggplot(plot_data_bottom, aes(x = DateTime_PT, y = TECR_filled)) +
    geom_line(color = "steelblue") +
    geom_point(aes(color = predicted), size = 1) +
    scale_color_manual(
      values = c("no" = "steelblue", "yes" = "orange"),
      name = "Predicted"
    ) +
    labs(y = "Discharge (TECR_filled)", x = "DateTime") +
    theme_minimal()
  
  # Combine to one fig that shares an x axis
  p_combined <- subplot(
    ggplotly(p_top),
    ggplotly(p_bottom),
    nrows = 2,
    shareX = TRUE
  )
  
  p_combined <- config(p_combined, scrollZoom = TRUE)
  
  html_file <- paste0(analyte, "_discharge_plot.html")
  htmlwidgets::saveWidget(p_combined, file = html_file, selfcontained = TRUE)
  
  drive_upload(
    media = html_file,
    path = as_id(drive_folder),
    name = html_file,
    overwrite = TRUE
  )
  
  #clean clean clean
  rm(p_top, p_bottom, p_combined, plot_data_top, plot_data_bottom)
  gc()
}


#### Aggregate analytes to daily & merge with discharge ####

#create date column
chem_daily <- df_joined %>%
  mutate(date = as.Date(DateTime_PT))
  

#select analyte columns of interest
chem_daily <- chem_daily %>%
  select(date, adj_N_mean, surfacewaterTempMean, specificConductance, dissolvedOxygen, pH, chlorophyll, turbidity, fDOM, d18OWater, d2HWater)

#summarise by day
chem_daily <- chem_daily %>%
  group_by(date) %>%
  summarise(
    across(everything(), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )
#make the NaN's into NAs
chem_daily <- chem_daily %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.x), NA, .x)))


#select linear-model filled discharge and other columns we want
predicted_discharge <- merged_daily_pred %>%
  select(date, TECR_filled, predicted)

#do a full join of discharge and chem data
NEON_daily <- full_join(predicted_discharge, chem_daily, by = "date")

#add in units to variable names.  Maybe this is annoying, maybe not
NEON_daily <- NEON_daily %>%
  rename(TECR_discharge_lps = TECR_filled, model_predicted = predicted, NO3_N_mg_L = adj_N_mean, SurfaceWaterTemp_C = surfacewaterTempMean, 
         specificConductance_uS_cm = specificConductance, dissolvedOxygen_mg_L = dissolvedOxygen, chlorophyll_ug_L = chlorophyll, turbidity_FNU = turbidity, 
         fDOM_QSU = fDOM, d18OWater_per_mil = d18OWater, d2HWater_per_mil = d2HWater ) 

#pull out a weird handful of days of empty data in 2018
NEON_daily <- NEON_daily %>%
  filter(date > "2018-11-14")

csv_file <- "NEON_daily_chem_discharge.csv"
write.csv(NEON_daily, file = csv_file, row.names = FALSE)

drive_upload(
  media = csv_file,
  path = as_id("1-e76PCqZ8twNHJk9yjvbUWzAEXIcP7Gd"),
  name = csv_file,
  overwrite = TRUE
)




#### CLEAN UP ####

file.remove(list.files(pattern = "\\.png$"))
file.remove(list.files(pattern = "\\.csv$"))
file.remove(list.files(pattern = "\\.html$"))

















### NEON data issues -- using the handy dandy built in Issue Log

#Enter the data product ID and search for issues affecting TECR or all sites
#issues <- getIssueLog(
 # dpID = "DP1.20048.001",
  #token = "YOUR_TOKEN_HERE")
#issues_filtered <- issues %>%
#  filter(grepl("TECR|All", locationAffected, ignore.case = TRUE))


#Results pasted below:

#Continous discharge issues
#2019-10-25 to 2021-05-04:  Pressure data appears erratic.  COVID-19 travel restrictions, wildfire closures, and winter weather limited site access, preventing sensor from being replaced and also collection 
#of field observation for validation.  Continuous stage and discharge estimates are suspect.

#Surface water elevation issues:
#2020-02-01 to 2020-05-10: 	TECR pressure transducer behaving poorly.
#2020-05-28 to 2020-07-18: TECR pressure transducer behaving poorly.
#See notes from NEON about elevation shift

#Field discharge issues: 
### reduced field visits during the pandemic (2020-2021)






###Daylights savings time:  Goes from 1:45 to 3:00 am in March,  goes from 1:45 back to 1:00 am in November. 
#I converted from UTC to PT using the with_tz() function, which handles DST automatically.  So this data will be compatible with other data collected in Pacific Time (that changes with daylights savings)
#If aggregating data to day_level means, might want to keep it in UTC to avoid the 25 hr and 23 hr day problem.


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



