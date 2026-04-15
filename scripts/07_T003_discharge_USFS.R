#### Read me ####
# The following code is for compiling and cleaning discharge data from the USFS T003 monitoring site
# The original files are pulled from the googledrive 'KREW stage data all years' folder uploaded by Kevin Mazzocco
#
#### Libraries #### 
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("googledrive")
library(dplyr)
library(tidyr)
library(ggplot2)
library(googledrive) # Make sure Google Drive authentication is updated, otherwise the connection won't work.
library(lubridate)
library(readr)

#### Load data ####
drive_find(n_max = 10) 

# Download the desired file to the working directory
drive_download("USFS_streamsDischarge.csv", path = "USFS_streamsDischarge.csv", overwrite = TRUE)

#This is the data from 2003-2015
usfs_q <- read.csv("USFS_streamsDischarge.csv")
#the rest of the data is in a few chunks, listed here
t003.2016 <- read.csv("https://docs.google.com/spreadsheets/d/1eWU5xuyNXEBE_SatVPHtq-EsZr41vr77/export?format=csv")
data.2017.2019 <- read.csv("https://docs.google.com/spreadsheets/d/1KnlTEVvUwJVlBV8pbgIGt0A2Ttjg0VD1/export?format=csv")
data.2020.2025<-read.csv("https://docs.google.com/spreadsheets/d/1Oo-r9He1JG4c66ZRZuYOJgAmpa2NxLU4/export?format=csv")


####Select data of interest ####

# Create a dataset for T003 specifically
t003 <- usfs_q[,c(1,13)]
t003.2017.2019 <- data.2017.2019[,c(1,12)]
t003.2020.2025 <- data.2020.2025[,c(1,12)]

#The 2016 data is a little bit different, need to update some names
t003.2016$T003_lps <- t003.2016$lps
t003.2016$Date_time <- t003.2016$DateTime
t003.2016 <- t003.2016[,c(4,3)]

#### Time/Date Formatting ####

#Fix date/time formatting.  This data is in Pacific Time, but never switches for daylights savings.  Because of this, we'll specify the UTC it's in, ETC/GMT+8
t003$Date_time_PT <- as.POSIXct(t003$Date_time, format = "%m/%d/%y %H:%M", tz = "Etc/GMT+8")
t003.2016$Date_time_PT <- as.POSIXct(t003.2016$Date_time, format = "%m/%d/%y %H:%M", tz = "Etc/GMT+8")
t003.2017.2019$Date_time_PT<- as.POSIXct(t003.2017.2019$Date_time, format = "%m/%d/%y %H:%M", tz = "Etc/GMT+8")
t003.2020.2025$Date_time_PT <- as.POSIXct(t003.2020.2025$Date_time, format = "%m/%d/%y %H:%M", tz = "Etc/GMT+8")

#Now actually make it in Pacific Time. This will make the time as read on a clock throughout the year. 
t003$Date_time_PT<-with_tz(t003$Date_time_PT, tzone = "America/Los_Angeles")
t003.2016$Date_time_PT<-with_tz(t003.2016$Date_time_PT, tzone = "America/Los_Angeles")
t003.2017.2019$Date_time_PT<-with_tz(t003.2017.2019$Date_time_PT, tzone = "America/Los_Angeles")
t003.2020.2025$Date_time_PT<-with_tz(t003.2020.2025$Date_time_PT, tzone = "America/Los_Angeles")


#okay we have October 2016 duplicated in two datasets, let's pull it out of the 2017 data
t003.2017.2019 <- t003.2017.2019 %>%
  filter(Date_time_PT >= as.POSIXct("2016-11-01 01:00:00", tz = "America/Los_Angeles") )

#We also have one duplicated timestamp in October 2019, so lets toss that
t003.2020.2025 <- t003.2020.2025 %>%
  filter(Date_time_PT > as.POSIXct("2019-10-01 01:00:00", tz = "America/Los_Angeles"))


#### Merging data ####
#bind the four datasets together
t003.2003.2025 <- rbind(t003, t003.2016, t003.2017.2019, t003.2020.2025)

#just keep what we want
t003.2003.2025 <- t003.2003.2025[,c(3,2)]


#Lets make sure nothing is duplicated
duplicates <- t003.2003.2025[duplicated(t003.2003.2025$Date_time_PT), ]


#And let's make sure nothing is missing
full_seq <- seq(
  from = min(t003.2003.2025$Date_time_PT),
  to   = max(t003.2003.2025$Date_time_PT),
  by   = "15 mins"
)

missing_times <- setdiff(full_seq, t003.2003.2025$Date_time_PT)
missing_times

#alrighty we're good to go! No duplicated or missing timestamps from 2003-2025


#upload this cleaned and compiled version to google drive
folder <- drive_get("KREW_compiled_cleaned_data")

tmp <- tempfile(fileext = ".csv")
write_csv(t003.2020.2025, tmp)

drive_upload(
  media = tmp,
  path = folder,
  name = "t003.2003.2025.discharge.csv",
  overwrite = TRUE
)



#### Plot entire dataset ####
t003_plot <- ggplot(t003.2003.2025, aes(Date_time_PT, T003_lps)) + geom_point(size = 0.125) +
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 1500, by = 100)) +
  scale_x_datetime(name = "Year", date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_plot

#upload this full plot to the google drive
ggsave(
  filename = "t003_2003_2025.png",
  plot = t003_plot,
  width = 12,
  height = 6,
  dpi = 300)

drive_upload(
  media = "t003_2003_2025.png",
  path = as_id("1y3iIvqrsBc_-CSB00OI9tvPNDTtxOsZO")
)





#### Plot each year individually ####

#make year column
t003.2003.2025$year <- year(t003.2003.2025$Date_time_PT)

#specify folder to upload to
folder <-drive_get("T003_Discharge_Figures")

#loop to create & upload annual discharge plots

years <- 2003:2025

for (y in years) {
  
  # Subset data
  df_year <- t003.2003.2025 %>% filter(year == y)
  
  # Skip if no data
  if (nrow(df_year) == 0) next
  
  # Create plot (edit y variable as needed)
  p <- ggplot(df_year, aes(x = Date_time_PT, y = T003_lps)) +
    geom_point(size = 0.2) +
    labs(
      title = paste("Year:", y),
      x = "Date",
      y = "Liters/sec"
    ) +
    theme_minimal()
  
  # Save locally
  filename <- paste0("t003_", y, ".png")
  
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





















#old code 

# 2003
t003_03 <- t003[1:8832,]

t003_03_plot <- ggplot(t003_03, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 130, by = 10)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_03_plot

# 2004
t003_04 <- t003[8833:43968,]

t003_04_plot <- ggplot(t003_04, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 240, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_04_plot

# 2005
t003_05 <- t003[43969:79008,]

t003_05_plot <- ggplot(t003_05, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 700, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_05_plot

# 2006
t003_06 <- t003[79009:114048,]

t003_06_plot <- ggplot(t003_06, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 520, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_06_plot

# 2007
t003_07 <- t003[114049:149088,]

t003_07_plot <- ggplot(t003_07, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 80, by = 5)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_07_plot

# 2008
t003_08 <- t003[149089:184224,]

t003_08_plot <- ggplot(t003_08, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 140, by = 10)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_08_plot

# 2009
t003_09 <- t003[184225:219264,]

t003_09_plot <- ggplot(t003_09, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 800, by = 50)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_09_plot

# 2010
t003_10 <- t003[219265:254304,]

t003_10_plot <- ggplot(t003_10, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 400, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_10_plot

# 2011
t003_11 <- t003[254305:289344,]

t003_11_plot <- ggplot(t003_11, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 500, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_11_plot

# 2012
t003_12 <- t003[289345:324480,]

t003_12_plot <- ggplot(t003_12, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 400, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_12_plot

# 2013
t003_13 <- t003[324481:359520,]

t003_13_plot <- ggplot(t003_13, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 100, by = 10)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_13_plot

# 2014
t003_14 <- t003[359521:394560,]

t003_14_plot <- ggplot(t003_14, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 80, by = 10)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_14_plot

# 2015
t003_15 <- t003[394561:420768,]

t003_15_plot <- ggplot(t003_15, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 180, by = 10)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_15_plot       

#### Save plots to Google Drive
plots <- list(t003_plot, t003_03_plot, t003_04_plot, t003_05_plot, t003_06_plot, t003_07_plot,
              t003_08_plot, t003_09_plot, t003_10_plot, t003_11_plot, t003_12_plot, t003_13_plot,
              t003_14_plot, t003_15_plot)

out_dir <- "plots"
dir.create(out_dir, showWarnings = FALSE)

# save to local folder
for (i in seq_along(plots)) {
  ggsave(
    filename = file.path(out_dir, paste0("plot_", i, ".png")),
    plot = plots[[i]],
    width = 8,
    height = 6,
    dpi = 300
  )
}

# get list of all files in your local folder
plot_files <- list.files("plots", full.names = TRUE)

# upload all of them
lapply(plot_files, function(f) {
  drive_upload(
    media = f,
    path = "https://drive.google.com/drive/u/0/folders/196iS6zSlwD81tlrSSNPkXF6Ie9r0Go_3",  # Google Drive folder
    name = basename(f)
  )
})

# clear the local folder we used to it can be used elsewhere
files <- list.files(path = "plot", full.names = TRUE)
file.remove(files)