#### Read me ####
# -- The following code is what was used to access salt-based discharge data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Stage discharge rating curves."

# Abstract: This data product provides parameters that describe the relationship 
# between manual staff gauge readings and manual and/or instumental discharge 
# measurements for NEON stream, river, and lake inflow and outflow locations. 
# The parameters provided are the coefficients defining an exponential curve and 
# are derived from manually measured discharge (manually or via ADCP) and staff 
# gauge readings by a Bayesian model. Rating curve parameters published in this 
# product are used together with sensor measurements of surface water pressure to 
# calculate the Continuous discharge (DP4.00130.001) data product. Data users 
# should refer to the user guide for Stage-discharge rating curve (NEON_ratingCurve_userGuide) 
# for more detailed information on the algorithm used to develop a rating curve.

## Tutorial instructions for using the NEON API can be found here: 
## https://www.neonscience.org/resources/learning-hub/tutorials/neon-api-usage

#### Libraries ####
install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("googledrive")

# activate packages
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googledrive)

#### Access data products ####
# data product name: "Stage discharge rating curves" (DP4.00133.001)
ratcurves_product <- GET("http://data.neonscience.org/api/v0/products/DP4.00133.001")
ratcurves_product

# make the data readable by jsonlite
ratcurves_text <- content(ratcurves_product, as = "text")

# flatten json into a nested list
ratcurves_avail <- jsonlite::fromJSON(ratcurves_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
ratcurves_avail$data$siteCodes

# TECR is 30th on the list
ratcurves_avail$data$siteCodes$siteCode[[24]]

# check which months at TECR have available data
ratcurves_avail$data$siteCodes$availableMonths[[24]]

# get a complete list of available data URLs
ratcurves_urls <- unlist(ratcurves_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(ratcurves_urls)

# show the first 10 URLs available
ratcurves_urls[1:10]

#### SEPTEMBER 2017 ####
ratcurves_sept2017 <- GET(ratcurves_urls[grep("TECR/2017-09", ratcurves_urls)])
ratcurves_files <- jsonlite::fromJSON(content(ratcurves_sept2017, as = "text"))
ratcurves_files$data$files$name

# look at the basic data table for rating curves
# table contains both gauge height and discharge, but not the rating curve itself
ratcurves_sept2017 <- read.csv(ratcurves_files$data$files$url
                         [grep("sdrc_gaugeDischargeMeas.2017-09.basic.",
                               ratcurves_files$data$files$name)])

# reformat Date and Time
ratcurves_sept2017$startDate <- as.POSIXct(ratcurves_sept2017$startDate, format = "%Y-%m-%dT%H:%MZ")
ratcurves_sept2017$endDate <- as.POSIXct(ratcurves_sept2017$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(ratcurves_sept2017, "ratcurves_sept2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "ratcurves_sept2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1NrlqPRD6ZvtAAo0JjeN08KUzwn7ZAzUy"),
  name = "ratcurves_sept2017.csv"
)

#### SEPTEMBER 2018 ####
ratcurves_sept2018 <- GET(ratcurves_urls[grep("TECR/2018-09", ratcurves_urls)])
ratcurves_files <- jsonlite::fromJSON(content(ratcurves_sept2018, as = "text"))
ratcurves_files$data$files$name

# look at the basic data table for rating curves
# table contains both gauge height and discharge, but not the rating curve itself
ratcurves_sept2018 <- read.csv(ratcurves_files$data$files$url
                               [grep("sdrc_gaugeDischargeMeas.2018-09.basic.",
                                     ratcurves_files$data$files$name)])

# reformat Date and Time
ratcurves_sept2018$startDate <- as.POSIXct(ratcurves_sept2018$startDate, format = "%Y-%m-%dT%H:%MZ")
ratcurves_sept2018$endDate <- as.POSIXct(ratcurves_sept2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(ratcurves_sept2018, "ratcurves_sept2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "ratcurves_sept2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1NrlqPRD6ZvtAAo0JjeN08KUzwn7ZAzUy"),
  name = "ratcurves_sept2018.csv"
)

#### SEPTEMBER 2019 ####
ratcurves_sept2019 <- GET(ratcurves_urls[grep("TECR/2019-09", ratcurves_urls)])
ratcurves_files <- jsonlite::fromJSON(content(ratcurves_sept2019, as = "text"))
ratcurves_files$data$files$name

# look at the basic data table for rating curves
# table contains both gauge height and discharge, but not the rating curve itself
ratcurves_sept2019 <- read.csv(ratcurves_files$data$files$url
                               [grep("sdrc_gaugeDischargeMeas.2019-09.basic.",
                                     ratcurves_files$data$files$name)])

# reformat Date and Time
ratcurves_sept2019$startDate <- as.POSIXct(ratcurves_sept2019$startDate, format = "%Y-%m-%dT%H:%MZ")
ratcurves_sept2019$endDate <- as.POSIXct(ratcurves_sept2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(ratcurves_sept2019, "ratcurves_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "ratcurves_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1NrlqPRD6ZvtAAo0JjeN08KUzwn7ZAzUy"),
  name = "ratcurves_sept2019.csv"
)

#### NOVEMBER 2019 ####
# no data

#### SEPTEMBER 2020 ####
ratcurves_sept2020 <- GET(ratcurves_urls[grep("TECR/2020-09", ratcurves_urls)])
ratcurves_files <- jsonlite::fromJSON(content(ratcurves_sept2020, as = "text"))
ratcurves_files$data$files$name

# look at the basic data table for rating curves
# table contains both gauge height and discharge, but not the rating curve itself
ratcurves_sept2020 <- read.csv(ratcurves_files$data$files$url
                               [grep("sdrc_gaugeDischargeMeas.2020-09.basic.",
                                     ratcurves_files$data$files$name)])

# reformat Date and Time
ratcurves_sept2020$startDate <- as.POSIXct(ratcurves_sept2020$startDate, format = "%Y-%m-%dT%H:%MZ")
ratcurves_sept2020$endDate <- as.POSIXct(ratcurves_sept2020$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(ratcurves_sept2020, "ratcurves_sept2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "ratcurves_sept2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1NrlqPRD6ZvtAAo0JjeN08KUzwn7ZAzUy"),
  name = "ratcurves_sept2020.csv"
)

#### SEPTEMBER 2021 ####
ratcurves_sept2021 <- GET(ratcurves_urls[grep("TECR/2021-09", ratcurves_urls)])
ratcurves_files <- jsonlite::fromJSON(content(ratcurves_sept2021, as = "text"))
ratcurves_files$data$files$name

# look at the basic data table for rating curves
# table contains both gauge height and discharge, but not the rating curve itself
ratcurves_sept2021 <- read.csv(ratcurves_files$data$files$url
                               [grep("sdrc_gaugeDischargeMeas.2021-09.basic.",
                                     ratcurves_files$data$files$name)])

# reformat Date and Time
ratcurves_sept2021$startDate <- as.POSIXct(ratcurves_sept2021$startDate, format = "%Y-%m-%dT%H:%MZ")
ratcurves_sept2021$endDate <- as.POSIXct(ratcurves_sept2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(ratcurves_sept2021, "ratcurves_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "ratcurves_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1NrlqPRD6ZvtAAo0JjeN08KUzwn7ZAzUy"),
  name = "ratcurves_sept2021.csv"
)

#### NOVEMBER 2021 ####
# -- no data

#### APRIL 2022 ####
ratcurves_apr2022 <- GET(ratcurves_urls[grep("TECR/2022-04", ratcurves_urls)])
ratcurves_files <- jsonlite::fromJSON(content(ratcurves_apr2022, as = "text"))
ratcurves_files$data$files$name

# look at the basic data table for rating curves
# table contains both gauge height and discharge, but not the rating curve itself
ratcurves_apr2022 <- read.csv(ratcurves_files$data$files$url
                               [grep("sdrc_gaugeDischargeMeas.2022-04.basic.",
                                     ratcurves_files$data$files$name)])

# reformat Date and Time
ratcurves_apr2022$startDate <- as.POSIXct(ratcurves_apr2022$startDate, format = "%Y-%m-%dT%H:%MZ")
ratcurves_apr2022$endDate <- as.POSIXct(ratcurves_apr2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(ratcurves_apr2022, "ratcurves_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "ratcurves_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1NrlqPRD6ZvtAAo0JjeN08KUzwn7ZAzUy"),
  name = "ratcurves_apr2022.csv"
)

#### SEPTEMBER 2022 ####
ratcurves_sept2022 <- GET(ratcurves_urls[grep("TECR/2022-09", ratcurves_urls)])
ratcurves_files <- jsonlite::fromJSON(content(ratcurves_sept2022, as = "text"))
ratcurves_files$data$files$name

# look at the basic data table for rating curves
# table contains both gauge height and discharge, but not the rating curve itself
ratcurves_sept2022 <- read.csv(ratcurves_files$data$files$url
                              [grep("sdrc_gaugeDischargeMeas.2022-09.basic.",
                                    ratcurves_files$data$files$name)])

# reformat Date and Time
ratcurves_sept2022$startDate <- as.POSIXct(ratcurves_sept2022$startDate, format = "%Y-%m-%dT%H:%MZ")
ratcurves_sept2022$endDate <- as.POSIXct(ratcurves_sept2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(ratcurves_sept2022, "ratcurves_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "ratcurves_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1NrlqPRD6ZvtAAo0JjeN08KUzwn7ZAzUy"),
  name = "ratcurves_sept2022.csv"
)

#### SEPTEMBER 2023 ####
ratcurves_sept2023 <- GET(ratcurves_urls[grep("TECR/2023-09", ratcurves_urls)])
ratcurves_files <- jsonlite::fromJSON(content(ratcurves_sept2023, as = "text"))
ratcurves_files$data$files$name

# look at the basic data table for rating curves
# table contains both gauge height and discharge, but not the rating curve itself
ratcurves_sept2023 <- read.csv(ratcurves_files$data$files$url
                               [grep("sdrc_gaugeDischargeMeas.2023-09.basic.",
                                     ratcurves_files$data$files$name)])

# reformat Date and Time
ratcurves_sept2023$startDate <- as.POSIXct(ratcurves_sept2023$startDate, format = "%Y-%m-%dT%H:%MZ")
ratcurves_sept2023$endDate <- as.POSIXct(ratcurves_sept2023$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(ratcurves_sept2023, "ratcurves_sept2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "ratcurves_sept2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1NrlqPRD6ZvtAAo0JjeN08KUzwn7ZAzUy"),
  name = "ratcurves_sept2023.csv"
)

#### Read me ####
# # -- As of 12/16/2025, all data after September 2023 were "provisional." 

#### Part 2: Read me ####
# The following code is used to combine all of the individual CSVs into one CSV,
# and to store that CSV in the same Google Drive folder for analysis.

#### Libraries ####
install.packages("googledrive")
install.packages("tidyverse")

library(googledrive)
library(tidyverse)

#### Local folders ####
# List and delete the files in these folders. These folders will be used for each dataset.
files <- list.files(path = "NEON", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#### Load data ####
# Set up Google Drive folder
ratingcurves <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1NrlqPRD6ZvtAAo0JjeN08KUzwn7ZAzUy")

# List and filter CSV files with "N" in their names
ratingcurves_files <- googledrive::drive_ls(path = ratingcurves, type = "csv")
ratingcurves_files <- ratingcurves_files[grepl("ratcurves", ratingcurves_files$name), ]

# Create an empty list to store the cleaned data frames
ratingcurves_list <- lapply(seq_along(ratingcurves_files$name), function(i) {
  googledrive::drive_download(
    file = ratingcurves_files$id[i],
    path = paste0("googledrive/", ratingcurves_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file
  read.csv(paste0("googledrive/", ratingcurves_files$name[i]), header = TRUE)
})

# Assign names to the list elements based on the file names
names(ratingcurves_list) <- ratingcurves_files$name

# Check the contents of the list
str(ratingcurves_list)

#### Combine CSVs into one dataframe ####
# Create a new dataframe for the merged files
folder <- "googledrive"

files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

combined_df <- NULL

# Loop over files
for (f in files) {
  temp <- read.csv(f, stringsAsFactors = FALSE)
  combined_df <- if (is.null(combined_df)) temp else rbind(combined_df, temp)
}

#### Write and rename the dataframe as a CSV ####
write.csv(
  combined_df,
  file = "all_ratingcurve_data.csv",
  row.names = FALSE
)

#### Upload CSV to the specific Google Drive folder ####
folder_id <- drive_get("Stage discharge rating curves")

drive_upload(
  "all_ratingcurve_data.csv",
  path = folder_id,
)
