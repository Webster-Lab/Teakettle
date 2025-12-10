#### Read me ####
# -- The following code is what was used to access gases data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Elevation of surface water." 

# Abstract: Surface water elevation is controlled by precipitation at both the 
# landscape and channel scales, overland flow, interflow, and groundwater flow. 
# It is correlated to discharge and is critical to understanding how water moves 
# through the environment, carrying nutrients and sediment, modulating aquatic 
# ecosystem structure and function. This data product contains continuous, 
# quality-controlled, surface water depth converted to elevation above mean sea level. 
# Measurements are taken once per minute and reported as 1-minute instantaneous 
# data and 5 and 30-minute averages.

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
# data product name: "Elevation of surface water" (DP1.20016.001)
elev_product <- GET("http://data.neonscience.org/api/v0/products/DP1.20016.001")
elev_product

# make the data readable by jsonlite
elev_text <- content(elev_product, as = "text")

# flatten json into a nested list
elev_avail <- jsonlite::fromJSON(elev_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
elev_avail$data$siteCodes

# TECR is 30th on the list
elev_avail$data$siteCodes$siteCode[[30]]

# check which months at TECR have available data
elev_avail$data$siteCodes$availableMonths[[30]]

# get a complete list of available data URLs
elev_urls <- unlist(elev_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(elev_urls)

# show the first 10 URLs available
elev_urls[1:10]

#### NOVEMBER 2018 ####
elev_nov2018 <- GET(elev_urls[grep("TECR/2018-11", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_nov2018, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2018-11.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_nov2018 <- read.csv(target_url)

# reformat Date and Time
elev_nov2018$startDateTime <- as.POSIXct(elev_nov2018$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_nov2018$endDateTime <- as.POSIXct(elev_nov2018$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_nov2018, "elev_nov2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_nov2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_nov2018.csv"
)

#### DECEMBER 2018 ####
elev_dec2018 <- GET(elev_urls[grep("TECR/2018-12", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_dec2018, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2018-12.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_dec2018 <- read.csv(target_url)

# reformat Date and Time
elev_dec2018$startDateTime <- as.POSIXct(elev_dec2018$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_dec2018$endDateTime <- as.POSIXct(elev_dec2018$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_dec2018, "elev_dec2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_dec2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_dec2018.csv"
)

#### JANUARY 2019 ####
elev_jan2019 <- GET(elev_urls[grep("TECR/2019-01", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_jan2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-01.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_jan2019 <- read.csv(target_url)

# reformat Date and Time
elev_jan2019$startDateTime <- as.POSIXct(elev_jan2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_jan2019$endDateTime <- as.POSIXct(elev_jan2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_jan2019, "elev_jan2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_jan2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_jan2019.csv"
)

#### FEBRUARY 2019 ####
elev_feb2019 <- GET(elev_urls[grep("TECR/2019-02", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_feb2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-02.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_feb2019 <- read.csv(target_url)

# reformat Date and Time
elev_feb2019$startDateTime <- as.POSIXct(elev_feb2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_feb2019$endDateTime <- as.POSIXct(elev_feb2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_feb2019, "elev_feb2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_feb2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_feb2019.csv"
)

#### MARCH 2019 ####
elev_mar2019 <- GET(elev_urls[grep("TECR/2019-03", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_mar2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-03.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_mar2019 <- read.csv(target_url)

# reformat Date and Time
elev_mar2019$startDateTime <- as.POSIXct(elev_mar2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_mar2019$endDateTime <- as.POSIXct(elev_mar2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_mar2019, "elev_mar2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_mar2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_mar2019.csv"
)

#### APRIL 2019 ####
elev_apr2019 <- GET(elev_urls[grep("TECR/2019-04", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_apr2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-04.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_apr2019 <- read.csv(target_url)

# reformat Date and Time
elev_apr2019$startDateTime <- as.POSIXct(elev_apr2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_apr2019$endDateTime <- as.POSIXct(elev_apr2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_apr2019, "elev_apr2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_apr2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_apr2019.csv"
)

#### MAY 2019 ####
elev_may2019 <- GET(elev_urls[grep("TECR/2019-05", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_may2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-05.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_may2019 <- read.csv(target_url)

# reformat Date and Time
elev_may2019$startDateTime <- as.POSIXct(elev_may2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_may2019$endDateTime <- as.POSIXct(elev_may2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_may2019, "elev_may2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_may2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_may2019.csv"
)

#### JUNE 2019 ####
elev_june2019 <- GET(elev_urls[grep("TECR/2019-06", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_june2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-06.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_june2019 <- read.csv(target_url)

# reformat Date and Time
elev_june2019$startDateTime <- as.POSIXct(elev_june2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_june2019$endDateTime <- as.POSIXct(elev_june2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_june2019, "elev_june2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_june2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_june2019.csv"
)

#### JULY 2019 ####
elev_july2019 <- GET(elev_urls[grep("TECR/2019-07", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_july2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-07.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_july2019 <- read.csv(target_url)

# reformat Date and Time
elev_july2019$startDateTime <- as.POSIXct(elev_july2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_july2019$endDateTime <- as.POSIXct(elev_july2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_july2019, "elev_july2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_july2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_july2019.csv"
)

#### AUGUST 2019 ####
elev_aug2019 <- GET(elev_urls[grep("TECR/2019-08", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_aug2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-08.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_aug2019 <- read.csv(target_url)

# reformat Date and Time
elev_aug2019$startDateTime <- as.POSIXct(elev_aug2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_aug2019$endDateTime <- as.POSIXct(elev_aug2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_aug2019, "elev_aug2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_aug2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_aug2019.csv"
)

#### SEPTEMBER 2019 ####
elev_sept2019 <- GET(elev_urls[grep("TECR/2019-09", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_sept2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-09.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_sept2019 <- read.csv(target_url)

# reformat Date and Time
elev_sept2019$startDateTime <- as.POSIXct(elev_sept2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_sept2019$endDateTime <- as.POSIXct(elev_sept2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_sept2019, "elev_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_sept2019.csv"
)

#### OCTOBER 2019 ####
elev_oct2019 <- GET(elev_urls[grep("TECR/2019-10", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_oct2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-10.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_oct2019 <- read.csv(target_url)

# reformat Date and Time
elev_oct2019$startDateTime <- as.POSIXct(elev_oct2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_oct2019$endDateTime <- as.POSIXct(elev_oct2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_oct2019, "elev_oct2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_oct2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_oct2019.csv"
)

#### NOVEMBER 2019 ####
elev_nov2019 <- GET(elev_urls[grep("TECR/2019-11", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_nov2019, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2019-11.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_nov2019 <- read.csv(target_url)

# reformat Date and Time
elev_nov2019$startDateTime <- as.POSIXct(elev_nov2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_nov2019$endDateTime <- as.POSIXct(elev_nov2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_nov2019, "elev_nov2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_nov2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_nov2019.csv"
)

#### DECEMBER 2019 ####
# -- no data

#### JANUARY 2020 ####
# -- no data

#### FEBRUARY 2020 ####
elev_feb2020 <- GET(elev_urls[grep("TECR/2020-02", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_feb2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-02.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_feb2020 <- read.csv(target_url)

# reformat Date and Time
elev_feb2020$startDateTime <- as.POSIXct(elev_feb2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_feb2020$endDateTime <- as.POSIXct(elev_feb2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_feb2020, "elev_feb2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_feb2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_feb2020.csv"
)

#### MARCH 2020 ####
elev_mar2020 <- GET(elev_urls[grep("TECR/2020-03", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_mar2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-03.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_mar2020 <- read.csv(target_url)

# reformat Date and Time
elev_mar2020$startDateTime <- as.POSIXct(elev_mar2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_mar2020$endDateTime <- as.POSIXct(elev_mar2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_mar2020, "elev_mar2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_mar2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_mar2020.csv"
)

#### APRIL 2020 ####
elev_apr2020 <- GET(elev_urls[grep("TECR/2020-04", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_apr2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-04.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_apr2020 <- read.csv(target_url)

# reformat Date and Time
elev_apr2020$startDateTime <- as.POSIXct(elev_apr2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_apr2020$endDateTime <- as.POSIXct(elev_apr2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_apr2020, "elev_apr2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_apr2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_apr2020.csv"
)

#### MAY 2020 ####
elev_may2020 <- GET(elev_urls[grep("TECR/2020-05", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_may2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-05.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_may2020 <- read.csv(target_url)

# reformat Date and Time
elev_may2020$startDateTime <- as.POSIXct(elev_may2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_may2020$endDateTime <- as.POSIXct(elev_may2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_may2020, "elev_may2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_may2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_may2020.csv"
)

#### JUNE 2020 ####
elev_june2020 <- GET(elev_urls[grep("TECR/2020-06", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_june2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-06.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_june2020 <- read.csv(target_url)

# reformat Date and Time
elev_june2020$startDateTime <- as.POSIXct(elev_june2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_june2020$endDateTime <- as.POSIXct(elev_june2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_june2020, "elev_june2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_june2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_june2020.csv"
)

#### JULY 2020 ####
elev_july2020 <- GET(elev_urls[grep("TECR/2020-07", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_july2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-07.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_july2020 <- read.csv(target_url)

# reformat Date and Time
elev_july2020$startDateTime <- as.POSIXct(elev_july2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_july2020$endDateTime <- as.POSIXct(elev_july2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_july2020, "elev_july2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_july2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_july2020.csv"
)

#### AUGUST 2020 ####
elev_aug2020 <- GET(elev_urls[grep("TECR/2020-08", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_aug2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-08.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_aug2020 <- read.csv(target_url)

# reformat Date and Time
elev_aug2020$startDateTime <- as.POSIXct(elev_aug2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_aug2020$endDateTime <- as.POSIXct(elev_aug2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_aug2020, "elev_aug2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_aug2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_aug2020.csv"
)

#### SEPTEMBER 2020 ####
elev_sept2020 <- GET(elev_urls[grep("TECR/2020-09", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_sept2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-09.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_sept2020 <- read.csv(target_url)

# reformat Date and Time
elev_sept2020$startDateTime <- as.POSIXct(elev_sept2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_sept2020$endDateTime <- as.POSIXct(elev_sept2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_sept2020, "elev_sept2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_sept2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_sept2020.csv"
)

#### OCTOBER 2020 ####
elev_oct2020 <- GET(elev_urls[grep("TECR/2020-10", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_oct2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-10.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_oct2020 <- read.csv(target_url)

# reformat Date and Time
elev_oct2020$startDateTime <- as.POSIXct(elev_oct2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_oct2020$endDateTime <- as.POSIXct(elev_oct2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_oct2020, "elev_oct2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_oct2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_oct2020.csv"
)

#### NOVEMBER 2020 ####
elev_nov2020 <- GET(elev_urls[grep("TECR/2020-11", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_nov2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-11.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_nov2020 <- read.csv(target_url)

# reformat Date and Time
elev_nov2020$startDateTime <- as.POSIXct(elev_nov2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_nov2020$endDateTime <- as.POSIXct(elev_nov2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_nov2020, "elev_nov2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_nov2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_nov2020.csv"
)

#### DECEMBER 2020 ####
elev_dec2020 <- GET(elev_urls[grep("TECR/2020-12", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_dec2020, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2020-12.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_dec2020 <- read.csv(target_url)

# reformat Date and Time
elev_dec2020$startDateTime <- as.POSIXct(elev_dec2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_dec2020$endDateTime <- as.POSIXct(elev_dec2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_dec2020, "elev_dec2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_dec2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_dec2020.csv"
)

#### JANUARY 2021 ####
elev_jan2021 <- GET(elev_urls[grep("TECR/2021-01", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_jan2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-01.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_jan2021 <- read.csv(target_url)

# reformat Date and Time
elev_jan2021$startDateTime <- as.POSIXct(elev_jan2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_jan2021$endDateTime <- as.POSIXct(elev_jan2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_jan2021, "elev_jan2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_jan2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_jan2021.csv"
)

#### FEBRUARY 2021 ####
elev_feb2021 <- GET(elev_urls[grep("TECR/2021-02", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_feb2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-02.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_feb2021 <- read.csv(target_url)

# reformat Date and Time
elev_feb2021$startDateTime <- as.POSIXct(elev_feb2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_feb2021$endDateTime <- as.POSIXct(elev_feb2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_feb2021, "elev_feb2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_feb2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_feb2021.csv"
)

#### MARCH 2021 ####
elev_mar2021 <- GET(elev_urls[grep("TECR/2021-03", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_mar2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-03.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_mar2021 <- read.csv(target_url)

# reformat Date and Time
elev_mar2021$startDateTime <- as.POSIXct(elev_mar2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_mar2021$endDateTime <- as.POSIXct(elev_mar2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_mar2021, "elev_mar2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_mar2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_mar2021.csv"
)

#### APRIL 2021 ####
elev_apr2021 <- GET(elev_urls[grep("TECR/2021-04", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_apr2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-04.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_apr2021 <- read.csv(target_url)

# reformat Date and Time
elev_apr2021$startDateTime <- as.POSIXct(elev_apr2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_apr2021$endDateTime <- as.POSIXct(elev_apr2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_apr2021, "elev_apr2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_apr2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_apr2021.csv"
)

#### MAY 2021 ####
elev_may2021 <- GET(elev_urls[grep("TECR/2021-05", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_may2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-05.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_may2021 <- read.csv(target_url)

# reformat Date and Time
elev_may2021$startDateTime <- as.POSIXct(elev_may2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_may2021$endDateTime <- as.POSIXct(elev_may2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_may2021, "elev_may2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_may2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_may2021.csv"
)

#### JUNE 2021 ####
elev_june2021 <- GET(elev_urls[grep("TECR/2021-06", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_june2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-06.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_june2021 <- read.csv(target_url)

# reformat Date and Time
elev_june2021$startDateTime <- as.POSIXct(elev_june2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_june2021$endDateTime <- as.POSIXct(elev_june2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_june2021, "elev_june2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_june2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_june2021.csv"
)

#### JULY 2021 ####
elev_july2021 <- GET(elev_urls[grep("TECR/2021-07", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_july2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-07.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_july2021 <- read.csv(target_url)

# reformat Date and Time
elev_july2021$startDateTime <- as.POSIXct(elev_july2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_july2021$endDateTime <- as.POSIXct(elev_july2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_july2021, "elev_july2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_july2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_july2021.csv"
)

#### AUGUST 2021 ####
elev_aug2021 <- GET(elev_urls[grep("TECR/2021-08", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_aug2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-08.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_aug2021 <- read.csv(target_url)

# reformat Date and Time
elev_aug2021$startDateTime <- as.POSIXct(elev_aug2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_aug2021$endDateTime <- as.POSIXct(elev_aug2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_aug2021, "elev_aug2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_aug2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_aug2021.csv"
)

#### SEPTEMBER 2021 ####
elev_sept2021 <- GET(elev_urls[grep("TECR/2021-09", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_sept2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-09.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_sept2021 <- read.csv(target_url)

# reformat Date and Time
elev_sept2021$startDateTime <- as.POSIXct(elev_sept2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_sept2021$endDateTime <- as.POSIXct(elev_sept2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_sept2021, "elev_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_sept2021.csv"
)

#### OCTOBER 2021 ####
elev_oct2021 <- GET(elev_urls[grep("TECR/2021-10", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_oct2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-10.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_oct2021 <- read.csv(target_url)

# reformat Date and Time
elev_oct2021$startDateTime <- as.POSIXct(elev_oct2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_oct2021$endDateTime <- as.POSIXct(elev_oct2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_oct2021, "elev_oct2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_oct2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_oct2021.csv"
)

#### NOVEMBER 2021 ####
elev_nov2021 <- GET(elev_urls[grep("TECR/2021-11", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_nov2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-11.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_nov2021 <- read.csv(target_url)

# reformat Date and Time
elev_nov2021$startDateTime <- as.POSIXct(elev_nov2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_nov2021$endDateTime <- as.POSIXct(elev_nov2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_nov2021, "elev_nov2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_nov2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_nov2021.csv"
)

#### DECEMBER 2021 ####
elev_dec2021 <- GET(elev_urls[grep("TECR/2021-12", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_dec2021, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2021-12.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_dec2021 <- read.csv(target_url)

# reformat Date and Time
elev_dec2021$startDateTime <- as.POSIXct(elev_dec2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_dec2021$endDateTime <- as.POSIXct(elev_dec2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_dec2021, "elev_dec2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_dec2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_dec2021.csv"
)

#### JANUARY 2022 ####
elev_jan2022 <- GET(elev_urls[grep("TECR/2022-01", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_jan2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-01.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_jan2022 <- read.csv(target_url)

# reformat Date and Time
elev_jan2022$startDateTime <- as.POSIXct(elev_jan2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_jan2022$endDateTime <- as.POSIXct(elev_jan2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_jan2022, "elev_jan2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_jan2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_jan2022.csv"
)

#### FEBRUARY 2022 ####
elev_feb2022 <- GET(elev_urls[grep("TECR/2022-02", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_feb2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-02.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_feb2022 <- read.csv(target_url)

# reformat Date and Time
elev_feb2022$startDateTime <- as.POSIXct(elev_feb2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_feb2022$endDateTime <- as.POSIXct(elev_feb2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_feb2022, "elev_feb2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_feb2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_feb2022.csv"
)

#### MARCH 2022 ####
elev_mar2022 <- GET(elev_urls[grep("TECR/2022-03", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_mar2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-03.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_mar2022 <- read.csv(target_url)

# reformat Date and Time
elev_mar2022$startDateTime <- as.POSIXct(elev_mar2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_mar2022$endDateTime <- as.POSIXct(elev_mar2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_mar2022, "elev_mar2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_mar2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_mar2022.csv"
)

#### APRIL 2022 ####
elev_apr2022 <- GET(elev_urls[grep("TECR/2022-04", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_apr2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-04.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_apr2022 <- read.csv(target_url)

# reformat Date and Time
elev_apr2022$startDateTime <- as.POSIXct(elev_apr2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_apr2022$endDateTime <- as.POSIXct(elev_apr2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_apr2022, "elev_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_apr2022.csv"
)

#### MAY 2022 ####
elev_may2022 <- GET(elev_urls[grep("TECR/2022-05", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_may2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-05.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_may2022 <- read.csv(target_url)

# reformat Date and Time
elev_may2022$startDateTime <- as.POSIXct(elev_may2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_may2022$endDateTime <- as.POSIXct(elev_may2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_may2022, "elev_may2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_may2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_may2022.csv"
)

#### JUNE 2022 ####
elev_june2022 <- GET(elev_urls[grep("TECR/2022-06", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_june2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-06.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_june2022 <- read.csv(target_url)

# reformat Date and Time
elev_june2022$startDateTime <- as.POSIXct(elev_june2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_june2022$endDateTime <- as.POSIXct(elev_june2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_june2022, "elev_june2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_june2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_june2022.csv"
)

#### JULY 2022 ####
elev_july2022 <- GET(elev_urls[grep("TECR/2022-07", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_july2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-07.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_july2022 <- read.csv(target_url)

# reformat Date and Time
elev_july2022$startDateTime <- as.POSIXct(elev_july2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_july2022$endDateTime <- as.POSIXct(elev_july2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_july2022, "elev_july2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_july2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_july2022.csv"
)

#### AUGUST 2022 ####
elev_aug2022 <- GET(elev_urls[grep("TECR/2022-08", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_aug2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-08.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_aug2022 <- read.csv(target_url)

# reformat Date and Time
elev_aug2022$startDateTime <- as.POSIXct(elev_aug2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_aug2022$endDateTime <- as.POSIXct(elev_aug2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_aug2022, "elev_aug2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_aug2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_aug2022.csv"
)

#### SEPTEMBER 2022 ####
elev_sept2022 <- GET(elev_urls[grep("TECR/2022-09", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_sept2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-09.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_sept2022 <- read.csv(target_url)

# reformat Date and Time
elev_sept2022$startDateTime <- as.POSIXct(elev_sept2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_sept2022$endDateTime <- as.POSIXct(elev_sept2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_sept2022, "elev_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_sept2022.csv"
)

#### OCTOBER 2022 ####
elev_oct2022 <- GET(elev_urls[grep("TECR/2022-10", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_oct2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-10.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_oct2022 <- read.csv(target_url)

# reformat Date and Time
elev_oct2022$startDateTime <- as.POSIXct(elev_oct2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_oct2022$endDateTime <- as.POSIXct(elev_oct2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_oct2022, "elev_oct2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_oct2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_oct2022.csv"
)

#### NOVEMBER 2022 ####
elev_nov2022 <- GET(elev_urls[grep("TECR/2022-11", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_nov2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-11.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_nov2022 <- read.csv(target_url)

# reformat Date and Time
elev_nov2022$startDateTime <- as.POSIXct(elev_nov2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_nov2022$endDateTime <- as.POSIXct(elev_nov2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_nov2022, "elev_nov2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_nov2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_nov2022.csv"
)

#### DECEMBER 2022 ####
elev_dec2022 <- GET(elev_urls[grep("TECR/2022-12", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_dec2022, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2022-12.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_dec2022 <- read.csv(target_url)

# reformat Date and Time
elev_dec2022$startDateTime <- as.POSIXct(elev_dec2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_dec2022$endDateTime <- as.POSIXct(elev_dec2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_dec2022, "elev_dec2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_dec2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_dec2022.csv"
)

#### JANUARY 2023 ####
elev_jan2023 <- GET(elev_urls[grep("TECR/2023-01", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_jan2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-01.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_jan2023 <- read.csv(target_url)

# reformat Date and Time
elev_jan2023$startDateTime <- as.POSIXct(elev_jan2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_jan2023$endDateTime <- as.POSIXct(elev_jan2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_jan2023, "elev_jan2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_jan2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_jan2023.csv"
)

#### FEBRUARY 2023 ####
elev_feb2023 <- GET(elev_urls[grep("TECR/2023-02", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_feb2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-02.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_feb2023 <- read.csv(target_url)

# reformat Date and Time
elev_feb2023$startDateTime <- as.POSIXct(elev_feb2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_feb2023$endDateTime <- as.POSIXct(elev_feb2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_feb2023, "elev_feb2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_feb2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_feb2023.csv"
)

#### MARCH 2023 ####
elev_mar2023 <- GET(elev_urls[grep("TECR/2023-03", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_mar2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-03.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_mar2023 <- read.csv(target_url)

# reformat Date and Time
elev_mar2023$startDateTime <- as.POSIXct(elev_mar2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_mar2023$endDateTime <- as.POSIXct(elev_mar2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_mar2023, "elev_mar2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_mar2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_mar2023.csv"
)

#### APRIL 2023 ####
elev_apr2023 <- GET(elev_urls[grep("TECR/2023-04", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_apr2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-04.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_apr2023 <- read.csv(target_url)

# reformat Date and Time
elev_apr2023$startDateTime <- as.POSIXct(elev_apr2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_apr2023$endDateTime <- as.POSIXct(elev_apr2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_apr2023, "elev_apr2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_apr2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_apr2023.csv"
)

#### MAY 2023 ####
elev_may2023 <- GET(elev_urls[grep("TECR/2023-05", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_may2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-05.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_may2023 <- read.csv(target_url)

# reformat Date and Time
elev_may2023$startDateTime <- as.POSIXct(elev_may2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_may2023$endDateTime <- as.POSIXct(elev_may2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_may2023, "elev_may2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_may2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_may2023.csv"
)

#### JUNE 2023 ####
elev_june2023 <- GET(elev_urls[grep("TECR/2023-06", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_june2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-06.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_june2023 <- read.csv(target_url)

# reformat Date and Time
elev_june2023$startDateTime <- as.POSIXct(elev_june2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_june2023$endDateTime <- as.POSIXct(elev_june2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_june2023, "elev_june2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_june2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_june2023.csv"
)

#### JULY 2023 ####
elev_july2023 <- GET(elev_urls[grep("TECR/2023-07", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_july2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-07.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_july2023 <- read.csv(target_url)

# reformat Date and Time
elev_july2023$startDateTime <- as.POSIXct(elev_july2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_july2023$endDateTime <- as.POSIXct(elev_july2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_july2023, "elev_july2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_july2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_july2023.csv"
)

#### AUGUST 2023 ####
elev_aug2023 <- GET(elev_urls[grep("TECR/2023-08", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_aug2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-08.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_aug2023 <- read.csv(target_url)

# reformat Date and Time
elev_aug2023$startDateTime <- as.POSIXct(elev_aug2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_aug2023$endDateTime <- as.POSIXct(elev_aug2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_aug2023, "elev_aug2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_aug2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_aug2023.csv"
)

#### SEPTEMBER 2023 ####
elev_sept2023 <- GET(elev_urls[grep("TECR/2023-09", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_sept2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-09.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_sept2023 <- read.csv(target_url)

# reformat Date and Time
elev_sept2023$startDateTime <- as.POSIXct(elev_sept2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_sept2023$endDateTime <- as.POSIXct(elev_sept2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_sept2023, "elev_sept2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_sept2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_sept2023.csv"
)

#### OCTOBER 2023 ####
elev_oct2023 <- GET(elev_urls[grep("TECR/2023-10", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_oct2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-10.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_oct2023 <- read.csv(target_url)

# reformat Date and Time
elev_oct2023$startDateTime <- as.POSIXct(elev_oct2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_oct2023$endDateTime <- as.POSIXct(elev_oct2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_oct2023, "elev_oct2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_oct2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_oct2023.csv"
)

#### NOVEMBER 2023 ####
elev_nov2023 <- GET(elev_urls[grep("TECR/2023-11", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_nov2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-11.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_nov2023 <- read.csv(target_url)

# reformat Date and Time
elev_nov2023$startDateTime <- as.POSIXct(elev_nov2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_nov2023$endDateTime <- as.POSIXct(elev_nov2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_nov2023, "elev_nov2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_nov2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_nov2023.csv"
)

#### DECEMBER 2023 ####
elev_dec2023 <- GET(elev_urls[grep("TECR/2023-12", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_dec2023, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2023-12.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_dec2023 <- read.csv(target_url)

# reformat Date and Time
elev_dec2023$startDateTime <- as.POSIXct(elev_dec2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_dec2023$endDateTime <- as.POSIXct(elev_dec2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_dec2023, "elev_dec2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_dec2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_dec2023.csv"
)

#### JANUARY 2024 ####
elev_jan2024 <- GET(elev_urls[grep("TECR/2024-01", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_jan2024, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2024-01.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_jan2024 <- read.csv(target_url)

# reformat Date and Time
elev_jan2024$startDateTime <- as.POSIXct(elev_jan2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_jan2024$endDateTime <- as.POSIXct(elev_jan2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_jan2024, "elev_jan2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_jan2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_jan2024.csv"
)

#### FEBRUARY 2024 ####
elev_feb2024 <- GET(elev_urls[grep("TECR/2024-02", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_feb2024, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2024-02.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_feb2024 <- read.csv(target_url)

# reformat Date and Time
elev_feb2024$startDateTime <- as.POSIXct(elev_feb2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_feb2024$endDateTime <- as.POSIXct(elev_feb2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_feb2024, "elev_feb2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_feb2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_feb2024.csv"
)

#### MARCH 2024 ####
elev_mar2024 <- GET(elev_urls[grep("TECR/2024-03", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_mar2024, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2024-03.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_mar2024 <- read.csv(target_url)

# reformat Date and Time
elev_mar2024$startDateTime <- as.POSIXct(elev_mar2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_mar2024$endDateTime <- as.POSIXct(elev_mar2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_mar2024, "elev_mar2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_mar2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_mar2024.csv"
)

#### APRIL 2024 ####
elev_apr2024 <- GET(elev_urls[grep("TECR/2024-04", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_apr2024, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2024-04.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_apr2024 <- read.csv(target_url)

# reformat Date and Time
elev_apr2024$startDateTime <- as.POSIXct(elev_apr2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_apr2024$endDateTime <- as.POSIXct(elev_apr2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_apr2024, "elev_apr2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_apr2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_apr2024.csv"
)

#### MAY 2024 ####
elev_may2024 <- GET(elev_urls[grep("TECR/2024-05", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_may2024, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2024-05.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_may2024 <- read.csv(target_url)

# reformat Date and Time
elev_may2024$startDateTime <- as.POSIXct(elev_may2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_may2024$endDateTime <- as.POSIXct(elev_may2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_may2024, "elev_may2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_may2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_may2024.csv"
)

#### JUNE 2024 ####
elev_june2024 <- GET(elev_urls[grep("TECR/2024-06", elev_urls)])
elev_files <- jsonlite::fromJSON(content(elev_june2024, as = "text"))
elev_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "EOS_5_min.2024-06.basic"

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(elev_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- elev_files$data$files$url[match_index][1]

# read the CSV
elev_june2024 <- read.csv(target_url)

# reformat Date and Time
elev_june2024$startDateTime <- as.POSIXct(elev_june2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
elev_june2024$endDateTime <- as.POSIXct(elev_june2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(elev_june2024, "elev_june2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "elev_june2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1FFVt_SAe5ZsDeMbR265PIkVAkHMyJ4tj"),
  name = "elev_june2024.csv"
)

#### Read me ####
# # -- As of 12/10/2025, all data after June 2024 were "provisional." 