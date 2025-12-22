#### Read me ####
# -- The following code is what was used to access salt-based discharge data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Temperature (digital thermistor) of surface water."

# Abstract: Temperature in surface water is measured using a digital thermistor 
# at all NEON lake littoral locations, rivers, and wadeable stream sites. 
# This data product contains continuous, quality-controlled, surface water temperature. 
# Measurements are recorded once per minute and reported as 5-minute and 30-minute averages.

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
# data product name: "Temperature (digital thermistor) of surface water" (DP1.20054.001)
temps_product <- GET("http://data.neonscience.org/api/v0/products/DP1.20054.001")
temps_product

# make the data readable by jsonlite
temps_text <- content(temps_product, as = "text")

# flatten json into a nested list
temps_avail <- jsonlite::fromJSON(temps_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
temps_avail$data$siteCodes

# TECR is 30th on the list
temps_avail$data$siteCodes$siteCode[[30]]

# check which months at TECR have available data
temps_avail$data$siteCodes$availableMonths[[30]]

# get a complete list of available data URLs
temps_urls <- unlist(temps_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(temps_urls)

# show the first 10 URLs available
temps_urls[1:10]

#### NOVEMBER 2018 ####
temps_nov2018 <- GET(temps_urls[grep("TECR/2018-11", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_nov2018, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2018-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_nov2018 <- read.csv(target_url)

# reformat Date and Time
temps_nov2018$startDateTime <- as.POSIXct(temps_nov2018$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_nov2018$endDateTime <- as.POSIXct(temps_nov2018$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_nov2018, "temps_nov2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_nov2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_nov2018.csv"
)

#### DECEMBER 2018 ####
temps_dec2018 <- GET(temps_urls[grep("TECR/2018-12", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_dec2018, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2018-12.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_dec2018 <- read.csv(target_url)

# reformat Date and Time
temps_dec2018$startDateTime <- as.POSIXct(temps_dec2018$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_dec2018$endDateTime <- as.POSIXct(temps_dec2018$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_dec2018, "temps_dec2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_dec2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_dec2018.csv"
)

#### JANUARY 2019 ####
temps_jan2019 <- GET(temps_urls[grep("TECR/2019-01", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_jan2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_jan2019 <- read.csv(target_url)

# reformat Date and Time
temps_jan2019$startDateTime <- as.POSIXct(temps_jan2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_jan2019$endDateTime <- as.POSIXct(temps_jan2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_jan2019, "temps_jan2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_jan2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_jan2019.csv"
)

#### FEBRUARY 2019 ####
temps_feb2019 <- GET(temps_urls[grep("TECR/2019-02", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_feb2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_feb2019 <- read.csv(target_url)

# reformat Date and Time
temps_feb2019$startDateTime <- as.POSIXct(temps_feb2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_feb2019$endDateTime <- as.POSIXct(temps_feb2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_feb2019, "temps_feb2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_feb2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_feb2019.csv"
)

#### MARCH 2019 ####
temps_mar2019 <- GET(temps_urls[grep("TECR/2019-03", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_mar2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_mar2019 <- read.csv(target_url)

# reformat Date and Time
temps_mar2019$startDateTime <- as.POSIXct(temps_mar2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_mar2019$endDateTime <- as.POSIXct(temps_mar2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_mar2019, "temps_mar2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_mar2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_mar2019.csv"
)

#### APRIL 2019 ####
temps_apr2019 <- GET(temps_urls[grep("TECR/2019-04", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_apr2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_apr2019 <- read.csv(target_url)

# reformat Date and Time
temps_apr2019$startDateTime <- as.POSIXct(temps_apr2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_apr2019$endDateTime <- as.POSIXct(temps_apr2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_apr2019, "temps_apr2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_apr2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_apr2019.csv"
)

#### MAY 2019 ####
temps_may2019 <- GET(temps_urls[grep("TECR/2019-05", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_may2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_may2019 <- read.csv(target_url)

# reformat Date and Time
temps_may2019$startDateTime <- as.POSIXct(temps_may2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_may2019$endDateTime <- as.POSIXct(temps_may2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_may2019, "temps_may2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_may2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_may2019.csv"
)

#### JUNE 2019 ####
temps_june2019 <- GET(temps_urls[grep("TECR/2019-06", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_june2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_june2019 <- read.csv(target_url)

# reformat Date and Time
temps_june2019$startDateTime <- as.POSIXct(temps_june2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_june2019$endDateTime <- as.POSIXct(temps_june2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_june2019, "temps_june2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_june2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_june2019.csv"
)

#### JULY 2019 ####
temps_july2019 <- GET(temps_urls[grep("TECR/2019-07", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_july2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-07.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_july2019 <- read.csv(target_url)

# reformat Date and Time
temps_july2019$startDateTime <- as.POSIXct(temps_july2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_july2019$endDateTime <- as.POSIXct(temps_july2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_july2019, "temps_july2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_july2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_july2019.csv"
)

#### AUGUST 2019 ####
temps_aug2019 <- GET(temps_urls[grep("TECR/2019-08", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_aug2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-08.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_aug2019 <- read.csv(target_url)

# reformat Date and Time
temps_aug2019$startDateTime <- as.POSIXct(temps_aug2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_aug2019$endDateTime <- as.POSIXct(temps_aug2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_aug2019, "temps_aug2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_aug2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_aug2019.csv"
)

#### SEPTEMBER 2019 ####
temps_sept2019 <- GET(temps_urls[grep("TECR/2019-09", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_sept2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_sept2019 <- read.csv(target_url)

# reformat Date and Time
temps_sept2019$startDateTime <- as.POSIXct(temps_sept2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_sept2019$endDateTime <- as.POSIXct(temps_sept2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_sept2019, "temps_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_sept2019.csv"
)

#### OCTOBER 2019 ####
temps_oct2019 <- GET(temps_urls[grep("TECR/2019-10", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_oct2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-10.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_oct2019 <- read.csv(target_url)

# reformat Date and Time
temps_oct2019$startDateTime <- as.POSIXct(temps_oct2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_oct2019$endDateTime <- as.POSIXct(temps_oct2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_oct2019, "temps_oct2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_oct2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_oct2019.csv"
)

#### NOVEMBER 2019 ####
temps_nov2019 <- GET(temps_urls[grep("TECR/2019-11", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_nov2019, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2019-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_nov2019 <- read.csv(target_url)

# reformat Date and Time
temps_nov2019$startDateTime <- as.POSIXct(temps_nov2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_nov2019$endDateTime <- as.POSIXct(temps_nov2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_nov2019, "temps_nov2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_nov2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_nov2019.csv"
)

#### DECEMBER 2019 ####
# -- no data

#### JANUARY 2020 ####
# -- no data

#### FEBRUARY 2020 ####
temps_feb2020 <- GET(temps_urls[grep("TECR/2020-02", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_feb2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_feb2020 <- read.csv(target_url)

# reformat Date and Time
temps_feb2020$startDateTime <- as.POSIXct(temps_feb2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_feb2020$endDateTime <- as.POSIXct(temps_feb2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_feb2020, "temps_feb2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_feb2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_feb2020.csv"
)

#### MARCH 2020 ####
temps_mar2020 <- GET(temps_urls[grep("TECR/2020-03", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_mar2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_mar2020 <- read.csv(target_url)

# reformat Date and Time
temps_mar2020$startDateTime <- as.POSIXct(temps_mar2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_mar2020$endDateTime <- as.POSIXct(temps_mar2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_mar2020, "temps_mar2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_mar2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_mar2020.csv"
)

#### APRIL 2020 ####
temps_apr2020 <- GET(temps_urls[grep("TECR/2020-04", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_apr2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_apr2020 <- read.csv(target_url)

# reformat Date and Time
temps_apr2020$startDateTime <- as.POSIXct(temps_apr2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_apr2020$endDateTime <- as.POSIXct(temps_apr2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_apr2020, "temps_apr2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_apr2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_apr2020.csv"
)

#### MAY 2020 ####
temps_may2020 <- GET(temps_urls[grep("TECR/2020-05", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_may2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_may2020 <- read.csv(target_url)

# reformat Date and Time
temps_may2020$startDateTime <- as.POSIXct(temps_may2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_may2020$endDateTime <- as.POSIXct(temps_may2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_may2020, "temps_may2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_may2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_may2020.csv"
)

#### JUNE 2020 ####
temps_june2020 <- GET(temps_urls[grep("TECR/2020-06", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_june2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_june2020 <- read.csv(target_url)

# reformat Date and Time
temps_june2020$startDateTime <- as.POSIXct(temps_june2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_june2020$endDateTime <- as.POSIXct(temps_june2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_june2020, "temps_june2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_june2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_june2020.csv"
)

#### JULY 2020 ####
temps_july2020 <- GET(temps_urls[grep("TECR/2020-07", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_july2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-07.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_july2020 <- read.csv(target_url)

# reformat Date and Time
temps_july2020$startDateTime <- as.POSIXct(temps_july2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_july2020$endDateTime <- as.POSIXct(temps_july2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_july2020, "temps_july2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_july2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_july2020.csv"
)

#### AUGUST 2020 ####
temps_aug2020 <- GET(temps_urls[grep("TECR/2020-08", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_aug2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-08.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_aug2020 <- read.csv(target_url)

# reformat Date and Time
temps_aug2020$startDateTime <- as.POSIXct(temps_aug2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_aug2020$endDateTime <- as.POSIXct(temps_aug2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_aug2020, "temps_aug2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_aug2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_aug2020.csv"
)

#### SEPTEMBER 2020 ####
temps_sept2020 <- GET(temps_urls[grep("TECR/2020-09", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_sept2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_sept2020 <- read.csv(target_url)

# reformat Date and Time
temps_sept2020$startDateTime <- as.POSIXct(temps_sept2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_sept2020$endDateTime <- as.POSIXct(temps_sept2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_sept2020, "temps_sept2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_sept2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_sept2020.csv"
)

#### SEPTEMBER 2020 ####
temps_sept2020 <- GET(temps_urls[grep("TECR/2020-09", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_sept2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_sept2020 <- read.csv(target_url)

# reformat Date and Time
temps_sept2020$startDateTime <- as.POSIXct(temps_sept2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_sept2020$endDateTime <- as.POSIXct(temps_sept2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_sept2020, "temps_sept2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_sept2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_sept2020.csv"
)

#### OCTOBER 2020 ####
temps_oct2020 <- GET(temps_urls[grep("TECR/2020-10", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_oct2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-10.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_oct2020 <- read.csv(target_url)

# reformat Date and Time
temps_oct2020$startDateTime <- as.POSIXct(temps_oct2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_oct2020$endDateTime <- as.POSIXct(temps_oct2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_oct2020, "temps_oct2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_oct2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_oct2020.csv"
)

#### NOVEMBER 2020 ####
temps_nov2020 <- GET(temps_urls[grep("TECR/2020-11", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_nov2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_nov2020 <- read.csv(target_url)

# reformat Date and Time
temps_nov2020$startDateTime <- as.POSIXct(temps_nov2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_nov2020$endDateTime <- as.POSIXct(temps_nov2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_nov2020, "temps_nov2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_nov2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_nov2020.csv"
)

#### DECEMBER 2020 ####
temps_dec2020 <- GET(temps_urls[grep("TECR/2020-12", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_dec2020, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2020-12.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_dec2020 <- read.csv(target_url)

# reformat Date and Time
temps_dec2020$startDateTime <- as.POSIXct(temps_dec2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_dec2020$endDateTime <- as.POSIXct(temps_dec2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_dec2020, "temps_dec2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_dec2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_dec2020.csv"
)

#### JANUARY 2021 ####
temps_jan2021 <- GET(temps_urls[grep("TECR/2021-01", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_jan2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_jan2021 <- read.csv(target_url)

# reformat Date and Time
temps_jan2021$startDateTime <- as.POSIXct(temps_jan2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_jan2021$endDateTime <- as.POSIXct(temps_jan2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_jan2021, "temps_jan2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_jan2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_jan2021.csv"
)

#### FEBRUARY 2021 ####
temps_feb2021 <- GET(temps_urls[grep("TECR/2021-02", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_feb2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_feb2021 <- read.csv(target_url)

# reformat Date and Time
temps_feb2021$startDateTime <- as.POSIXct(temps_feb2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_feb2021$endDateTime <- as.POSIXct(temps_feb2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_feb2021, "temps_feb2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_feb2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_feb2021.csv"
)

#### MARCH 2021 ####
temps_mar2021 <- GET(temps_urls[grep("TECR/2021-03", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_mar2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_mar2021 <- read.csv(target_url)

# reformat Date and Time
temps_mar2021$startDateTime <- as.POSIXct(temps_mar2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_mar2021$endDateTime <- as.POSIXct(temps_mar2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_mar2021, "temps_mar2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_mar2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_mar2021.csv"
)

#### APRIL 2021 ####
temps_apr2021 <- GET(temps_urls[grep("TECR/2021-04", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_apr2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_apr2021 <- read.csv(target_url)

# reformat Date and Time
temps_apr2021$startDateTime <- as.POSIXct(temps_apr2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_apr2021$endDateTime <- as.POSIXct(temps_apr2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_apr2021, "temps_apr2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_apr2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_apr2021.csv"
)

#### MAY 2021 ####
temps_may2021 <- GET(temps_urls[grep("TECR/2021-05", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_may2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_may2021 <- read.csv(target_url)

# reformat Date and Time
temps_may2021$startDateTime <- as.POSIXct(temps_may2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_may2021$endDateTime <- as.POSIXct(temps_may2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_may2021, "temps_may2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_may2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_may2021.csv"
)

#### JUNE 2021 ####
temps_june2021 <- GET(temps_urls[grep("TECR/2021-06", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_june2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_june2021 <- read.csv(target_url)

# reformat Date and Time
temps_june2021$startDateTime <- as.POSIXct(temps_june2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_june2021$endDateTime <- as.POSIXct(temps_june2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_june2021, "temps_june2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_june2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_june2021.csv"
)

#### JULY 2021 ####
temps_july2021 <- GET(temps_urls[grep("TECR/2021-07", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_july2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-07.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_july2021 <- read.csv(target_url)

# reformat Date and Time
temps_july2021$startDateTime <- as.POSIXct(temps_july2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_july2021$endDateTime <- as.POSIXct(temps_july2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_july2021, "temps_july2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_july2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_july2021.csv"
)

#### AUGUST 2021 ####
temps_aug2021 <- GET(temps_urls[grep("TECR/2021-08", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_aug2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-08.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_aug2021 <- read.csv(target_url)

# reformat Date and Time
temps_aug2021$startDateTime <- as.POSIXct(temps_aug2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_aug2021$endDateTime <- as.POSIXct(temps_aug2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_aug2021, "temps_aug2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_aug2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_aug2021.csv"
)

#### SEPTEMBER 2021 ####
temps_sept2021 <- GET(temps_urls[grep("TECR/2021-09", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_sept2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_sept2021 <- read.csv(target_url)

# reformat Date and Time
temps_sept2021$startDateTime <- as.POSIXct(temps_sept2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_sept2021$endDateTime <- as.POSIXct(temps_sept2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_sept2021, "temps_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_sept2021.csv"
)

#### OCTOBER 2021 ####
temps_oct2021 <- GET(temps_urls[grep("TECR/2021-10", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_oct2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-10.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_oct2021 <- read.csv(target_url)

# reformat Date and Time
temps_oct2021$startDateTime <- as.POSIXct(temps_oct2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_oct2021$endDateTime <- as.POSIXct(temps_oct2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_oct2021, "temps_oct2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_oct2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_oct2021.csv"
)

#### NOVEMBER 2021 ####
temps_nov2021 <- GET(temps_urls[grep("TECR/2021-11", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_nov2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_nov2021 <- read.csv(target_url)

# reformat Date and Time
temps_nov2021$startDateTime <- as.POSIXct(temps_nov2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_nov2021$endDateTime <- as.POSIXct(temps_nov2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_nov2021, "temps_nov2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_nov2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_nov2021.csv"
)

#### DECEMBER 2021 ####
temps_dec2021 <- GET(temps_urls[grep("TECR/2021-12", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_dec2021, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2021-12.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_dec2021 <- read.csv(target_url)

# reformat Date and Time
temps_dec2021$startDateTime <- as.POSIXct(temps_dec2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_dec2021$endDateTime <- as.POSIXct(temps_dec2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_dec2021, "temps_dec2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_dec2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_dec2021.csv"
)

#### JANUARY 2022 ####
temps_jan2022 <- GET(temps_urls[grep("TECR/2022-01", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_jan2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_jan2022 <- read.csv(target_url)

# reformat Date and Time
temps_jan2022$startDateTime <- as.POSIXct(temps_jan2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_jan2022$endDateTime <- as.POSIXct(temps_jan2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_jan2022, "temps_jan2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_jan2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_jan2022.csv"
)

#### FEBRUARY 2022 ####
temps_feb2022 <- GET(temps_urls[grep("TECR/2022-02", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_feb2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_feb2022 <- read.csv(target_url)

# reformat Date and Time
temps_feb2022$startDateTime <- as.POSIXct(temps_feb2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_feb2022$endDateTime <- as.POSIXct(temps_feb2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_feb2022, "temps_feb2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_feb2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_feb2022.csv"
)

#### MARCH 2022 ####
temps_mar2022 <- GET(temps_urls[grep("TECR/2022-03", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_mar2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_mar2022 <- read.csv(target_url)

# reformat Date and Time
temps_mar2022$startDateTime <- as.POSIXct(temps_mar2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_mar2022$endDateTime <- as.POSIXct(temps_mar2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_mar2022, "temps_mar2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_mar2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_mar2022.csv"
)

#### APRIL 2022 ####
temps_apr2022 <- GET(temps_urls[grep("TECR/2022-04", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_apr2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_apr2022 <- read.csv(target_url)

# reformat Date and Time
temps_apr2022$startDateTime <- as.POSIXct(temps_apr2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_apr2022$endDateTime <- as.POSIXct(temps_apr2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_apr2022, "temps_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_apr2022.csv"
)

#### MAY 2022 ####
temps_may2022 <- GET(temps_urls[grep("TECR/2022-05", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_may2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_may2022 <- read.csv(target_url)

# reformat Date and Time
temps_may2022$startDateTime <- as.POSIXct(temps_may2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_may2022$endDateTime <- as.POSIXct(temps_may2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_may2022, "temps_may2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_may2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_may2022.csv"
)

#### JUNE 2022 ####
temps_june2022 <- GET(temps_urls[grep("TECR/2022-06", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_june2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_june2022 <- read.csv(target_url)

# reformat Date and Time
temps_june2022$startDateTime <- as.POSIXct(temps_june2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_june2022$endDateTime <- as.POSIXct(temps_june2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_june2022, "temps_june2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_june2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_june2022.csv"
)

#### JULY 2022 ####
temps_july2022 <- GET(temps_urls[grep("TECR/2022-07", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_july2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-07.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_july2022 <- read.csv(target_url)

# reformat Date and Time
temps_july2022$startDateTime <- as.POSIXct(temps_july2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_july2022$endDateTime <- as.POSIXct(temps_july2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_july2022, "temps_july2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_july2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_july2022.csv"
)

#### AUGUST 2022 ####
temps_aug2022 <- GET(temps_urls[grep("TECR/2022-08", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_aug2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-08.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_aug2022 <- read.csv(target_url)

# reformat Date and Time
temps_aug2022$startDateTime <- as.POSIXct(temps_aug2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_aug2022$endDateTime <- as.POSIXct(temps_aug2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_aug2022, "temps_aug2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_aug2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_aug2022.csv"
)

#### SEPTEMBER 2022 ####
temps_sept2022 <- GET(temps_urls[grep("TECR/2022-09", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_sept2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_sept2022 <- read.csv(target_url)

# reformat Date and Time
temps_sept2022$startDateTime <- as.POSIXct(temps_sept2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_sept2022$endDateTime <- as.POSIXct(temps_sept2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_sept2022, "temps_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_sept2022.csv"
)

#### SEPTEMBER 2022 ####
temps_sept2022 <- GET(temps_urls[grep("TECR/2022-09", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_sept2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_sept2022 <- read.csv(target_url)

# reformat Date and Time
temps_sept2022$startDateTime <- as.POSIXct(temps_sept2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_sept2022$endDateTime <- as.POSIXct(temps_sept2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_sept2022, "temps_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_sept2022.csv"
)

#### OCTOBER 2022 ####
temps_oct2022 <- GET(temps_urls[grep("TECR/2022-10", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_oct2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-10.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_oct2022 <- read.csv(target_url)

# reformat Date and Time
temps_oct2022$startDateTime <- as.POSIXct(temps_oct2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_oct2022$endDateTime <- as.POSIXct(temps_oct2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_oct2022, "temps_oct2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_oct2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_oct2022.csv"
)

#### NOVEMBER 2022 ####
temps_nov2022 <- GET(temps_urls[grep("TECR/2022-11", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_nov2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_nov2022 <- read.csv(target_url)

# reformat Date and Time
temps_nov2022$startDateTime <- as.POSIXct(temps_nov2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_nov2022$endDateTime <- as.POSIXct(temps_nov2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_nov2022, "temps_nov2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_nov2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_nov2022.csv"
)

#### DECEMBER 2022 ####
temps_dec2022 <- GET(temps_urls[grep("TECR/2022-12", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_dec2022, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2022-12.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_dec2022 <- read.csv(target_url)

# reformat Date and Time
temps_dec2022$startDateTime <- as.POSIXct(temps_dec2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_dec2022$endDateTime <- as.POSIXct(temps_dec2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_dec2022, "temps_dec2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_dec2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_dec2022.csv"
)

#### JANUARY 2023 ####
temps_jan2023 <- GET(temps_urls[grep("TECR/2023-01", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_jan2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_jan2023 <- read.csv(target_url)

# reformat Date and Time
temps_jan2023$startDateTime <- as.POSIXct(temps_jan2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_jan2023$endDateTime <- as.POSIXct(temps_jan2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_jan2023, "temps_jan2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_jan2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_jan2023.csv"
)

#### FEBRUARY 2023 ####
temps_feb2023 <- GET(temps_urls[grep("TECR/2023-02", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_feb2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_feb2023 <- read.csv(target_url)

# reformat Date and Time
temps_feb2023$startDateTime <- as.POSIXct(temps_feb2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_feb2023$endDateTime <- as.POSIXct(temps_feb2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_feb2023, "temps_feb2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_feb2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_feb2023.csv"
)

#### MARCH 2023 ####
temps_mar2023 <- GET(temps_urls[grep("TECR/2023-03", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_mar2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_mar2023 <- read.csv(target_url)

# reformat Date and Time
temps_mar2023$startDateTime <- as.POSIXct(temps_mar2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_mar2023$endDateTime <- as.POSIXct(temps_mar2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_mar2023, "temps_mar2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_mar2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_mar2023.csv"
)

#### APRIL 2023 ####
temps_apr2023 <- GET(temps_urls[grep("TECR/2023-04", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_apr2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_apr2023 <- read.csv(target_url)

# reformat Date and Time
temps_apr2023$startDateTime <- as.POSIXct(temps_apr2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_apr2023$endDateTime <- as.POSIXct(temps_apr2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_apr2023, "temps_apr2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_apr2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_apr2023.csv"
)

#### MAY 2023 ####
temps_may2023 <- GET(temps_urls[grep("TECR/2023-05", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_may2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_may2023 <- read.csv(target_url)

# reformat Date and Time
temps_may2023$startDateTime <- as.POSIXct(temps_may2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_may2023$endDateTime <- as.POSIXct(temps_may2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_may2023, "temps_may2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_may2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_may2023.csv"
)

#### JUNE 2023 ####
temps_june2023 <- GET(temps_urls[grep("TECR/2023-06", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_june2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_june2023 <- read.csv(target_url)

# reformat Date and Time
temps_june2023$startDateTime <- as.POSIXct(temps_june2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_june2023$endDateTime <- as.POSIXct(temps_june2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_june2023, "temps_june2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_june2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_june2023.csv"
)

#### JULY 2023 ####
temps_july2023 <- GET(temps_urls[grep("TECR/2023-07", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_july2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-07.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_july2023 <- read.csv(target_url)

# reformat Date and Time
temps_july2023$startDateTime <- as.POSIXct(temps_july2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_july2023$endDateTime <- as.POSIXct(temps_july2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_july2023, "temps_july2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_july2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_july2023.csv"
)

#### AUGUST 2023 ####
temps_aug2023 <- GET(temps_urls[grep("TECR/2023-08", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_aug2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-08.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_aug2023 <- read.csv(target_url)

# reformat Date and Time
temps_aug2023$startDateTime <- as.POSIXct(temps_aug2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_aug2023$endDateTime <- as.POSIXct(temps_aug2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_aug2023, "temps_aug2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_aug2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_aug2023.csv"
)

#### SEPTEMBER 2023 ####
temps_sept2023 <- GET(temps_urls[grep("TECR/2023-09", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_sept2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_sept2023 <- read.csv(target_url)

# reformat Date and Time
temps_sept2023$startDateTime <- as.POSIXct(temps_sept2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_sept2023$endDateTime <- as.POSIXct(temps_sept2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_sept2023, "temps_sept2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_sept2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_sept2023.csv"
)

#### OCTOBER 2023 ####
temps_oct2023 <- GET(temps_urls[grep("TECR/2023-10", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_oct2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-10.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_oct2023 <- read.csv(target_url)

# reformat Date and Time
temps_oct2023$startDateTime <- as.POSIXct(temps_oct2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_oct2023$endDateTime <- as.POSIXct(temps_oct2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_oct2023, "temps_oct2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_oct2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_oct2023.csv"
)

#### NOVEMBER 2023 ####
temps_nov2023 <- GET(temps_urls[grep("TECR/2023-11", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_nov2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_nov2023 <- read.csv(target_url)

# reformat Date and Time
temps_nov2023$startDateTime <- as.POSIXct(temps_nov2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_nov2023$endDateTime <- as.POSIXct(temps_nov2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_nov2023, "temps_nov2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_nov2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_nov2023.csv"
)

#### DECEMBER 2023 ####
temps_dec2023 <- GET(temps_urls[grep("TECR/2023-12", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_dec2023, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2023-12.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_dec2023 <- read.csv(target_url)

# reformat Date and Time
temps_dec2023$startDateTime <- as.POSIXct(temps_dec2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_dec2023$endDateTime <- as.POSIXct(temps_dec2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_dec2023, "temps_dec2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_dec2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_dec2023.csv"
)

#### JANUARY 2024 ####
temps_jan2024 <- GET(temps_urls[grep("TECR/2024-01", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_jan2024, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2024-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_jan2024 <- read.csv(target_url)

# reformat Date and Time
temps_jan2024$startDateTime <- as.POSIXct(temps_jan2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_jan2024$endDateTime <- as.POSIXct(temps_jan2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_jan2024, "temps_jan2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_jan2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_jan2024.csv"
)

#### FEBRUARY 2024 ####
temps_feb2024 <- GET(temps_urls[grep("TECR/2024-02", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_feb2024, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2024-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_feb2024 <- read.csv(target_url)

# reformat Date and Time
temps_feb2024$startDateTime <- as.POSIXct(temps_feb2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_feb2024$endDateTime <- as.POSIXct(temps_feb2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_feb2024, "temps_feb2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_feb2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_feb2024.csv"
)

#### MARCH 2024 ####
temps_mar2024 <- GET(temps_urls[grep("TECR/2024-03", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_mar2024, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2024-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_mar2024 <- read.csv(target_url)

# reformat Date and Time
temps_mar2024$startDateTime <- as.POSIXct(temps_mar2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_mar2024$endDateTime <- as.POSIXct(temps_mar2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_mar2024, "temps_mar2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_mar2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_mar2024.csv"
)

#### APRIL 2024 ####
temps_apr2024 <- GET(temps_urls[grep("TECR/2024-04", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_apr2024, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2024-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_apr2024 <- read.csv(target_url)

# reformat Date and Time
temps_apr2024$startDateTime <- as.POSIXct(temps_apr2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_apr2024$endDateTime <- as.POSIXct(temps_apr2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_apr2024, "temps_apr2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_apr2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_apr2024.csv"
)

#### MAY 2024 ####
temps_may2024 <- GET(temps_urls[grep("TECR/2024-05", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_may2024, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2024-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_may2024 <- read.csv(target_url)

# reformat Date and Time
temps_may2024$startDateTime <- as.POSIXct(temps_may2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_may2024$endDateTime <- as.POSIXct(temps_may2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_may2024, "temps_may2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_may2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_may2024.csv"
)

#### JUNE 2024 ####
temps_june2024 <- GET(temps_urls[grep("TECR/2024-06", temps_urls)])
temps_files <- jsonlite::fromJSON(content(temps_june2024, as = "text"))
temps_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "TOSW_5_min.2024-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(temps_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- temps_files$data$files$url[match_index][1]

# read the CSV
temps_june2024 <- read.csv(target_url)

# reformat Date and Time
temps_june2024$startDateTime <- as.POSIXct(temps_june2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
temps_june2024$endDateTime <- as.POSIXct(temps_june2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(temps_june2024, "temps_june2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "temps_june2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt"),
  name = "temps_june2024.csv"
)

#### Read me ####
# # -- As of 12/16/2025, all data after June 2024 were "provisional." 

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
temps <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cM7EbVUU817C6bVL679HAbPOPtqdqTmt")

# List and filter CSV files with "N" in their names
temps_files <- googledrive::drive_ls(path = temps, type = "csv")
temps_files <- temps_files[grepl("temps", temps_files$name), ]

# Create an empty list to store the cleaned data frames
temps_list <- lapply(seq_along(temps_files$name), function(i) {
  googledrive::drive_download(
    file = temps_files$id[i],
    path = paste0("googledrive/", temps_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file
  read.csv(paste0("googledrive/", temps_files$name[i]), header = TRUE)
})

# Assign names to the list elements based on the file names
names(temps_list) <- temps_files$name

# Check the contents of the list
str(temps_list)

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
  file = "all_temp_data.csv",
  row.names = FALSE
)

#### Upload CSV to the specific Google Drive folder ####
folder_id <- drive_get("Temperature (digital thermistor) of surface water")

drive_upload(
  "all_temp_data.csv",
  path = folder_id,
)
