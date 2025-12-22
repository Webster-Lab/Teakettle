#### Read me ####
# -- The following code is what was used to access salt-based discharge data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Temperature (digital thermistor) of surface water."

# Abstract: Water quality consists of a suite of physical and chemical parameters 
# characterizing the health of an aquatic ecosystem. This data product contains 
# in-situ, sensor-based measurements of specific conductance, dissolved oxygen, 
# pH, turbidity, fluorescent dissolved organic matter (fDOM) and chlorophyll-a, 
# available as 1-min measurements in wadeable stream sites and 5-min measurements 
# in lake and river sites. Chlorophyll-a is only measured in lake and river sites.

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
# data product name: "Water quality" (DP1.20288.001)
wq_product <- GET("http://data.neonscience.org/api/v0/products/DP1.20288.001")
wq_product

# make the data readable by jsonlite
wq_text <- content(wq_product, as = "text")

# flatten json into a nested list
wq_avail <- jsonlite::fromJSON(wq_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
wq_avail$data$siteCodes

# TECR is 30th on the list
wq_avail$data$siteCodes$siteCode[[30]]

# check which months at TECR have available data
wq_avail$data$siteCodes$availableMonths[[30]]

# get a complete list of available data URLs
wq_urls <- unlist(wq_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(wq_urls)

# show the first 10 URLs available
wq_urls[1:10]

#### NOVEMBER 2018 ####
wq_nov2018 <- GET(wq_urls[grep("TECR/2018-11", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_nov2018, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2018-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_nov2018 <- read.csv(target_url)

# reformat Date and Time
wq_nov2018$startDateTime <- as.POSIXct(wq_nov2018$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_nov2018$endDateTime <- as.POSIXct(wq_nov2018$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_nov2018, "wq_nov2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_nov2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_nov2018.csv"
)

#### DECEMBER 2018 ####
wq_dec2018 <- GET(wq_urls[grep("TECR/2018-12", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_dec2018, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2018-12.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_dec2018 <- read.csv(target_url)

# reformat Date and Time
wq_dec2018$startDateTime <- as.POSIXct(wq_dec2018$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_dec2018$endDateTime <- as.POSIXct(wq_dec2018$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_dec2018, "wq_dec2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_dec2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_dec2018.csv"
)

#### JANUARY 2019 ####
wq_jan2019 <- GET(wq_urls[grep("TECR/2019-01", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_jan2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_jan2019 <- read.csv(target_url)

# reformat Date and Time
wq_jan2019$startDateTime <- as.POSIXct(wq_jan2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_jan2019$endDateTime <- as.POSIXct(wq_jan2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_jan2019, "wq_jan2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_jan2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_jan2019.csv"
)

#### FEBRUARY 2019 ####
wq_feb2019 <- GET(wq_urls[grep("TECR/2019-02", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_feb2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_feb2019 <- read.csv(target_url)

# reformat Date and Time
wq_feb2019$startDateTime <- as.POSIXct(wq_feb2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_feb2019$endDateTime <- as.POSIXct(wq_feb2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_feb2019, "wq_feb2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_feb2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_feb2019.csv"
)

#### MARCH 2019 ####
wq_mar2019 <- GET(wq_urls[grep("TECR/2019-03", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_mar2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_mar2019 <- read.csv(target_url)

# reformat Date and Time
wq_mar2019$startDateTime <- as.POSIXct(wq_mar2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_mar2019$endDateTime <- as.POSIXct(wq_mar2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_mar2019, "wq_mar2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_mar2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_mar2019.csv"
)

#### APRIL 2019 ####
wq_apr2019 <- GET(wq_urls[grep("TECR/2019-04", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_apr2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_apr2019 <- read.csv(target_url)

# reformat Date and Time
wq_apr2019$startDateTime <- as.POSIXct(wq_apr2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_apr2019$endDateTime <- as.POSIXct(wq_apr2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_apr2019, "wq_apr2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_apr2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_apr2019.csv"
)

#### MAY 2019 ####
wq_may2019 <- GET(wq_urls[grep("TECR/2019-05", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_may2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_may2019 <- read.csv(target_url)

# reformat Date and Time
wq_may2019$startDateTime <- as.POSIXct(wq_may2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_may2019$endDateTime <- as.POSIXct(wq_may2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_may2019, "wq_may2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_may2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_may2019.csv"
)

#### JUNE 2019 ####
wq_june2019 <- GET(wq_urls[grep("TECR/2019-06", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_june2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_june2019 <- read.csv(target_url)

# reformat Date and Time
wq_june2019$startDateTime <- as.POSIXct(wq_june2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_june2019$endDateTime <- as.POSIXct(wq_june2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_june2019, "wq_june2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_june2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_june2019.csv"
)

#### JULY 2019 ####
wq_july2019 <- GET(wq_urls[grep("TECR/2019-07", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_july2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-07.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_july2019 <- read.csv(target_url)

# reformat Date and Time
wq_july2019$startDateTime <- as.POSIXct(wq_july2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_july2019$endDateTime <- as.POSIXct(wq_july2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_july2019, "wq_july2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_july2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_july2019.csv"
)

#### AUGUST 2019 ####
wq_aug2019 <- GET(wq_urls[grep("TECR/2019-08", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_aug2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-08.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_aug2019 <- read.csv(target_url)

# reformat Date and Time
wq_aug2019$startDateTime <- as.POSIXct(wq_aug2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_aug2019$endDateTime <- as.POSIXct(wq_aug2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_aug2019, "wq_aug2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_aug2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_aug2019.csv"
)

#### SEPTEMBER 2019 ####
wq_sept2019 <- GET(wq_urls[grep("TECR/2019-09", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_sept2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_sept2019 <- read.csv(target_url)

# reformat Date and Time
wq_sept2019$startDateTime <- as.POSIXct(wq_sept2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_sept2019$endDateTime <- as.POSIXct(wq_sept2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_sept2019, "wq_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_sept2019.csv"
)

#### OCTOBER 2019 ####
wq_oct2019 <- GET(wq_urls[grep("TECR/2019-10", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_oct2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-10.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_oct2019 <- read.csv(target_url)

# reformat Date and Time
wq_oct2019$startDateTime <- as.POSIXct(wq_oct2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_oct2019$endDateTime <- as.POSIXct(wq_oct2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_oct2019, "wq_oct2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_oct2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_oct2019.csv"
)

#### NOVEMBER 2019 ####
wq_nov2019 <- GET(wq_urls[grep("TECR/2019-11", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_nov2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_nov2019 <- read.csv(target_url)

# reformat Date and Time
wq_nov2019$startDateTime <- as.POSIXct(wq_nov2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_nov2019$endDateTime <- as.POSIXct(wq_nov2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_nov2019, "wq_nov2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_nov2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_nov2019.csv"
)

#### DECEMBER 2019 ####
wq_dec2019 <- GET(wq_urls[grep("TECR/2019-12", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_dec2019, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2019-12.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_dec2019 <- read.csv(target_url)

# reformat Date and Time
wq_dec2019$startDateTime <- as.POSIXct(wq_dec2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_dec2019$endDateTime <- as.POSIXct(wq_dec2019$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_dec2019, "wq_dec2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_dec2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_dec2019.csv"
)

#### JANUARY 2020 ####
wq_jan2020 <- GET(wq_urls[grep("TECR/2020-01", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_jan2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_jan2020 <- read.csv(target_url)

# reformat Date and Time
wq_jan2020$startDateTime <- as.POSIXct(wq_jan2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_jan2020$endDateTime <- as.POSIXct(wq_jan2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_jan2020, "wq_jan2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_jan2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_jan2020.csv"
)

#### FEBRUARY 2020 ####
wq_feb2020 <- GET(wq_urls[grep("TECR/2020-02", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_feb2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_feb2020 <- read.csv(target_url)

# reformat Date and Time
wq_feb2020$startDateTime <- as.POSIXct(wq_feb2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_feb2020$endDateTime <- as.POSIXct(wq_feb2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_feb2020, "wq_feb2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_feb2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_feb2020.csv"
)

#### MARCH 2020 ####
wq_mar2020 <- GET(wq_urls[grep("TECR/2020-03", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_mar2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_mar2020 <- read.csv(target_url)

# reformat Date and Time
wq_mar2020$startDateTime <- as.POSIXct(wq_mar2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_mar2020$endDateTime <- as.POSIXct(wq_mar2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_mar2020, "wq_mar2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_mar2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_mar2020.csv"
)

#### APRIL 2020 ####
wq_apr2020 <- GET(wq_urls[grep("TECR/2020-04", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_apr2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_apr2020 <- read.csv(target_url)

# reformat Date and Time
wq_apr2020$startDateTime <- as.POSIXct(wq_apr2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_apr2020$endDateTime <- as.POSIXct(wq_apr2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_apr2020, "wq_apr2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_apr2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_apr2020.csv"
)

#### MAY 2020 ####
wq_may2020 <- GET(wq_urls[grep("TECR/2020-05", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_may2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_may2020 <- read.csv(target_url)

# reformat Date and Time
wq_may2020$startDateTime <- as.POSIXct(wq_may2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_may2020$endDateTime <- as.POSIXct(wq_may2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_may2020, "wq_may2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_may2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_may2020.csv"
)

#### JUNE 2020 ####
wq_june2020 <- GET(wq_urls[grep("TECR/2020-06", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_june2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_june2020 <- read.csv(target_url)

# reformat Date and Time
wq_june2020$startDateTime <- as.POSIXct(wq_june2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_june2020$endDateTime <- as.POSIXct(wq_june2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_june2020, "wq_june2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_june2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_june2020.csv"
)

#### JULY 2020 ####
wq_july2020 <- GET(wq_urls[grep("TECR/2020-07", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_july2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-07.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_july2020 <- read.csv(target_url)

# reformat Date and Time
wq_july2020$startDateTime <- as.POSIXct(wq_july2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_july2020$endDateTime <- as.POSIXct(wq_july2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_july2020, "wq_july2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_july2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_july2020.csv"
)

#### AUGUST 2020 ####
wq_aug2020 <- GET(wq_urls[grep("TECR/2020-08", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_aug2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-08.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_aug2020 <- read.csv(target_url)

# reformat Date and Time
wq_aug2020$startDateTime <- as.POSIXct(wq_aug2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_aug2020$endDateTime <- as.POSIXct(wq_aug2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_aug2020, "wq_aug2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_aug2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_aug2020.csv"
)

#### SEPTEMBER 2020 ####
wq_sept2020 <- GET(wq_urls[grep("TECR/2020-09", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_sept2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_sept2020 <- read.csv(target_url)

# reformat Date and Time
wq_sept2020$startDateTime <- as.POSIXct(wq_sept2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_sept2020$endDateTime <- as.POSIXct(wq_sept2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_sept2020, "wq_sept2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_sept2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_sept2020.csv"
)

#### OCTOBER 2020 ####
wq_oct2020 <- GET(wq_urls[grep("TECR/2020-10", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_oct2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-10.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_oct2020 <- read.csv(target_url)

# reformat Date and Time
wq_oct2020$startDateTime <- as.POSIXct(wq_oct2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_oct2020$endDateTime <- as.POSIXct(wq_oct2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_oct2020, "wq_oct2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_oct2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_oct2020.csv"
)

#### NOVEMBER 2020 ####
wq_nov2020 <- GET(wq_urls[grep("TECR/2020-11", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_nov2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_nov2020 <- read.csv(target_url)

# reformat Date and Time
wq_nov2020$startDateTime <- as.POSIXct(wq_nov2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_nov2020$endDateTime <- as.POSIXct(wq_nov2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_nov2020, "wq_nov2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_nov2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_nov2020.csv"
)

#### DECEMBER 2020 ####
wq_dec2020 <- GET(wq_urls[grep("TECR/2020-12", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_dec2020, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2020-12.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_dec2020 <- read.csv(target_url)

# reformat Date and Time
wq_dec2020$startDateTime <- as.POSIXct(wq_dec2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_dec2020$endDateTime <- as.POSIXct(wq_dec2020$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_dec2020, "wq_dec2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_dec2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_dec2020.csv"
)

#### JANUARY 2021 ####
wq_jan2021 <- GET(wq_urls[grep("TECR/2021-01", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_jan2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_jan2021 <- read.csv(target_url)

# reformat Date and Time
wq_jan2021$startDateTime <- as.POSIXct(wq_jan2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_jan2021$endDateTime <- as.POSIXct(wq_jan2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_jan2021, "wq_jan2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_jan2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_jan2021.csv"
)

#### FEBRUARY 2021 ####
wq_feb2021 <- GET(wq_urls[grep("TECR/2021-02", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_feb2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_feb2021 <- read.csv(target_url)

# reformat Date and Time
wq_feb2021$startDateTime <- as.POSIXct(wq_feb2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_feb2021$endDateTime <- as.POSIXct(wq_feb2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_feb2021, "wq_feb2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_feb2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_feb2021.csv"
)

#### MARCH 2021 ####
wq_mar2021 <- GET(wq_urls[grep("TECR/2021-03", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_mar2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_mar2021 <- read.csv(target_url)

# reformat Date and Time
wq_mar2021$startDateTime <- as.POSIXct(wq_mar2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_mar2021$endDateTime <- as.POSIXct(wq_mar2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_mar2021, "wq_mar2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_mar2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_mar2021.csv"
)

#### APRIL 2021 ####
wq_apr2021 <- GET(wq_urls[grep("TECR/2021-04", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_apr2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_apr2021 <- read.csv(target_url)

# reformat Date and Time
wq_apr2021$startDateTime <- as.POSIXct(wq_apr2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_apr2021$endDateTime <- as.POSIXct(wq_apr2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_apr2021, "wq_apr2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_apr2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_apr2021.csv"
)

#### MAY 2021 ####
wq_may2021 <- GET(wq_urls[grep("TECR/2021-05", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_may2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_may2021 <- read.csv(target_url)

# reformat Date and Time
wq_may2021$startDateTime <- as.POSIXct(wq_may2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_may2021$endDateTime <- as.POSIXct(wq_may2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_may2021, "wq_may2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_may2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_may2021.csv"
)

#### JUNE 2021 ####
wq_june2021 <- GET(wq_urls[grep("TECR/2021-06", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_june2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_june2021 <- read.csv(target_url)

# reformat Date and Time
wq_june2021$startDateTime <- as.POSIXct(wq_june2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_june2021$endDateTime <- as.POSIXct(wq_june2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_june2021, "wq_june2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_june2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_june2021.csv"
)

#### JULY 2021 ####
wq_july2021 <- GET(wq_urls[grep("TECR/2021-07", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_july2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-07.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_july2021 <- read.csv(target_url)

# reformat Date and Time
wq_july2021$startDateTime <- as.POSIXct(wq_july2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_july2021$endDateTime <- as.POSIXct(wq_july2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_july2021, "wq_july2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_july2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_july2021.csv"
)

#### AUGUST 2021 ####
wq_aug2021 <- GET(wq_urls[grep("TECR/2021-08", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_aug2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-08.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_aug2021 <- read.csv(target_url)

# reformat Date and Time
wq_aug2021$startDateTime <- as.POSIXct(wq_aug2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_aug2021$endDateTime <- as.POSIXct(wq_aug2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_aug2021, "wq_aug2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_aug2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_aug2021.csv"
)

#### SEPTEMBER 2021 ####
wq_sept2021 <- GET(wq_urls[grep("TECR/2021-09", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_sept2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-09.basic."
# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_sept2021 <- read.csv(target_url)

# reformat Date and Time
wq_sept2021$startDateTime <- as.POSIXct(wq_sept2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_sept2021$endDateTime <- as.POSIXct(wq_sept2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_sept2021, "wq_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_sept2021.csv"
)

#### OCTOBER 2021 ####
wq_oct2021 <- GET(wq_urls[grep("TECR/2021-10", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_oct2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-10.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_oct2021 <- read.csv(target_url)

# reformat Date and Time
wq_oct2021$startDateTime <- as.POSIXct(wq_oct2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_oct2021$endDateTime <- as.POSIXct(wq_oct2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_oct2021, "wq_oct2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_oct2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_oct2021.csv"
)

#### NOVEMBER 2021 ####
wq_nov2021 <- GET(wq_urls[grep("TECR/2021-11", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_nov2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_nov2021 <- read.csv(target_url)

# reformat Date and Time
wq_nov2021$startDateTime <- as.POSIXct(wq_nov2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_nov2021$endDateTime <- as.POSIXct(wq_nov2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_nov2021, "wq_nov2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_nov2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_nov2021.csv"
)

#### DECEMBER 2021 ####
wq_dec2021 <- GET(wq_urls[grep("TECR/2021-12", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_dec2021, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2021-12.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_dec2021 <- read.csv(target_url)

# reformat Date and Time
wq_dec2021$startDateTime <- as.POSIXct(wq_dec2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_dec2021$endDateTime <- as.POSIXct(wq_dec2021$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_dec2021, "wq_dec2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_dec2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_dec2021.csv"
)

#### JANUARY 2022 ####
wq_jan2022 <- GET(wq_urls[grep("TECR/2022-01", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_jan2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_jan2022 <- read.csv(target_url)

# reformat Date and Time
wq_jan2022$startDateTime <- as.POSIXct(wq_jan2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_jan2022$endDateTime <- as.POSIXct(wq_jan2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_jan2022, "wq_jan2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_jan2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_jan2022.csv"
)

#### FEBRUARY 2022 ####
wq_feb2022 <- GET(wq_urls[grep("TECR/2022-02", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_feb2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_feb2022 <- read.csv(target_url)

# reformat Date and Time
wq_feb2022$startDateTime <- as.POSIXct(wq_feb2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_feb2022$endDateTime <- as.POSIXct(wq_feb2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_feb2022, "wq_feb2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_feb2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_feb2022.csv"
)

#### MARCH 2022 ####
wq_mar2022 <- GET(wq_urls[grep("TECR/2022-03", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_mar2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_mar2022 <- read.csv(target_url)

# reformat Date and Time
wq_mar2022$startDateTime <- as.POSIXct(wq_mar2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_mar2022$endDateTime <- as.POSIXct(wq_mar2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_mar2022, "wq_mar2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_mar2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_mar2022.csv"
)

#### APRIL 2022 ####
wq_apr2022 <- GET(wq_urls[grep("TECR/2022-04", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_apr2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_apr2022 <- read.csv(target_url)

# reformat Date and Time
wq_apr2022$startDateTime <- as.POSIXct(wq_apr2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_apr2022$endDateTime <- as.POSIXct(wq_apr2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_apr2022, "wq_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_apr2022.csv"
)

#### MAY 2022 ####
wq_may2022 <- GET(wq_urls[grep("TECR/2022-05", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_may2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_may2022 <- read.csv(target_url)

# reformat Date and Time
wq_may2022$startDateTime <- as.POSIXct(wq_may2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_may2022$endDateTime <- as.POSIXct(wq_may2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_may2022, "wq_may2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_may2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_may2022.csv"
)

#### JUNE 2022 ####
wq_june2022 <- GET(wq_urls[grep("TECR/2022-06", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_june2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_june2022 <- read.csv(target_url)

# reformat Date and Time
wq_june2022$startDateTime <- as.POSIXct(wq_june2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_june2022$endDateTime <- as.POSIXct(wq_june2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_june2022, "wq_june2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_june2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_june2022.csv"
)

#### JULY 2022 ####
wq_july2022 <- GET(wq_urls[grep("TECR/2022-07", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_july2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-07.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_july2022 <- read.csv(target_url)

# reformat Date and Time
wq_july2022$startDateTime <- as.POSIXct(wq_july2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_july2022$endDateTime <- as.POSIXct(wq_july2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_july2022, "wq_july2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_july2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_july2022.csv"
)

#### AUGUST 2022 ####
wq_aug2022 <- GET(wq_urls[grep("TECR/2022-08", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_aug2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-08.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_aug2022 <- read.csv(target_url)

# reformat Date and Time
wq_aug2022$startDateTime <- as.POSIXct(wq_aug2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_aug2022$endDateTime <- as.POSIXct(wq_aug2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_aug2022, "wq_aug2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_aug2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_aug2022.csv"
)

#### SEPTEMBER 2022 ####
wq_sept2022 <- GET(wq_urls[grep("TECR/2022-09", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_sept2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_sept2022 <- read.csv(target_url)

# reformat Date and Time
wq_sept2022$startDateTime <- as.POSIXct(wq_sept2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_sept2022$endDateTime <- as.POSIXct(wq_sept2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_sept2022, "wq_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_sept2022.csv"
)

#### OCTOBER 2022 ####
wq_oct2022 <- GET(wq_urls[grep("TECR/2022-10", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_oct2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-10.basic.."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_oct2022 <- read.csv(target_url)

# reformat Date and Time
wq_oct2022$startDateTime <- as.POSIXct(wq_oct2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_oct2022$endDateTime <- as.POSIXct(wq_oct2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_oct2022, "wq_oct2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_oct2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_oct2022.csv"
)

#### NOVEMBER 2022 ####
wq_nov2022 <- GET(wq_urls[grep("TECR/2022-11", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_nov2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_nov2022 <- read.csv(target_url)

# reformat Date and Time
wq_nov2022$startDateTime <- as.POSIXct(wq_nov2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_nov2022$endDateTime <- as.POSIXct(wq_nov2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_nov2022, "wq_nov2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_nov2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_nov2022.csv"
)

#### DECEMBER 2022 ####
wq_dec2022 <- GET(wq_urls[grep("TECR/2022-12", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_dec2022, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2022-12.basic.."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_dec2022 <- read.csv(target_url)

# reformat Date and Time
wq_dec2022$startDateTime <- as.POSIXct(wq_dec2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_dec2022$endDateTime <- as.POSIXct(wq_dec2022$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_dec2022, "wq_dec2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_dec2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_dec2022.csv"
)

#### JANUARY 2023 ####
wq_jan2023 <- GET(wq_urls[grep("TECR/2023-01", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_jan2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_jan2023 <- read.csv(target_url)

# reformat Date and Time
wq_jan2023$startDateTime <- as.POSIXct(wq_jan2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_jan2023$endDateTime <- as.POSIXct(wq_jan2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_jan2023, "wq_jan2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_jan2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_jan2023.csv"
)

#### FEBRUARY 2023 ####
wq_feb2023 <- GET(wq_urls[grep("TECR/2023-02", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_feb2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_feb2023 <- read.csv(target_url)

# reformat Date and Time
wq_feb2023$startDateTime <- as.POSIXct(wq_feb2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_feb2023$endDateTime <- as.POSIXct(wq_feb2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_feb2023, "wq_feb2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_feb2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_feb2023.csv"
)

#### MARCH 2023 ####
wq_mar2023 <- GET(wq_urls[grep("TECR/2023-03", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_mar2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_mar2023 <- read.csv(target_url)

# reformat Date and Time
wq_mar2023$startDateTime <- as.POSIXct(wq_mar2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_mar2023$endDateTime <- as.POSIXct(wq_mar2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_mar2023, "wq_mar2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_mar2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_mar2023.csv"
)

#### APRIL 2023 ####
wq_apr2023 <- GET(wq_urls[grep("TECR/2023-04", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_apr2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_apr2023 <- read.csv(target_url)

# reformat Date and Time
wq_apr2023$startDateTime <- as.POSIXct(wq_apr2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_apr2023$endDateTime <- as.POSIXct(wq_apr2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_apr2023, "wq_apr2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_apr2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_apr2023.csv"
)

#### MAY 2023 ####
wq_may2023 <- GET(wq_urls[grep("TECR/2023-05", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_may2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_may2023 <- read.csv(target_url)

# reformat Date and Time
wq_may2023$startDateTime <- as.POSIXct(wq_may2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_may2023$endDateTime <- as.POSIXct(wq_may2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_may2023, "wq_may2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_may2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_may2023.csv"
)

#### JUNE 2023 ####
wq_june2023 <- GET(wq_urls[grep("TECR/2023-06", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_june2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_june2023 <- read.csv(target_url)

# reformat Date and Time
wq_june2023$startDateTime <- as.POSIXct(wq_june2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_june2023$endDateTime <- as.POSIXct(wq_june2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_june2023, "wq_june2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_june2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_june2023.csv"
)

#### JULY 2023 ####
wq_july2023 <- GET(wq_urls[grep("TECR/2023-07", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_july2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-07.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_july2023 <- read.csv(target_url)

# reformat Date and Time
wq_july2023$startDateTime <- as.POSIXct(wq_july2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_july2023$endDateTime <- as.POSIXct(wq_july2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_july2023, "wq_july2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_july2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_july2023.csv"
)

#### AUGUST 2023 ####
wq_aug2023 <- GET(wq_urls[grep("TECR/2023-08", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_aug2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-08.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_aug2023 <- read.csv(target_url)

# reformat Date and Time
wq_aug2023$startDateTime <- as.POSIXct(wq_aug2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_aug2023$endDateTime <- as.POSIXct(wq_aug2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_aug2023, "wq_aug2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_aug2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_aug2023.csv"
)

#### SEPTEMBER 2023 ####
wq_sept2023 <- GET(wq_urls[grep("TECR/2023-09", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_sept2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-09.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_sept2023 <- read.csv(target_url)

# reformat Date and Time
wq_sept2023$startDateTime <- as.POSIXct(wq_sept2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_sept2023$endDateTime <- as.POSIXct(wq_sept2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_sept2023, "wq_sept2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_sept2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_sept2023.csv"
)

#### OCTOBER 2023 ####
wq_oct2023 <- GET(wq_urls[grep("TECR/2023-10", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_oct2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-10.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_oct2023 <- read.csv(target_url)

# reformat Date and Time
wq_oct2023$startDateTime <- as.POSIXct(wq_oct2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_oct2023$endDateTime <- as.POSIXct(wq_oct2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_oct2023, "wq_oct2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_oct2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_oct2023.csv"
)

#### NOVEMBER 2023 ####
wq_nov2023 <- GET(wq_urls[grep("TECR/2023-11", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_nov2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-11.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_nov2023 <- read.csv(target_url)

# reformat Date and Time
wq_nov2023$startDateTime <- as.POSIXct(wq_nov2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_nov2023$endDateTime <- as.POSIXct(wq_nov2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_nov2023, "wq_nov2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_nov2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_nov2023.csv"
)

#### DECEMBER 2023 ####
wq_dec2023 <- GET(wq_urls[grep("TECR/2023-12", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_dec2023, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2023-12.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_dec2023 <- read.csv(target_url)

# reformat Date and Time
wq_dec2023$startDateTime <- as.POSIXct(wq_dec2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_dec2023$endDateTime <- as.POSIXct(wq_dec2023$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_dec2023, "wq_dec2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_dec2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_dec2023.csv"
)

#### JANUARY 2024 ####
wq_jan2024 <- GET(wq_urls[grep("TECR/2024-01", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_jan2024, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2024-01.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_jan2024 <- read.csv(target_url)

# reformat Date and Time
wq_jan2024$startDateTime <- as.POSIXct(wq_jan2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_jan2024$endDateTime <- as.POSIXct(wq_jan2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_jan2024, "wq_jan2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_jan2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_jan2024.csv"
)

#### FEBRUARY 2024 ####
wq_feb2024 <- GET(wq_urls[grep("TECR/2024-02", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_feb2024, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2024-02.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_feb2024 <- read.csv(target_url)

# reformat Date and Time
wq_feb2024$startDateTime <- as.POSIXct(wq_feb2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_feb2024$endDateTime <- as.POSIXct(wq_feb2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_feb2024, "wq_feb2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_feb2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_feb2024.csv"
)

#### MARCH 2024 ####
wq_mar2024 <- GET(wq_urls[grep("TECR/2024-03", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_mar2024, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2024-03.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_mar2024 <- read.csv(target_url)

# reformat Date and Time
wq_mar2024$startDateTime <- as.POSIXct(wq_mar2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_mar2024$endDateTime <- as.POSIXct(wq_mar2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_mar2024, "wq_mar2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_mar2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_mar2024.csv"
)

#### APRIL 2024 ####
wq_apr2024 <- GET(wq_urls[grep("TECR/2024-04", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_apr2024, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2024-04.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_apr2024 <- read.csv(target_url)

# reformat Date and Time
wq_apr2024$startDateTime <- as.POSIXct(wq_apr2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_apr2024$endDateTime <- as.POSIXct(wq_apr2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_apr2024, "wq_apr2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_apr2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_apr2024.csv"
)

#### MAY 2024 ####
wq_may2024 <- GET(wq_urls[grep("TECR/2024-05", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_may2024, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2024-05.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_may2024 <- read.csv(target_url)

# reformat Date and Time
wq_may2024$startDateTime <- as.POSIXct(wq_may2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_may2024$endDateTime <- as.POSIXct(wq_may2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_may2024, "wq_may2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_may2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_may2024.csv"
)

#### JUNE 2024 ####
wq_june2024 <- GET(wq_urls[grep("TECR/2024-06", wq_urls)])
wq_files <- jsonlite::fromJSON(content(wq_june2024, as = "text"))
wq_files$data$files$name

# because there are duplicate files, specify the target file name
target_name <- "waq_instantaneous.2024-06.basic."

# use str_detect to find which file names match the pattern
match_index <- stringr::str_detect(wq_files$data$files$name, target_name)

# select the corresponding URL (if more than one matches, you need to pick one)
target_url <- wq_files$data$files$url[match_index][1]

# read the CSV
wq_june2024 <- read.csv(target_url)

# reformat Date and Time
wq_june2024$startDateTime <- as.POSIXct(wq_june2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
wq_june2024$endDateTime <- as.POSIXct(wq_june2024$endDateTime, format = "%Y-%m-%dT%H:%MS=:%SZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(wq_june2024, "wq_june2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "wq_june2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE"),
  name = "wq_june2024.csv"
)

#### Read me ####
# # -- As of 12/16/2025, all data after June 2024 were "provisional." 

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
waterquality <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1OOuGM2_rUupO5q144e3AE63k-vyE0NoE")

# List and filter CSV files with "N" in their names
waterquality_files <- googledrive::drive_ls(path = waterquality, type = "csv")
waterquality_files <- waterquality_files[grepl("wq", waterquality_files$name), ]

# Create an empty list to store the cleaned data frames
waterquality_list <- lapply(seq_along(waterquality_files$name), function(i) {
  googledrive::drive_download(
    file = waterquality_files$id[i],
    path = paste0("googledrive/", waterquality_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file
  read.csv(paste0("googledrive/", waterquality_files$name[i]), header = TRUE)
})

# Assign names to the list elements based on the file names
names(waterquality_list) <- waterquality_files$name

# Check the contents of the list
str(waterquality_list)

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
  file = "all_waterquality_data.csv",
  row.names = FALSE
)

#### Upload CSV to the specific Google Drive folder ####
folder_id <- drive_get("Water quality")

drive_upload(
  "all_waterquality_data.csv",
  path = folder_id,
)
