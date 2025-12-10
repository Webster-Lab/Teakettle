#### Read me ####
# -- The following code is what was used to access nitrate data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Nitrate in surface water." 

# Abstract from NEON: Nitrate is measured using an optical sensor at the 
# downstream (S2) sensor station in streams and on the buoy at lake and river sites. 
# It is reported as a mean value from 20 measurements made during a sampling 
# burst every 15 minutes. Some older data may have been processed as the 
# mean value from 50 measurements, but as of late 2018, the algorithm was 
# updated to 20 measurements and older data will eventually be re-processed 
# to use 20 measurements.

# A tutorial for using the NEON API can be found here: 
# https://www.neonscience.org/resources/learning-hub/tutorials/neon-api-usage

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
# data product name: "Nitrate in surface water" (DP1.20033.001)
N_product <- GET("http://data.neonscience.org/api/v0/products/DP1.20033.001")
N_product

# make the data readable by jsonlite
N_text <- content(N_product, as = "text")

# flatten json into a nested list
N_avail <- jsonlite::fromJSON(N_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
N_avail$data$siteCodes

# TECR is 30th on the list
N_avail$data$siteCodes$siteCode[[30]]

# check which months at TECR have available data
N_avail$data$siteCodes$availableMonths[[30]]

# get a complete list of available data URLs
N_urls <- unlist(N_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(N_urls)

# show the first 10 URLs available
N_urls[1:10]
#### NOVEMBER 2018 ####

N_nov2018 <- GET(N_urls[grep("TECR/2018-11", N_urls)])
N_files <- jsonlite::fromJSON(content(N_nov2018, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_nov2018 <- read.csv(N_files$data$files$url
                    [grep("NSW_15_minute.2018-11.basic.",
                          N_files$data$files$name)])

# reformat Date and Time
N_nov2018$startDateTime <- as.POSIXct(N_nov2018$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_nov2018$endDateTime <- as.POSIXct(N_nov2018$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_nov2018$adj_N_mean <- (N_nov2018$surfWaterNitrateMean * 62.0049)/1000

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_nov2018, "N_nov2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_nov2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_nov2018.csv"
)

#### DECEMBER 2018 ####

N_dec2018 <- GET(N_urls[grep("TECR/2018-12", N_urls)])
N_files <- jsonlite::fromJSON(content(N_dec2018, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_dec2018 <- read.csv(N_files$data$files$url
                    [grep("NSW_15_minute.2018-12.basic.",
                          N_files$data$files$name)])

# reformat Date and Time
N_dec2018$startDateTime <- as.POSIXct(N_dec2018$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_dec2018$endDateTime <- as.POSIXct(N_dec2018$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_dec2018$adj_N_mean <- (N_dec2018$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_dec2018, "N_dec2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_dec2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_dec2018.csv"
)

#### JANUARY 2019 ####

N_jan2019 <- GET(N_urls[grep("TECR/2019-01", N_urls)])
N_files <- jsonlite::fromJSON(content(N_jan2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_jan2019 <- read.csv(N_files$data$files$url
                    [grep("NSW_15_minute.2019-01.basic.",
                          N_files$data$files$name)])

# reformat Date and Time
N_jan2019$startDateTime <- as.POSIXct(N_jan2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_jan2019$endDateTime <- as.POSIXct(N_jan2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_jan2019$adj_N_mean <- (N_jan2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_jan2019, "N_jan2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_jan2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_jan2019.csv"
)

#### FEBRUARY 2019 ####

N_feb2019 <- GET(N_urls[grep("TECR/2019-02", N_urls)])
N_files <- jsonlite::fromJSON(content(N_feb2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_feb2019 <- read.csv(N_files$data$files$url
                    [grep("NSW_15_minute.2019-02.basic.",
                          N_files$data$files$name)])

# reformat Date and Time
N_feb2019$startDateTime <- as.POSIXct(N_feb2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_feb2019$endDateTime <- as.POSIXct(N_feb2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_feb2019$adj_N_mean <- (N_feb2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_feb2019, "N_feb2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_feb2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_feb2019.csv"
)

#### MARCH 2019 ####

N_mar2019 <- GET(N_urls[grep("TECR/2019-03", N_urls)])
N_files <- jsonlite::fromJSON(content(N_mar2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_mar2019 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2019-03.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_mar2019$startDateTime <- as.POSIXct(N_mar2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_mar2019$endDateTime <- as.POSIXct(N_mar2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_mar2019$adj_N_mean <- (N_mar2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_mar2019, "N_mar2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_mar2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_mar2019.csv"
)

#### APRIL 2019 ####

N_apr2019 <- GET(N_urls[grep("TECR/2019-04", N_urls)])
N_files <- jsonlite::fromJSON(content(N_apr2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_apr2019 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2019-04.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_apr2019$startDateTime <- as.POSIXct(N_apr2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_apr2019$endDateTime <- as.POSIXct(N_apr2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_apr2019$adj_N_mean <- (N_apr2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_apr2019, "N_apr2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_apr2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_apr2019.csv"
)

#### MAY 2019 ####

N_may2019 <- GET(N_urls[grep("TECR/2019-05", N_urls)])
N_files <- jsonlite::fromJSON(content(N_may2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_may2019 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2019-05.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_may2019$startDateTime <- as.POSIXct(N_may2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_may2019$endDateTime <- as.POSIXct(N_may2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_may2019$adj_N_mean <- (N_may2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_may2019, "N_may2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_may2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_may2019.csv"
)

#### JUNE 2019 ####

N_june2019 <- GET(N_urls[grep("TECR/2019-06", N_urls)])
N_files <- jsonlite::fromJSON(content(N_june2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_june2019 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2019-06.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_june2019$startDateTime <- as.POSIXct(N_june2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_june2019$endDateTime <- as.POSIXct(N_june2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_june2019$adj_N_mean <- (N_june2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_june2019, "N_june2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_june2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_june2019.csv"
)

#### JULY 2019 ####

N_july2019 <- GET(N_urls[grep("TECR/2019-07", N_urls)])
N_files <- jsonlite::fromJSON(content(N_july2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_july2019 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2019-07.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_july2019$startDateTime <- as.POSIXct(N_july2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_july2019$endDateTime <- as.POSIXct(N_july2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_july2019$adj_N_mean <- (N_july2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_july2019, "N_july2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_july2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_july2019.csv"
)

#### AUGUST 2019 ####

N_aug2019 <- GET(N_urls[grep("TECR/2019-08", N_urls)])
N_files <- jsonlite::fromJSON(content(N_aug2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_aug2019 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2019-08.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_aug2019$startDateTime <- as.POSIXct(N_aug2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_aug2019$endDateTime <- as.POSIXct(N_aug2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_aug2019$adj_N_mean <- (N_aug2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_aug2019, "N_aug2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_aug2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_aug2019.csv"
)

#### SEPTEMBER 2019 ####

N_sept2019 <- GET(N_urls[grep("TECR/2019-09", N_urls)])
N_files <- jsonlite::fromJSON(content(N_sept2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_sept2019 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2019-09.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_sept2019$startDateTime <- as.POSIXct(N_sept2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_sept2019$endDateTime <- as.POSIXct(N_sept2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_sept2019$adj_N_mean <- (N_sept2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_sept2019, "N_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_sept2019.csv"
)

#### OCTOBER 2019 ####

N_oct2019 <- GET(N_urls[grep("TECR/2019-10", N_urls)])
N_files <- jsonlite::fromJSON(content(N_oct2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_oct2019 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2019-10.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_oct2019$startDateTime <- as.POSIXct(N_oct2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_oct2019$endDateTime <- as.POSIXct(N_oct2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_oct2019$adj_N_mean <- (N_oct2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_oct2019, "N_oct2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_oct2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_oct2019.csv"
)

#### NOVEMBER 2019 ####

N_nov2019 <- GET(N_urls[grep("TECR/2019-11", N_urls)])
N_files <- jsonlite::fromJSON(content(N_nov2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_nov2019 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2019-11.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_nov2019$startDateTime <- as.POSIXct(N_nov2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_nov2019$endDateTime <- as.POSIXct(N_nov2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_nov2019$adj_N_mean <- (N_nov2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_nov2019, "N_nov2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_nov2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_nov2019.csv"
)

#### DECEMBER 2019 ####

N_dec2019 <- GET(N_urls[grep("TECR/2019-12", N_urls)])
N_files <- jsonlite::fromJSON(content(N_dec2019, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_dec2019 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2019-12.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_dec2019$startDateTime <- as.POSIXct(N_dec2019$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_dec2019$endDateTime <- as.POSIXct(N_dec2019$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_dec2019$adj_N_mean <- (N_dec2019$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_dec2019, "N_dec2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_dec2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_dec2019.csv"
)

#### JANURARY 2020 ####

N_jan2020 <- GET(N_urls[grep("TECR/2020-01", N_urls)])
N_files <- jsonlite::fromJSON(content(N_jan2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_jan2020 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2020-01.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_jan2020$startDateTime <- as.POSIXct(N_jan2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_jan2020$endDateTime <- as.POSIXct(N_jan2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_jan2020$adj_N_mean <- (N_jan2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_jan2020, "N_jan2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_jan2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_jan2020.csv"
)

#### FEBRUARY 2020 ####

N_feb2020 <- GET(N_urls[grep("TECR/2020-02", N_urls)])
N_files <- jsonlite::fromJSON(content(N_feb2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_feb2020 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2020-02.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_feb2020$startDateTime <- as.POSIXct(N_feb2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_feb2020$endDateTime <- as.POSIXct(N_feb2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_feb2020$adj_N_mean <- (N_feb2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_feb2020, "N_feb2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_feb2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_feb2020.csv"
)

#### MARCH 2020 ####

N_mar2020 <- GET(N_urls[grep("TECR/2020-03", N_urls)])
N_files <- jsonlite::fromJSON(content(N_mar2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_mar2020 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2020-03.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_mar2020$startDateTime <- as.POSIXct(N_mar2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_mar2020$endDateTime <- as.POSIXct(N_mar2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_mar2020$adj_N_mean <- (N_mar2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_mar2020, "N_mar2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_mar2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_mar2020.csv"
)

#### APRIL 2020 ####

N_apr2020 <- GET(N_urls[grep("TECR/2020-04", N_urls)])
N_files <- jsonlite::fromJSON(content(N_apr2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_apr2020 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2020-04.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_apr2020$startDateTime <- as.POSIXct(N_apr2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_apr2020$endDateTime <- as.POSIXct(N_apr2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_apr2020$adj_N_mean <- (N_apr2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_apr2020, "N_apr2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_apr2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_apr2020.csv"
)

#### MAY 2020 ####

N_may2020 <- GET(N_urls[grep("TECR/2020-05", N_urls)])
N_files <- jsonlite::fromJSON(content(N_may2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_may2020 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2020-05.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_may2020$startDateTime <- as.POSIXct(N_may2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_may2020$endDateTime <- as.POSIXct(N_may2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_may2020$adj_N_mean <- (N_may2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_may2020, "N_may2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_may2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_may2020.csv"
)

#### JUNE 2020 ####

N_june2020 <- GET(N_urls[grep("TECR/2020-06", N_urls)])
N_files <- jsonlite::fromJSON(content(N_june2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_june2020 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2020-06.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_june2020$startDateTime <- as.POSIXct(N_june2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_june2020$endDateTime <- as.POSIXct(N_june2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_june2020$adj_N_mean <- (N_june2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_june2020, "N_june2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_june2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_june2020.csv"
)

#### JULY 2020 ####

N_july2020 <- GET(N_urls[grep("TECR/2020-07", N_urls)])
N_files <- jsonlite::fromJSON(content(N_july2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_july2020 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2020-07.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_july2020$startDateTime <- as.POSIXct(N_july2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_july2020$endDateTime <- as.POSIXct(N_july2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_july2020$adj_N_mean <- (N_july2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_july2020, "N_july2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_july2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_july2020.csv"
)

#### AUGUST 2020 ####

N_aug2020 <- GET(N_urls[grep("TECR/2020-08", N_urls)])
N_files <- jsonlite::fromJSON(content(N_aug2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_aug2020 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2020-08.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_aug2020$startDateTime <- as.POSIXct(N_aug2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_aug2020$endDateTime <- as.POSIXct(N_aug2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_aug2020$adj_N_mean <- (N_aug2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_aug2020, "N_aug2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_aug2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_aug2020.csv"
)

#### SEPTEMBER 2020 ####

N_sept2020 <- GET(N_urls[grep("TECR/2020-09", N_urls)])
N_files <- jsonlite::fromJSON(content(N_sept2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_sept2020 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2020-09.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_sept2020$startDateTime <- as.POSIXct(N_sept2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_sept2020$endDateTime <- as.POSIXct(N_sept2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_sept2020$adj_N_mean <- (N_sept2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_sept2020, "N_sept2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_sept2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_sept2020.csv"
)

#### OCTOBER 2020 ####

N_oct2020 <- GET(N_urls[grep("TECR/2020-10", N_urls)])
N_files <- jsonlite::fromJSON(content(N_oct2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_oct2020 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2020-10.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_oct2020$startDateTime <- as.POSIXct(N_oct2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_oct2020$endDateTime <- as.POSIXct(N_oct2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_oct2020$adj_N_mean <- (N_oct2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_oct2020, "N_oct2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_oct2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_oct2020.csv"
)

#### NOVEMBER 2020 ####

N_nov2020 <- GET(N_urls[grep("TECR/2020-11", N_urls)])
N_files <- jsonlite::fromJSON(content(N_nov2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_nov2020 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2020-11.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_nov2020$startDateTime <- as.POSIXct(N_nov2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_nov2020$endDateTime <- as.POSIXct(N_nov2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_nov2020$adj_N_mean <- (N_nov2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_nov2020, "N_nov2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_nov2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_nov2020.csv"
)

#### DECEMBER 2020 ####

N_dec2020 <- GET(N_urls[grep("TECR/2020-12", N_urls)])
N_files <- jsonlite::fromJSON(content(N_dec2020, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_dec2020 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2020-12.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_dec2020$startDateTime <- as.POSIXct(N_dec2020$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_dec2020$endDateTime <- as.POSIXct(N_dec2020$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_dec2020$adj_N_mean <- (N_dec2020$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_dec2020, "N_dec2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_dec2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_dec2020.csv"
) 

#### JANUARY 2021 ####

N_jan2021 <- GET(N_urls[grep("TECR/2021-01", N_urls)])
N_files <- jsonlite::fromJSON(content(N_jan2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_jan2021 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2021-01.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_jan2021$startDateTime <- as.POSIXct(N_jan2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_jan2021$endDateTime <- as.POSIXct(N_jan2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_jan2021$adj_N_mean <- (N_jan2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_jan2021, "N_jan2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_jan2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_jan2021.csv"
) 

#### FEBRUARY 2021 ####

N_feb2021 <- GET(N_urls[grep("TECR/2021-02", N_urls)])
N_files <- jsonlite::fromJSON(content(N_feb2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_feb2021 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2021-02.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_feb2021$startDateTime <- as.POSIXct(N_feb2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_feb2021$endDateTime <- as.POSIXct(N_feb2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_feb2021$adj_N_mean <- (N_feb2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_feb2021, "N_feb2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_feb2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_feb2021.csv"
) 

#### MARCH 2021 ####

N_mar2021 <- GET(N_urls[grep("TECR/2021-03", N_urls)])
N_files <- jsonlite::fromJSON(content(N_mar2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_mar2021 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2021-03.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_mar2021$startDateTime <- as.POSIXct(N_mar2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_mar2021$endDateTime <- as.POSIXct(N_mar2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_mar2021$adj_N_mean <- (N_mar2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_mar2021, "N_mar2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_mar2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_mar2021.csv"
) 

#### APRIL 2021 ####

N_apr2021 <- GET(N_urls[grep("TECR/2021-04", N_urls)])
N_files <- jsonlite::fromJSON(content(N_apr2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_apr2021 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2021-04.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_apr2021$startDateTime <- as.POSIXct(N_apr2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_apr2021$endDateTime <- as.POSIXct(N_apr2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_apr2021$adj_N_mean <- (N_apr2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_apr2021, "N_apr2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_apr2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_apr2021.csv"
) 

#### MAY 2021 ####

N_may2021 <- GET(N_urls[grep("TECR/2021-05", N_urls)])
N_files <- jsonlite::fromJSON(content(N_may2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_may2021 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2021-05.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_may2021$startDateTime <- as.POSIXct(N_may2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_may2021$endDateTime <- as.POSIXct(N_may2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_may2021$adj_N_mean <- (N_may2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_may2021, "N_may2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_may2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_may2021.csv"
) 

#### JUNE 2021 ####

N_june2021 <- GET(N_urls[grep("TECR/2021-06", N_urls)])
N_files <- jsonlite::fromJSON(content(N_june2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_june2021 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2021-06.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_june2021$startDateTime <- as.POSIXct(N_june2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_june2021$endDateTime <- as.POSIXct(N_june2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_june2021$adj_N_mean <- (N_june2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_june2021, "N_june2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_june2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_june2021.csv"
) 

#### JULY 2021 ####

N_july2021 <- GET(N_urls[grep("TECR/2021-07", N_urls)])
N_files <- jsonlite::fromJSON(content(N_july2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_july2021 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2021-07.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_july2021$startDateTime <- as.POSIXct(N_july2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_july2021$endDateTime <- as.POSIXct(N_july2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_july2021$adj_N_mean <- (N_july2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_july2021, "N_july2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_july2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_july2021.csv"
) 

#### AUGUST 2021 ####

N_aug2021 <- GET(N_urls[grep("TECR/2021-08", N_urls)])
N_files <- jsonlite::fromJSON(content(N_aug2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_aug2021 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2021-08.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_aug2021$startDateTime <- as.POSIXct(N_aug2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_aug2021$endDateTime <- as.POSIXct(N_aug2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_aug2021$adj_N_mean <- (N_aug2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_aug2021, "N_aug2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_aug2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_aug2021.csv"
) 

#### SEPTEMBER 2021 ####

N_sept2021 <- GET(N_urls[grep("TECR/2021-09", N_urls)])
N_files <- jsonlite::fromJSON(content(N_sept2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_sept2021 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2021-09.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_sept2021$startDateTime <- as.POSIXct(N_sept2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_sept2021$endDateTime <- as.POSIXct(N_sept2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_sept2021$adj_N_mean <- (N_sept2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_sept2021, "N_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_sept2021.csv"
) 

#### OCTOBER 2021 ####

N_oct2021 <- GET(N_urls[grep("TECR/2021-10", N_urls)])
N_files <- jsonlite::fromJSON(content(N_oct2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_oct2021 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2021-10.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_oct2021$startDateTime <- as.POSIXct(N_oct2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_oct2021$endDateTime <- as.POSIXct(N_oct2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_oct2021$adj_N_mean <- (N_oct2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_oct2021, "N_oct2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_oct2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_oct2021.csv"
) 

#### NOVEMBER 2021 ####

N_nov2021 <- GET(N_urls[grep("TECR/2021-11", N_urls)])
N_files <- jsonlite::fromJSON(content(N_nov2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_nov2021 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2021-11.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_nov2021$startDateTime <- as.POSIXct(N_nov2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_nov2021$endDateTime <- as.POSIXct(N_nov2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_nov2021$adj_N_mean <- (N_nov2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_nov2021, "N_nov2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_nov2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_nov2021.csv"
) 

#### DECEMBER 2021 ####

N_dec2021 <- GET(N_urls[grep("TECR/2021-12", N_urls)])
N_files <- jsonlite::fromJSON(content(N_dec2021, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_dec2021 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2021-12.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_dec2021$startDateTime <- as.POSIXct(N_dec2021$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_dec2021$endDateTime <- as.POSIXct(N_dec2021$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_dec2021$adj_N_mean <- (N_dec2021$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_dec2021, "N_dec2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_dec2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_dec2021.csv"
) 

#### JANUARY 2022 ####

N_jan2022 <- GET(N_urls[grep("TECR/2022-01", N_urls)])
N_files <- jsonlite::fromJSON(content(N_jan2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_jan2022 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2022-01.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_jan2022$startDateTime <- as.POSIXct(N_jan2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_jan2022$endDateTime <- as.POSIXct(N_jan2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_jan2022$adj_N_mean <- (N_jan2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_jan2022, "N_jan2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_jan2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_jan2022.csv"
) 

#### FEBRUARY 2022 ####

N_feb2022 <- GET(N_urls[grep("TECR/2022-02", N_urls)])
N_files <- jsonlite::fromJSON(content(N_feb2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_feb2022 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2022-02.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_feb2022$startDateTime <- as.POSIXct(N_feb2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_feb2022$endDateTime <- as.POSIXct(N_feb2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_feb2022$adj_N_mean <- (N_feb2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_feb2022, "N_feb2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_feb2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_feb2022.csv"
) 

#### MARCH 2022 ####

N_mar2022 <- GET(N_urls[grep("TECR/2022-03", N_urls)])
N_files <- jsonlite::fromJSON(content(N_mar2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_mar2022 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2022-03.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_mar2022$startDateTime <- as.POSIXct(N_mar2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_mar2022$endDateTime <- as.POSIXct(N_mar2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_mar2022$adj_N_mean <- (N_mar2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_mar2022, "N_mar2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_mar2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_mar2022.csv"
) 

#### APRIL 2022 ####

N_apr2022 <- GET(N_urls[grep("TECR/2022-04", N_urls)])
N_files <- jsonlite::fromJSON(content(N_apr2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_apr2022 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2022-04.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_apr2022$startDateTime <- as.POSIXct(N_apr2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_apr2022$endDateTime <- as.POSIXct(N_apr2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_apr2022$adj_N_mean <- (N_apr2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_apr2022, "N_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_apr2022.csv"
) 

#### MAY 2022 ####

N_may2022 <- GET(N_urls[grep("TECR/2022-05", N_urls)])
N_files <- jsonlite::fromJSON(content(N_may2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_may2022 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2022-05.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_may2022$startDateTime <- as.POSIXct(N_may2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_may2022$endDateTime <- as.POSIXct(N_may2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_may2022$adj_N_mean <- (N_may2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_may2022, "N_may2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_may2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_may2022.csv"
) 

#### JUNE 2022 ####

N_june2022 <- GET(N_urls[grep("TECR/2022-06", N_urls)])
N_files <- jsonlite::fromJSON(content(N_june2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_june2022 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2022-06.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_june2022$startDateTime <- as.POSIXct(N_june2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_june2022$endDateTime <- as.POSIXct(N_june2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_june2022$adj_N_mean <- (N_june2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_june2022, "N_june2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_june2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_june2022.csv"
) 

#### JULY 2022 ####

N_july2022 <- GET(N_urls[grep("TECR/2022-07", N_urls)])
N_files <- jsonlite::fromJSON(content(N_july2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_july2022 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2022-07.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_july2022$startDateTime <- as.POSIXct(N_july2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_july2022$endDateTime <- as.POSIXct(N_july2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_july2022$adj_N_mean <- (N_july2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_july2022, "N_july2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_july2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_july2022.csv"
) 

#### AUGUST 2022 ####

N_aug2022 <- GET(N_urls[grep("TECR/2022-08", N_urls)])
N_files <- jsonlite::fromJSON(content(N_aug2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_aug2022 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2022-08.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_aug2022$startDateTime <- as.POSIXct(N_aug2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_aug2022$endDateTime <- as.POSIXct(N_aug2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_aug2022$adj_N_mean <- (N_aug2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_aug2022, "N_aug2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_aug2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_aug2022.csv"
) 

#### SEPTEMBER 2022 ####

N_sept2022 <- GET(N_urls[grep("TECR/2022-09", N_urls)])
N_files <- jsonlite::fromJSON(content(N_sept2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_sept2022 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2022-09.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_sept2022$startDateTime <- as.POSIXct(N_sept2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_sept2022$endDateTime <- as.POSIXct(N_sept2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_sept2022$adj_N_mean <- (N_sept2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_sept2022, "N_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_sept2022.csv"
) 

#### OCTOBER 2022 ####

N_oct2022 <- GET(N_urls[grep("TECR/2022-10", N_urls)])
N_files <- jsonlite::fromJSON(content(N_oct2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_oct2022 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2022-10.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_oct2022$startDateTime <- as.POSIXct(N_oct2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_oct2022$endDateTime <- as.POSIXct(N_oct2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_oct2022$adj_N_mean <- (N_oct2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_oct2022, "N_oct2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_oct2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_oct2022.csv"
) 

#### NOVEMBER 2022 ####

N_nov2022 <- GET(N_urls[grep("TECR/2022-11", N_urls)])
N_files <- jsonlite::fromJSON(content(N_nov2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_nov2022 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2022-11.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_nov2022$startDateTime <- as.POSIXct(N_nov2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_nov2022$endDateTime <- as.POSIXct(N_nov2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_nov2022$adj_N_mean <- (N_nov2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_nov2022, "N_nov2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_nov2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_nov2022.csv"
) 

#### DECEMBER 2022 ####

N_dec2022 <- GET(N_urls[grep("TECR/2022-12", N_urls)])
N_files <- jsonlite::fromJSON(content(N_dec2022, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_dec2022 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2022-12.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_dec2022$startDateTime <- as.POSIXct(N_dec2022$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_dec2022$endDateTime <- as.POSIXct(N_dec2022$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_dec2022$adj_N_mean <- (N_dec2022$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_dec2022, "N_dec2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_dec2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_dec2022.csv"
) 

#### JANUARY 2023 ####

N_jan2023 <- GET(N_urls[grep("TECR/2023-01", N_urls)])
N_files <- jsonlite::fromJSON(content(N_jan2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_jan2023 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2023-01.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_jan2023$startDateTime <- as.POSIXct(N_jan2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_jan2023$endDateTime <- as.POSIXct(N_jan2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_jan2023$adj_N_mean <- (N_jan2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_jan2023, "N_jan2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_jan2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_jan2023.csv"
) 

#### FEBRUARY 2023 ####

N_feb2023 <- GET(N_urls[grep("TECR/2023-02", N_urls)])
N_files <- jsonlite::fromJSON(content(N_feb2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_feb2023 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2023-02.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_feb2023$startDateTime <- as.POSIXct(N_feb2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_feb2023$endDateTime <- as.POSIXct(N_feb2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_feb2023$adj_N_mean <- (N_feb2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_feb2023, "N_feb2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_feb2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_feb2023.csv"
) 

#### MARCH 2023 ####

N_mar2023 <- GET(N_urls[grep("TECR/2023-03", N_urls)])
N_files <- jsonlite::fromJSON(content(N_mar2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_mar2023 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2023-03.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_mar2023$startDateTime <- as.POSIXct(N_mar2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_mar2023$endDateTime <- as.POSIXct(N_mar2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_mar2023$adj_N_mean <- (N_mar2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_mar2023, "N_mar2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_mar2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_mar2023.csv"
) 

#### APRIL 2023 ####

N_apr2023 <- GET(N_urls[grep("TECR/2023-04", N_urls)])
N_files <- jsonlite::fromJSON(content(N_apr2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_apr2023 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2023-04.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_apr2023$startDateTime <- as.POSIXct(N_apr2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_apr2023$endDateTime <- as.POSIXct(N_apr2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_apr2023$adj_N_mean <- (N_apr2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_apr2023, "N_apr2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_apr2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_apr2023.csv"
) 

#### MAY 2023 ####

N_may2023 <- GET(N_urls[grep("TECR/2023-05", N_urls)])
N_files <- jsonlite::fromJSON(content(N_may2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_may2023 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2023-05.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_may2023$startDateTime <- as.POSIXct(N_may2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_may2023$endDateTime <- as.POSIXct(N_may2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_may2023$adj_N_mean <- (N_may2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_may2023, "N_may2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_may2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_may2023.csv"
) 

#### JUNE 2023 ####

N_june2023 <- GET(N_urls[grep("TECR/2023-06", N_urls)])
N_files <- jsonlite::fromJSON(content(N_june2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_june2023 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2023-06.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_june2023$startDateTime <- as.POSIXct(N_june2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_june2023$endDateTime <- as.POSIXct(N_june2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_june2023$adj_N_mean <- (N_june2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_june2023, "N_june2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_june2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_june2023.csv"
) 

#### JULY 2023 ####

N_july2023 <- GET(N_urls[grep("TECR/2023-07", N_urls)])
N_files <- jsonlite::fromJSON(content(N_july2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_july2023 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2023-07.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_july2023$startDateTime <- as.POSIXct(N_july2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_july2023$endDateTime <- as.POSIXct(N_july2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_july2023$adj_N_mean <- (N_july2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_july2023, "N_july2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_july2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_july2023.csv"
) 

#### AUGUST 2023 ####

N_aug2023 <- GET(N_urls[grep("TECR/2023-08", N_urls)])
N_files <- jsonlite::fromJSON(content(N_aug2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_aug2023 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2023-08.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_aug2023$startDateTime <- as.POSIXct(N_aug2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_aug2023$endDateTime <- as.POSIXct(N_aug2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_aug2023$adj_N_mean <- (N_aug2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_aug2023, "N_aug2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_aug2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_aug2023.csv"
) 

#### SEPTEMBER 2023 ####

N_sept2023 <- GET(N_urls[grep("TECR/2023-09", N_urls)])
N_files <- jsonlite::fromJSON(content(N_sept2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_sept2023 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2023-09.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_sept2023$startDateTime <- as.POSIXct(N_sept2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_sept2023$endDateTime <- as.POSIXct(N_sept2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_sept2023$adj_N_mean <- (N_sept2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_sept2023, "N_sept2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_sept2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_sept2023.csv"
) 

#### OCTOBER 2023 ####

N_oct2023 <- GET(N_urls[grep("TECR/2023-10", N_urls)])
N_files <- jsonlite::fromJSON(content(N_oct2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_oct2023 <- read.csv(N_files$data$files$url
                       [grep("NSW_15_minute.2023-10.basic.",
                             N_files$data$files$name)])

# reformat Date and Time
N_oct2023$startDateTime <- as.POSIXct(N_oct2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_oct2023$endDateTime <- as.POSIXct(N_oct2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_oct2023$adj_N_mean <- (N_oct2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_oct2023, "N_oct2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_oct2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_oct2023.csv"
) 

#### NOVEMBER 2023 ####

N_nov2023 <- GET(N_urls[grep("TECR/2023-11", N_urls)])
N_files <- jsonlite::fromJSON(content(N_nov2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_nov2023 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2023-11.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_nov2023$startDateTime <- as.POSIXct(N_nov2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_nov2023$endDateTime <- as.POSIXct(N_nov2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_nov2023$adj_N_mean <- (N_nov2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_nov2023, "N_nov2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_nov2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_nov2023.csv"
) 

#### DECEMBER 2023 ####

N_dec2023 <- GET(N_urls[grep("TECR/2023-12", N_urls)])
N_files <- jsonlite::fromJSON(content(N_dec2023, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_dec2023 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2023-12.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_dec2023$startDateTime <- as.POSIXct(N_dec2023$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_dec2023$endDateTime <- as.POSIXct(N_dec2023$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_dec2023$adj_N_mean <- (N_dec2023$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_dec2023, "N_dec2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_dec2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1R-45SX7ckJCuWaRVA-xUE5504VI5P3Jz"),
  name = "N_dec2023.csv"
) 

#### JANUARY 2024 ####
N_jan2024 <- GET(N_urls[grep("TECR/2024-01", N_urls)])
N_files <- jsonlite::fromJSON(content(N_jan2024, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_jan2024 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2024-01.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_jan2024$startDateTime <- as.POSIXct(N_jan2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_jan2024$endDateTime <- as.POSIXct(N_jan2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_jan2024$adj_N_mean <- (N_jan2024$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_jan2024, "N_jan2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_jan2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1lBqEmFXTIRHO-vXIUcPJMSGOdBNzpJy7"),
  name = "N_jan2024.csv"
) 

#### FEBRUARY 2024 ####
N_feb2024 <- GET(N_urls[grep("TECR/2024-02", N_urls)])
N_files <- jsonlite::fromJSON(content(N_feb2024, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_feb2024 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2024-02.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_feb2024$startDateTime <- as.POSIXct(N_feb2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_feb2024$endDateTime <- as.POSIXct(N_feb2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_feb2024$adj_N_mean <- (N_feb2024$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_feb2024, "N_feb2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_feb2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1lBqEmFXTIRHO-vXIUcPJMSGOdBNzpJy7"),
  name = "N_feb2024.csv"
) 

#### MARCH 2024 ####
N_mar2024 <- GET(N_urls[grep("TECR/2024-03", N_urls)])
N_files <- jsonlite::fromJSON(content(N_mar2024, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_mar2024 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2024-03.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_mar2024$startDateTime <- as.POSIXct(N_mar2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_mar2024$endDateTime <- as.POSIXct(N_mar2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_mar2024$adj_N_mean <- (N_mar2024$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_mar2024, "N_mar2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_mar2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1lBqEmFXTIRHO-vXIUcPJMSGOdBNzpJy7"),
  name = "N_mar2024.csv"
) 
#### APRIL 2024 ####
N_apr2024 <- GET(N_urls[grep("TECR/2024-04", N_urls)])
N_files <- jsonlite::fromJSON(content(N_apr2024, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_apr2024 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2024-04.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_apr2024$startDateTime <- as.POSIXct(N_apr2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_apr2024$endDateTime <- as.POSIXct(N_apr2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_apr2024$adj_N_mean <- (N_apr2024$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_apr2024, "N_apr2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_apr2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1lBqEmFXTIRHO-vXIUcPJMSGOdBNzpJy7"),
  name = "N_apr2024.csv"
) 
#### MAY 2024 ####
N_may2024 <- GET(N_urls[grep("TECR/2024-05", N_urls)])
N_files <- jsonlite::fromJSON(content(N_may2024, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_may2024 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2024-05.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_may2024$startDateTime <- as.POSIXct(N_may2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_may2024$endDateTime <- as.POSIXct(N_may2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_may2024$adj_N_mean <- (N_may2024$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_may2024, "N_may2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_may2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1lBqEmFXTIRHO-vXIUcPJMSGOdBNzpJy7"),
  name = "N_may2024.csv"
) 
#### JUNE 2024 ####
N_june2024 <- GET(N_urls[grep("TECR/2024-06", N_urls)])
N_files <- jsonlite::fromJSON(content(N_june2024, as = "text"))
N_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
N_june2024 <- read.csv(N_files$data$files$url
                      [grep("NSW_15_minute.2024-06.basic.",
                            N_files$data$files$name)])

# reformat Date and Time
N_june2024$startDateTime <- as.POSIXct(N_june2024$startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")
N_june2024$endDateTime <- as.POSIXct(N_june2024$endDateTime, format = "%Y-%m-%dT%H:%M:%SZ")

# add column that is the adjusted value of N, from micromoles/liter to milligrams/liter 
# using the molecular weight of Nitrate: 62.0049, divided by 1000
N_june2024$adj_N_mean <- (N_june2024$surfWaterNitrateMean * 62.0049)/1000

## Save the raw file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(N_june2024, "N_june2024.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "N_june2024.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1lBqEmFXTIRHO-vXIUcPJMSGOdBNzpJy7"),
  name = "N_june2024.csv"
) 
#### Read me ####
# -- As of 12/04/2025, all data after June 2024 were "provisional," and because 
# of this were not downloaded into the Google Drive.