#### Read me ####
# -- The following code is what was used to access gases data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Gauge height." 

# Abstract: Gauge height is the height of the water surface above an established 
# altitude where the stage is zero. The zero level is arbitrary, but is often close 
# to the streambed. Gauge height at stream sites will be related to measurements 
# of stream discharge to formulate stage-discharge rating curves at all NEON stream 
# and river sites. Rating equations will be applied to continuous surface water 
# elevation data in order to calculate continuous stream discharge at all NEON aquatic sites. 
# For additional details, see protocol NEON.DOC. 001085: AOS Protocol and 
# Procedure: Stream Discharge, NEON.DOC.001646: General AQU Field Metadata Sheet, 
# and NEON.DOC.001152: Aquatic Sampling Design.

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
# data product name: "Gauge height" (DP1.20267.001)
gauge_product <- GET("http://data.neonscience.org/api/v0/products/DP1.20267.001")
gauge_product

# make the data readable by jsonlite
gauge_text <- content(gauge_product, as = "text")

# flatten json into a nested list
gauge_avail <- jsonlite::fromJSON(gauge_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
gauge_avail$data$siteCodes

# TECR is 30th on the list
gauge_avail$data$siteCodes$siteCode[[30]]

# check which months at TECR have available data
gauge_avail$data$siteCodes$availableMonths[[30]]

# get a complete list of available data URLs
gauge_urls <- unlist(gauge_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(gauge_urls)

# show the first 10 URLs available
gauge_urls[1:10]

#### MAY 2017 ####
gauge_may2017 <- GET(gauge_urls[grep("TECR/2017-05", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_may2017, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_may2017 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2017-05.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_may2017$startDate <- as.POSIXct(gauge_may2017$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_may2017$endDate <- as.POSIXct(gauge_may2017$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_may2017, "gauge_may2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_may2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_may2017.csv"
)

#### JUNE 2017 ####
gauge_june2017 <- GET(gauge_urls[grep("TECR/2017-06", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_june2017, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_june2017 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2017-06.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_june2017$startDate <- as.POSIXct(gauge_june2017$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_june2017$endDate <- as.POSIXct(gauge_june2017$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_june2017, "gauge_june2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_june2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_june2017.csv"
)

#### JULY 2017 ####
gauge_july2017 <- GET(gauge_urls[grep("TECR/2017-07", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_july2017, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_july2017 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2017-07.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_july2017$startDate <- as.POSIXct(gauge_july2017$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_july2017$endDate <- as.POSIXct(gauge_july2017$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_july2017, "gauge_july2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_july2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_july2017.csv"
)

#### AUGUST 2017 ####
gauge_aug2017 <- GET(gauge_urls[grep("TECR/2017-08", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_aug2017, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_aug2017 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2017-08.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_aug2017$startDate <- as.POSIXct(gauge_aug2017$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_aug2017$endDate <- as.POSIXct(gauge_aug2017$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_aug2017, "gauge_aug2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_aug2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_aug2017.csv"
)

#### SEPTEMBER 2017 ####
gauge_sept2017 <- GET(gauge_urls[grep("TECR/2017-09", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_sept2017, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_sept2017 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2017-09.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_sept2017$startDate <- as.POSIXct(gauge_sept2017$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_sept2017$endDate <- as.POSIXct(gauge_sept2017$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_sept2017, "gauge_sept2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_sept2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_sept2017.csv"
)

#### OCTOBER 2017 ####
gauge_oct2017 <- GET(gauge_urls[grep("TECR/2017-10", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_oct2017, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_oct2017 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2017-10.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_oct2017$startDate <- as.POSIXct(gauge_oct2017$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_oct2017$endDate <- as.POSIXct(gauge_oct2017$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_oct2017, "gauge_oct2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_oct2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_oct2017.csv"
)

#### NOVEMBER 2017 ####
gauge_nov2017 <- GET(gauge_urls[grep("TECR/2017-11", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_nov2017, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_nov2017 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2017-11.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_nov2017$startDate <- as.POSIXct(gauge_nov2017$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_nov2017$endDate <- as.POSIXct(gauge_nov2017$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_nov2017, "gauge_nov2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_nov2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_nov2017.csv"
)

#### DECEMBER 2017 ####
gauge_dec2017 <- GET(gauge_urls[grep("TECR/2017-12", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_dec2017, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_dec2017 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2017-12.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_dec2017$startDate <- as.POSIXct(gauge_dec2017$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_dec2017$endDate <- as.POSIXct(gauge_dec2017$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_dec2017, "gauge_dec2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_dec2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_dec2017.csv"
)

#### JANUARY 2018 ####
gauge_jan2018 <- GET(gauge_urls[grep("TECR/2018-01", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_jan2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_jan2018 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2018-01.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_jan2018$startDate <- as.POSIXct(gauge_jan2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_jan2018$endDate <- as.POSIXct(gauge_jan2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_jan2018, "gauge_jan2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_jan2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_jan2018.csv"
)

#### FEBRUARY 2018 ####
gauge_feb2018 <- GET(gauge_urls[grep("TECR/2018-02", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_feb2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_feb2018 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2018-02.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_feb2018$startDate <- as.POSIXct(gauge_feb2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_feb2018$endDate <- as.POSIXct(gauge_feb2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_feb2018, "gauge_feb2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_feb2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_feb2018.csv"
)

#### MARCH 2018 ####
gauge_mar2018 <- GET(gauge_urls[grep("TECR/2018-03", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_mar2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_mar2018 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2018-03.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_mar2018$startDate <- as.POSIXct(gauge_mar2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_mar2018$endDate <- as.POSIXct(gauge_mar2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_mar2018, "gauge_mar2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_mar2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_mar2018.csv"
)

#### APRIL 2018 ####
gauge_apr2018 <- GET(gauge_urls[grep("TECR/2018-04", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_apr2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_apr2018 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2018-04.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_apr2018$startDate <- as.POSIXct(gauge_apr2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_apr2018$endDate <- as.POSIXct(gauge_apr2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_apr2018, "gauge_apr2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_apr2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_apr2018.csv"
)

#### MAY 2018 ####
gauge_may2018 <- GET(gauge_urls[grep("TECR/2018-05", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_may2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_may2018 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2018-05.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_may2018$startDate <- as.POSIXct(gauge_may2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_may2018$endDate <- as.POSIXct(gauge_may2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_may2018, "gauge_may2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_may2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_may2018.csv"
)

#### JUNE 2018 ####
gauge_june2018 <- GET(gauge_urls[grep("TECR/2018-06", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_june2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_june2018 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2018-06.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_june2018$startDate <- as.POSIXct(gauge_june2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_june2018$endDate <- as.POSIXct(gauge_june2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_june2018, "gauge_june2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_june2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_june2018.csv"
)

#### JULY 2018 ####
gauge_july2018 <- GET(gauge_urls[grep("TECR/2018-07", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_july2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_july2018 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2018-07.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_july2018$startDate <- as.POSIXct(gauge_july2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_july2018$endDate <- as.POSIXct(gauge_july2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_july2018, "gauge_july2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_july2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_july2018.csv"
)

#### AUGUST 2018 ####
gauge_aug2018 <- GET(gauge_urls[grep("TECR/2018-08", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_aug2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_aug2018 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2018-08.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_aug2018$startDate <- as.POSIXct(gauge_aug2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_aug2018$endDate <- as.POSIXct(gauge_aug2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_aug2018, "gauge_aug2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_aug2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_aug2018.csv"
)

#### SEPTEMBER 2018 ####
gauge_sept2018 <- GET(gauge_urls[grep("TECR/2018-09", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_sept2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_sept2018 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2018-09.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_sept2018$startDate <- as.POSIXct(gauge_sept2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_sept2018$endDate <- as.POSIXct(gauge_sept2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_sept2018, "gauge_sept2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_sept2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_sept2018.csv"
)

#### OCTOBER 2018 ####
gauge_oct2018 <- GET(gauge_urls[grep("TECR/2018-10", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_oct2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_oct2018 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2018-10.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_oct2018$startDate <- as.POSIXct(gauge_oct2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_oct2018$endDate <- as.POSIXct(gauge_oct2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_oct2018, "gauge_oct2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_oct2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_oct2018.csv"
)

#### NOVEMBER 2018 ####
gauge_nov2018 <- GET(gauge_urls[grep("TECR/2018-11", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_nov2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_nov2018 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2018-11.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_nov2018$startDate <- as.POSIXct(gauge_nov2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_nov2018$endDate <- as.POSIXct(gauge_nov2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_nov2018, "gauge_nov2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_nov2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_nov2018.csv"
)

#### DECEMBER 2018 ####
gauge_dec2018 <- GET(gauge_urls[grep("TECR/2018-12", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_dec2018, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_dec2018 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2018-12.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_dec2018$startDate <- as.POSIXct(gauge_dec2018$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_dec2018$endDate <- as.POSIXct(gauge_dec2018$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_dec2018, "gauge_dec2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_dec2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_dec2018.csv"
)

#### JANUARY 2019 ####
gauge_jan2019 <- GET(gauge_urls[grep("TECR/2019-01", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_jan2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_jan2019 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2019-01.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_jan2019$startDate <- as.POSIXct(gauge_jan2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_jan2019$endDate <- as.POSIXct(gauge_jan2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_jan2019, "gauge_jan2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_jan2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_jan2019.csv"
)

#### FEBRUARY 2019 ####
# - no data

#### MARCH 2019 ####
gauge_mar2019 <- GET(gauge_urls[grep("TECR/2019-03", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_mar2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_mar2019 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2019-03.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_mar2019$startDate <- as.POSIXct(gauge_mar2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_mar2019$endDate <- as.POSIXct(gauge_mar2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_mar2019, "gauge_mar2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_mar2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_mar2019.csv"
)

#### APRIL 2019 ####
gauge_apr2019 <- GET(gauge_urls[grep("TECR/2019-04", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_apr2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_apr2019 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2019-04.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_apr2019$startDate <- as.POSIXct(gauge_apr2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_apr2019$endDate <- as.POSIXct(gauge_apr2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_apr2019, "gauge_apr2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_apr2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_apr2019.csv"
)

#### MAY 2019 ####
gauge_may2019 <- GET(gauge_urls[grep("TECR/2019-05", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_may2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_may2019 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2019-05.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_may2019$startDate <- as.POSIXct(gauge_may2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_may2019$endDate <- as.POSIXct(gauge_may2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_may2019, "gauge_may2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_may2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_may2019.csv"
)

#### JUNE 2019 ####
gauge_june2019 <- GET(gauge_urls[grep("TECR/2019-06", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_june2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_june2019 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2019-06.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_june2019$startDate <- as.POSIXct(gauge_june2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_june2019$endDate <- as.POSIXct(gauge_june2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_june2019, "gauge_june2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_june2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_june2019.csv"
)

#### JULY 2019 ####
gauge_july2019 <- GET(gauge_urls[grep("TECR/2019-07", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_july2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_july2019 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2019-07.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_july2019$startDate <- as.POSIXct(gauge_july2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_july2019$endDate <- as.POSIXct(gauge_july2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_july2019, "gauge_july2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_july2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_july2019.csv"
)

#### AUGUST 2019 ####
gauge_aug2019 <- GET(gauge_urls[grep("TECR/2019-08", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_aug2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_aug2019 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2019-08.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_aug2019$startDate <- as.POSIXct(gauge_aug2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_aug2019$endDate <- as.POSIXct(gauge_aug2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_aug2019, "gauge_aug2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_aug2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_aug2019.csv"
)

#### SEPTEMBER 2019 ####
gauge_sept2019 <- GET(gauge_urls[grep("TECR/2019-09", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_sept2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_sept2019 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2019-09.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_sept2019$startDate <- as.POSIXct(gauge_sept2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_sept2019$endDate <- as.POSIXct(gauge_sept2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_sept2019, "gauge_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_sept2019.csv"
)

#### OCTOBER 2019 ####
gauge_oct2019 <- GET(gauge_urls[grep("TECR/2019-10", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_oct2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_oct2019 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2019-10.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_oct2019$startDate <- as.POSIXct(gauge_oct2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_oct2019$endDate <- as.POSIXct(gauge_oct2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_oct2019, "gauge_oct2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_oct2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_oct2019.csv"
)

#### NOVEMBER 2019 ####
gauge_nov2019 <- GET(gauge_urls[grep("TECR/2019-11", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_nov2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_nov2019 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2019-11.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_nov2019$startDate <- as.POSIXct(gauge_nov2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_nov2019$endDate <- as.POSIXct(gauge_nov2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_nov2019, "gauge_nov2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_nov2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_nov2019.csv"
)

#### DECEMBER 2019 ####
gauge_dec2019 <- GET(gauge_urls[grep("TECR/2019-12", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_dec2019, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_dec2019 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2019-12.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_dec2019$startDate <- as.POSIXct(gauge_dec2019$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_dec2019$endDate <- as.POSIXct(gauge_dec2019$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_dec2019, "gauge_dec2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_dec2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_dec2019.csv"
)

#### JANUARY 2020 ####
gauge_jan2020 <- GET(gauge_urls[grep("TECR/2020-01", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_jan2020, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_jan2020 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2020-01.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_jan2020$startDate <- as.POSIXct(gauge_jan2020$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_jan2020$endDate <- as.POSIXct(gauge_jan2020$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_jan2020, "gauge_jan2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_jan2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_jan2020.csv"
)

#### FEBRUARY 2020 ####
gauge_feb2020 <- GET(gauge_urls[grep("TECR/2020-02", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_feb2020, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_feb2020 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2020-02.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_feb2020$startDate <- as.POSIXct(gauge_feb2020$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_feb2020$endDate <- as.POSIXct(gauge_feb2020$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_feb2020, "gauge_feb2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_feb2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_feb2020.csv"
)

#### MARCH 2020 ####
gauge_mar2020 <- GET(gauge_urls[grep("TECR/2020-03", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_mar2020, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_mar2020 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2020-03.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_mar2020$startDate <- as.POSIXct(gauge_mar2020$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_mar2020$endDate <- as.POSIXct(gauge_mar2020$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_mar2020, "gauge_mar2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_mar2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_mar2020.csv"
)

#### APRIL 2020 ####
# -- no data

#### MAY 2020 ####
# -- no data

#### JUNE ####
# -- no data

#### JULY 2020 ####
gauge_july2020 <- GET(gauge_urls[grep("TECR/2020-07", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_july2020, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_july2020 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2020-07.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_july2020$startDate <- as.POSIXct(gauge_july2020$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_july2020$endDate <- as.POSIXct(gauge_july2020$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_july2020, "gauge_july2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_july2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_july2020.csv"
)

#### AUGUST 2020 ####
# -- no data

#### SEPTEMBER 2020 ####
# -- no data

#### OCTOBER 2020 ####
gauge_oct2020 <- GET(gauge_urls[grep("TECR/2020-10", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_oct2020, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_oct2020 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2020-10.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_oct2020$startDate <- as.POSIXct(gauge_oct2020$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_oct2020$endDate <- as.POSIXct(gauge_oct2020$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_oct2020, "gauge_oct2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_oct2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_oct2020.csv"
)

#### NOVEMBER 2020 ####
gauge_nov2020 <- GET(gauge_urls[grep("TECR/2020-11", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_nov2020, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_nov2020 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2020-11.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_nov2020$startDate <- as.POSIXct(gauge_nov2020$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_nov2020$endDate <- as.POSIXct(gauge_nov2020$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_nov2020, "gauge_nov2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_nov2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_nov2020.csv"
)

#### DECEMBER 2020 ####
gauge_dec2020 <- GET(gauge_urls[grep("TECR/2020-12", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_dec2020, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_dec2020 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2020-12.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_dec2020$startDate <- as.POSIXct(gauge_dec2020$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_dec2020$endDate <- as.POSIXct(gauge_dec2020$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_dec2020, "gauge_dec2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_dec2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_dec2020.csv"
)

#### JANUARY 2021 ####
# -- no data

#### FEBRUARY 2021 ####
gauge_feb2021 <- GET(gauge_urls[grep("TECR/2021-02", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_feb2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_feb2021 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2021-02.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_feb2021$startDate <- as.POSIXct(gauge_feb2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_feb2021$endDate <- as.POSIXct(gauge_feb2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_feb2021, "gauge_feb2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_feb2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_feb2021.csv"
)

#### MARCH 2021 ####
gauge_mar2021 <- GET(gauge_urls[grep("TECR/2021-03", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_mar2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_mar2021 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2021-03.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_mar2021$startDate <- as.POSIXct(gauge_mar2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_mar2021$endDate <- as.POSIXct(gauge_mar2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_mar2021, "gauge_mar2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_mar2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_mar2021.csv"
)

#### APRIL 2021 ####
gauge_apr2021 <- GET(gauge_urls[grep("TECR/2021-04", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_apr2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_apr2021 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2021-04.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_apr2021$startDate <- as.POSIXct(gauge_apr2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_apr2021$endDate <- as.POSIXct(gauge_apr2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_apr2021, "gauge_apr2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_apr2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_apr2021.csv"
)

#### MAY 2021 ####
gauge_may2021 <- GET(gauge_urls[grep("TECR/2021-05", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_may2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_may2021 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2021-05.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_may2021$startDate <- as.POSIXct(gauge_may2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_may2021$endDate <- as.POSIXct(gauge_may2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_may2021, "gauge_may2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_may2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_may2021.csv"
)

#### JUNE 2021 ####
gauge_june2021 <- GET(gauge_urls[grep("TECR/2021-06", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_june2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_june2021 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2021-06.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_june2021$startDate <- as.POSIXct(gauge_june2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_june2021$endDate <- as.POSIXct(gauge_june2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_june2021, "gauge_june2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_june2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_june2021.csv"
)

#### JULY 2021 ####
gauge_july2021 <- GET(gauge_urls[grep("TECR/2021-07", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_july2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_july2021 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2021-07.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_july2021$startDate <- as.POSIXct(gauge_july2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_july2021$endDate <- as.POSIXct(gauge_july2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_july2021, "gauge_july2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_july2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_july2021.csv"
)

#### AUGUST 2021 ####
gauge_aug2021 <- GET(gauge_urls[grep("TECR/2021-08", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_aug2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_aug2021 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2021-08.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_aug2021$startDate <- as.POSIXct(gauge_aug2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_aug2021$endDate <- as.POSIXct(gauge_aug2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_aug2021, "gauge_aug2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_aug2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_aug2021.csv"
)

#### SEPTEMBER 2021 ####
gauge_sept2021 <- GET(gauge_urls[grep("TECR/2021-09", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_sept2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_sept2021 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2021-09.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_sept2021$startDate <- as.POSIXct(gauge_sept2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_sept2021$endDate <- as.POSIXct(gauge_sept2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_sept2021, "gauge_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_sept2021.csv"
)

#### OCTOBER 2021 ####
gauge_oct2021 <- GET(gauge_urls[grep("TECR/2021-10", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_oct2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_oct2021 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2021-10.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_oct2021$startDate <- as.POSIXct(gauge_oct2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_oct2021$endDate <- as.POSIXct(gauge_oct2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_oct2021, "gauge_oct2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_oct2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_oct2021.csv"
)

#### NOVEMBER 2021 ####
gauge_nov2021 <- GET(gauge_urls[grep("TECR/2021-11", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_nov2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_nov2021 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2021-11.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_nov2021$startDate <- as.POSIXct(gauge_nov2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_nov2021$endDate <- as.POSIXct(gauge_nov2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_nov2021, "gauge_nov2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_nov2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_nov2021.csv"
)

#### DECEMBER 2021 ####
gauge_dec2021 <- GET(gauge_urls[grep("TECR/2021-12", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_dec2021, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_dec2021 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2021-12.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_dec2021$startDate <- as.POSIXct(gauge_dec2021$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_dec2021$endDate <- as.POSIXct(gauge_dec2021$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_dec2021, "gauge_dec2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_dec2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_dec2021.csv"
)

#### JANUARY 2022 ####
gauge_jan2022 <- GET(gauge_urls[grep("TECR/2022-01", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_jan2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_jan2022 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2022-01.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_jan2022$startDate <- as.POSIXct(gauge_jan2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_jan2022$endDate <- as.POSIXct(gauge_jan2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_jan2022, "gauge_jan2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_jan2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_jan2022.csv"
)

#### FEBRUARY 2022 ####
gauge_feb2022 <- GET(gauge_urls[grep("TECR/2022-02", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_feb2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_feb2022 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2022-02.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_feb2022$startDate <- as.POSIXct(gauge_feb2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_feb2022$endDate <- as.POSIXct(gauge_feb2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_feb2022, "gauge_feb2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_feb2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_feb2022.csv"
)

#### MARCH 2022 ####
gauge_mar2022 <- GET(gauge_urls[grep("TECR/2022-03", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_mar2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_mar2022 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2022-03.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_mar2022$startDate <- as.POSIXct(gauge_mar2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_mar2022$endDate <- as.POSIXct(gauge_mar2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_mar2022, "gauge_mar2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_mar2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_mar2022.csv"
)

#### APRIL 2022 ####
gauge_apr2022 <- GET(gauge_urls[grep("TECR/2022-04", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_apr2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_apr2022 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2022-04.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_apr2022$startDate <- as.POSIXct(gauge_apr2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_apr2022$endDate <- as.POSIXct(gauge_apr2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_apr2022, "gauge_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_apr2022.csv"
)

#### MAY 2022 ####
gauge_may2022 <- GET(gauge_urls[grep("TECR/2022-05", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_may2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_may2022 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2022-05.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_may2022$startDate <- as.POSIXct(gauge_may2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_may2022$endDate <- as.POSIXct(gauge_may2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_may2022, "gauge_may2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_may2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_may2022.csv"
)

#### JUNE 2022 ####
gauge_june2022 <- GET(gauge_urls[grep("TECR/2022-06", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_june2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_june2022 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2022-06.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_june2022$startDate <- as.POSIXct(gauge_june2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_june2022$endDate <- as.POSIXct(gauge_june2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_june2022, "gauge_june2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_june2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_june2022.csv"
)

#### JULY 2022 ####
gauge_july2022 <- GET(gauge_urls[grep("TECR/2022-07", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_july2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_july2022 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2022-07.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_july2022$startDate <- as.POSIXct(gauge_july2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_july2022$endDate <- as.POSIXct(gauge_july2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_july2022, "gauge_july2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_july2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_july2022.csv"
)

#### AUGUST 2022 ####
gauge_aug2022 <- GET(gauge_urls[grep("TECR/2022-08", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_aug2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_aug2022 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2022-08.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_aug2022$startDate <- as.POSIXct(gauge_aug2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_aug2022$endDate <- as.POSIXct(gauge_aug2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_aug2022, "gauge_aug2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_aug2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_aug2022.csv"
)

#### SEPTEMBER 2022 ####
gauge_sept2022 <- GET(gauge_urls[grep("TECR/2022-09", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_sept2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_sept2022 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2022-09.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_sept2022$startDate <- as.POSIXct(gauge_sept2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_sept2022$endDate <- as.POSIXct(gauge_sept2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_sept2022, "gauge_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_sept2022.csv"
)

#### OCTOBER 2022 ####
gauge_oct2022 <- GET(gauge_urls[grep("TECR/2022-10", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_oct2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_oct2022 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2022-10.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_oct2022$startDate <- as.POSIXct(gauge_oct2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_oct2022$endDate <- as.POSIXct(gauge_oct2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_oct2022, "gauge_oct2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_oct2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_oct2022.csv"
)

#### NOVEMBER 2022 ####
gauge_nov2022 <- GET(gauge_urls[grep("TECR/2022-11", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_nov2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_nov2022 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2022-11.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_nov2022$startDate <- as.POSIXct(gauge_nov2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_nov2022$endDate <- as.POSIXct(gauge_nov2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_nov2022, "gauge_nov2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_nov2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_nov2022.csv"
)

#### DECEMBER 2022 ####
gauge_dec2022 <- GET(gauge_urls[grep("TECR/2022-12", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_dec2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_dec2022 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2022-12.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_dec2022$startDate <- as.POSIXct(gauge_dec2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_dec2022$endDate <- as.POSIXct(gauge_dec2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_dec2022, "gauge_dec2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_dec2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_dec2022.csv"
)

#### DECEMBER 2022 ####
gauge_dec2022 <- GET(gauge_urls[grep("TECR/2022-12", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_dec2022, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_dec2022 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2022-12.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_dec2022$startDate <- as.POSIXct(gauge_dec2022$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_dec2022$endDate <- as.POSIXct(gauge_dec2022$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_dec2022, "gauge_dec2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_dec2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_dec2022.csv"
)

#### JANUARY 2023 ####
# - no data

#### FEBRUARY 2023 ####
# - no data

#### MARCH 2023 ####
# - no data

#### APRIL 2023 ####
# - no data

#### MAY 2023 ####
# - no data

#### JUNE 2023 ####
gauge_june2023 <- GET(gauge_urls[grep("TECR/2023-06", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_june2023, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_june2023 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2023-06.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_june2023$startDate <- as.POSIXct(gauge_june2023$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_june2023$endDate <- as.POSIXct(gauge_june2023$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_june2023, "gauge_june2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_june2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_june2023.csv"
)

#### JULY 2023 ####
gauge_july2023 <- GET(gauge_urls[grep("TECR/2023-07", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_july2023, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_july2023 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2023-07.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_july2023$startDate <- as.POSIXct(gauge_july2023$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_july2023$endDate <- as.POSIXct(gauge_july2023$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_july2023, "gauge_july2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_july2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_july2023.csv"
)

#### AUGUST 2023 ####
gauge_aug2023 <- GET(gauge_urls[grep("TECR/2023-08", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_aug2023, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_aug2023 <- read.csv(gauge_files$data$files$url
                           [grep("gag_fieldData.2023-08.basic.",
                                 gauge_files$data$files$name)])

# reformat Date and Time
gauge_aug2023$startDate <- as.POSIXct(gauge_aug2023$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_aug2023$endDate <- as.POSIXct(gauge_aug2023$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_aug2023, "gauge_aug2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_aug2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_aug2023.csv"
)

#### SEPTEMBER 2023 ####
gauge_sept2023 <- GET(gauge_urls[grep("TECR/2023-09", gauge_urls)])
gauge_files <- jsonlite::fromJSON(content(gauge_sept2023, as = "text"))
gauge_files$data$files$name

# look at the basic (not expanded) data table for gauge height 
gauge_sept2023 <- read.csv(gauge_files$data$files$url
                          [grep("gag_fieldData.2023-09.basic.",
                                gauge_files$data$files$name)])

# reformat Date and Time
gauge_sept2023$startDate <- as.POSIXct(gauge_sept2023$startDate, format = "%Y-%m-%dT%H:%MZ")
gauge_sept2023$endDate <- as.POSIXct(gauge_sept2023$endDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gauge_sept2023, "gauge_sept2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gauge_sept2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T"),
  name = "gauge_sept2023.csv"
)

#### Read me ####
# # -- As of 12/08/2025, all data after September 2023 were "provisional."

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
gauge <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1zth-iYIf-fqCzh2V6I5gS2DbXD-nQR1T")

# List and filter CSV files with "N" in their names
gauge_files <- googledrive::drive_ls(path = gauge, type = "csv")
gauge_files <- gauge_files[grepl("gauge", gauge_files$name), ]

# Create an empty list to store the cleaned data frames
gauge_list <- lapply(seq_along(gauge_files$name), function(i) {
  googledrive::drive_download(
    file = gauge_files$id[i],
    path = paste0("googledrive/", gauge_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file
  read.csv(paste0("googledrive/", gauge_files$name[i]), header = TRUE)
})

# Assign names to the list elements based on the file names
names(gauge_list) <- gauge_files$name

# Check the contents of the list
str(gauge_list)

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
  file = "all_gaugeheight_data.csv",
  row.names = FALSE
)

#### Upload CSV to the specific Google Drive folder ####
folder_id <- drive_get("Gauge height")

drive_upload(
  "all_gaugeheight_data.csv",
  path = folder_id,
)
