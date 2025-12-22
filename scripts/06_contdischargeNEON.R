#### Read me ####
# -- The following code is what was used to access continuous discharge data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Continuous discharge." 

# Abstract: This data product describes the volume of water flowing through a 
# stream or river cross-section during a given period of time. For each NEON 
# stream or river site, site-specific stage-discharge rating curve equations 
# are derived from point observations of gauge height and discharge. 
# Continuous sensor measurements of surface water pressure are used to derive 
# water column height. The rating curve equations are applied to water column 
# height to derive continuous stream discharge.

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
# data product name: "Continuous Discharge" (DP4.00130.001)
contq_product <- GET("http://data.neonscience.org/api/v0/products/DP4.00130.001")
contq_product

# make the data readable by jsonlite
contq_text <- content(contq_product, as = "text")

# flatten json into a nested list
contq_avail <- jsonlite::fromJSON(contq_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
contq_avail$data$siteCodes

# TECR is 30th on the list
contq_avail$data$siteCodes$siteCode[[24]]

# check which months at TECR have available data
contq_avail$data$siteCodes$availableMonths[[24]]

# get a complete list of available data URLs
contq_urls <- unlist(contq_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(contq_urls)

# show the first 10 URLs available
contq_urls[1:10]

#### NOVEMBER 2018 ####
contq_nov2018 <- GET(contq_urls[grep("TECR/2018-11", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_nov2018, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_nov2018 <- read.csv(contq_files$data$files$url
                         [grep("csd_continuousDischarge.2018-11.basic.",
                               contq_files$data$files$name)])

# reformat Date and Time
contq_nov2018$endDate <- as.POSIXct(contq_nov2018$endDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2018$collectDate <- as.POSIXct(q_nov2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_nov2018, "contq_nov2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_nov2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_nov2018.csv"
)

#### DECEMBER 2018 ####
contq_dec2018 <- GET(contq_urls[grep("TECR/2018-12", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_dec2018, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_dec2018 <- read.csv(contq_files$data$files$url
                      [grep("csd_continuousDischarge.2018-12.basic.",
                            contq_files$data$files$name)])

# reformat Date and Time
contq_dec2018$endDate <- as.POSIXct(contq_dec2018$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_dec2018$collectDate <- as.POSIXct(contq_dec2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_dec2018, "contq_dec2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_dec2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1contqZ7O7nIpGZcontqxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_dec2018.csv"
)

#### JANUARY 2019 ####
contq_jan2019 <- GET(contq_urls[grep("TECR/2019-01", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_jan2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_jan2019 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2019-01.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_jan2019$endDate <- as.POSIXct(contq_jan2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_jan2019$collectDate <- as.POSIXct(contq_jan2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_jan2019, "contq_jan2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_jan2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_jan2019.csv"
)

#### FEBRUARY 2019 ####
contq_feb2019 <- GET(contq_urls[grep("TECR/2019-02", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_feb2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_feb2019 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2019-02.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_feb2019$endDate <- as.POSIXct(contq_feb2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_feb2019$collectDate <- as.POSIXct(contq_feb2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_feb2019, "contq_feb2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_feb2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_feb2019.csv"
)

#### MARCH 2019 ####
contq_mar2019 <- GET(contq_urls[grep("TECR/2019-03", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_mar2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_mar2019 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2019-03.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_mar2019$endDate <- as.POSIXct(contq_mar2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_mar2019$collectDate <- as.POSIXct(contq_mar2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_mar2019, "contq_mar2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_mar2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_mar2019.csv"
)

#### APRIL 2019 ####
contq_apr2019 <- GET(contq_urls[grep("TECR/2019-04", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_apr2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_apr2019 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2019-04.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_apr2019$endDate <- as.POSIXct(contq_apr2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_apr2019$collectDate <- as.POSIXct(contq_apr2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_apr2019, "contq_apr2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_apr2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_apr2019.csv"
)

#### MAY 2019 ####
contq_may2019 <- GET(contq_urls[grep("TECR/2019-05", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_may2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_may2019 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2019-05.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_may2019$endDate <- as.POSIXct(contq_may2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_may2019$collectDate <- as.POSIXct(contq_may2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_may2019, "contq_may2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_may2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_may2019.csv"
)

#### JUNE 2019 ####
contq_june2019 <- GET(contq_urls[grep("TECR/2019-06", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_june2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_june2019 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2019-06.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_june2019$endDate <- as.POSIXct(contq_june2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_june2019$collectDate <- as.POSIXct(contq_june2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_june2019, "contq_june2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_june2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_june2019.csv"
)

#### JULY 2019 ####
contq_july2019 <- GET(contq_urls[grep("TECR/2019-07", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_july2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_july2019 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2019-07.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_july2019$endDate <- as.POSIXct(contq_july2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_july2019$collectDate <- as.POSIXct(contq_july2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_july2019, "contq_july2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_july2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_july2019.csv"
)

#### AUGUST 2019 ####
contq_aug2019 <- GET(contq_urls[grep("TECR/2019-08", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_aug2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_aug2019 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2019-08.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_aug2019$endDate <- as.POSIXct(contq_aug2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_aug2019$collectDate <- as.POSIXct(contq_aug2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_aug2019, "contq_aug2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_aug2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_aug2019.csv"
)

#### SEPTEMBER 2019 ####
contq_sept2019 <- GET(contq_urls[grep("TECR/2019-09", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_sept2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_sept2019 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2019-09.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_sept2019$endDate <- as.POSIXct(contq_sept2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_sept2019$collectDate <- as.POSIXct(contq_sept2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_sept2019, "contq_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_sept2019.csv"
)

#### OCTOBER 2019 ####
contq_oct2019 <- GET(contq_urls[grep("TECR/2019-10", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_oct2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_oct2019 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2019-10.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_oct2019$endDate <- as.POSIXct(contq_oct2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_oct2019$collectDate <- as.POSIXct(contq_oct2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_oct2019, "contq_oct2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_oct2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_oct2019.csv"
)

#### NOVEMBER 2019 ####
contq_nov2019 <- GET(contq_urls[grep("TECR/2019-11", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_nov2019, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_nov2019 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2019-11.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_nov2019$endDate <- as.POSIXct(contq_nov2019$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_nov2019$collectDate <- as.POSIXct(contq_nov2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_nov2019, "contq_nov2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_nov2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_nov2019.csv"
)

#### DECEMBER 2019 ####
# -- no data

#### JANUARY 2020 ####
# -- no data

#### FEBRUARY 2020 ####
contq_feb2020 <- GET(contq_urls[grep("TECR/2020-02", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_feb2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_feb2020 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2020-02.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_feb2020$endDate <- as.POSIXct(contq_feb2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_feb2020$collectDate <- as.POSIXct(contq_feb2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_feb2020, "contq_feb2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_feb2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_feb2020.csv"
)

#### MARCH 2020 ####
contq_mar2020 <- GET(contq_urls[grep("TECR/2020-03", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_mar2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_mar2020 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2020-03.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_mar2020$endDate <- as.POSIXct(contq_mar2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_mar2020$collectDate <- as.POSIXct(contq_mar2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_mar2020, "contq_mar2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_mar2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_mar2020.csv"
)
#### APRIL 2020 ####
contq_apr2020 <- GET(contq_urls[grep("TECR/2020-04", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_apr2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_apr2020 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2020-04.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_apr2020$endDate <- as.POSIXct(contq_apr2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_apr2020$collectDate <- as.POSIXct(contq_apr2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_apr2020, "contq_apr2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_apr2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_apr2020.csv"
)
#### MAY 2020 ####
contq_may2020 <- GET(contq_urls[grep("TECR/2020-05", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_may2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_may2020 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2020-05.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_may2020$endDate <- as.POSIXct(contq_may2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_may2020$collectDate <- as.POSIXct(contq_may2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_may2020, "contq_may2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_may2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_may2020.csv"
)
#### JUNE 2020 ####
contq_june2020 <- GET(contq_urls[grep("TECR/2020-06", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_june2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_june2020 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2020-06.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_june2020$endDate <- as.POSIXct(contq_june2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_june2020$collectDate <- as.POSIXct(contq_june2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_june2020, "contq_june2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_june2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_june2020.csv"
)
#### JULY 2020 ####
contq_july2020 <- GET(contq_urls[grep("TECR/2020-07", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_july2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_july2020 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2020-07.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_july2020$endDate <- as.POSIXct(contq_july2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_july2020$collectDate <- as.POSIXct(contq_july2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_july2020, "contq_july2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_july2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_july2020.csv"
)

#### AUGUST 2020 ####
contq_aug2020 <- GET(contq_urls[grep("TECR/2020-08", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_aug2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_aug2020 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2020-08.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_aug2020$endDate <- as.POSIXct(contq_aug2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_aug2020$collectDate <- as.POSIXct(contq_aug2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_aug2020, "contq_aug2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_aug2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_aug2020.csv"
)

#### SEPTEMBER 2020 ####
contq_sept2020 <- GET(contq_urls[grep("TECR/2020-09", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_sept2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_sept2020 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2020-09.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_sept2020$endDate <- as.POSIXct(contq_sept2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_sept2020$collectDate <- as.POSIXct(contq_sept2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_sept2020, "contq_sept2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_sept2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_sept2020.csv"
)

#### OCTOBER 2020 ####
contq_oct2020 <- GET(contq_urls[grep("TECR/2020-10", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_oct2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_oct2020 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2020-10.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_oct2020$endDate <- as.POSIXct(contq_oct2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_oct2020$collectDate <- as.POSIXct(contq_oct2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_oct2020, "contq_oct2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_oct2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_oct2020.csv"
)


#### NOVEMBER 2020 ####
contq_nov2020 <- GET(contq_urls[grep("TECR/2020-11", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_nov2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_nov2020 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2020-11.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_nov2020$endDate <- as.POSIXct(contq_nov2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_nov2020$collectDate <- as.POSIXct(contq_nov2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_nov2020, "contq_nov2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_nov2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_nov2020.csv"
)

#### DECEMBER 2020 ####
contq_dec2020 <- GET(contq_urls[grep("TECR/2020-12", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_dec2020, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_dec2020 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2020-12.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_dec2020$endDate <- as.POSIXct(contq_dec2020$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_dec2020$collectDate <- as.POSIXct(contq_dec2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_dec2020, "contq_dec2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_dec2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_dec2020.csv"
)

#### JANUARY 2021 ####
contq_jan2021 <- GET(contq_urls[grep("TECR/2021-01", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_jan2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_jan2021 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2021-01.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_jan2021$endDate <- as.POSIXct(contq_jan2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_jan2021$collectDate <- as.POSIXct(contq_jan2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_jan2021, "contq_jan2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_jan2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_jan2021.csv"
)

#### FEBRUARY 2021 ####
contq_feb2021 <- GET(contq_urls[grep("TECR/2021-02", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_feb2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_feb2021 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2021-02.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_feb2021$endDate <- as.POSIXct(contq_feb2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_feb2021$collectDate <- as.POSIXct(contq_feb2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_feb2021, "contq_feb2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_feb2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_feb2021.csv"
)

#### MARCH 2021 ####
contq_mar2021 <- GET(contq_urls[grep("TECR/2021-03", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_mar2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_mar2021 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2021-03.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_mar2021$endDate <- as.POSIXct(contq_mar2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_mar2021$collectDate <- as.POSIXct(contq_mar2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_mar2021, "contq_mar2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_mar2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_mar2021.csv"
)

#### APRIL 2021 ####
contq_apr2021 <- GET(contq_urls[grep("TECR/2021-04", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_apr2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_apr2021 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2021-04.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_apr2021$endDate <- as.POSIXct(contq_apr2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_apr2021$collectDate <- as.POSIXct(contq_apr2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_apr2021, "contq_apr2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_apr2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_apr2021.csv"
)

#### MAY 2021 ####
contq_may2021 <- GET(contq_urls[grep("TECR/2021-05", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_may2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_may2021 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2021-05.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_may2021$endDate <- as.POSIXct(contq_may2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_may2021$collectDate <- as.POSIXct(contq_may2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_may2021, "contq_may2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_may2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_may2021.csv"
)

#### JUNE 2021 ####
contq_june2021 <- GET(contq_urls[grep("TECR/2021-06", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_june2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_june2021 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2021-06.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_june2021$endDate <- as.POSIXct(contq_june2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_june2021$collectDate <- as.POSIXct(contq_june2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_june2021, "contq_june2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_june2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_june2021.csv"
)

#### JULY 2021 ####
contq_july2021 <- GET(contq_urls[grep("TECR/2021-07", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_july2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_july2021 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2021-07.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_july2021$endDate <- as.POSIXct(contq_july2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_july2021$collectDate <- as.POSIXct(contq_july2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_july2021, "contq_july2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_july2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_july2021.csv"
)

#### AUGUST 2021 ####
contq_aug2021 <- GET(contq_urls[grep("TECR/2021-08", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_aug2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_aug2021 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2021-08.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_aug2021$endDate <- as.POSIXct(contq_aug2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_aug2021$collectDate <- as.POSIXct(contq_aug2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_aug2021, "contq_aug2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_aug2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_aug2021.csv"
)

#### SEPTEMBER 2021 ####
contq_sept2021 <- GET(contq_urls[grep("TECR/2021-09", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_sept2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_sept2021 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2021-09.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_sept2021$endDate <- as.POSIXct(contq_sept2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_sept2021$collectDate <- as.POSIXct(contq_sept2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_sept2021, "contq_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_sept2021.csv"
)

#### OCTOBER 2021 ####
contq_oct2021 <- GET(contq_urls[grep("TECR/2021-10", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_oct2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_oct2021 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2021-10.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_oct2021$endDate <- as.POSIXct(contq_oct2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_oct2021$collectDate <- as.POSIXct(contq_oct2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_oct2021, "contq_oct2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_oct2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_oct2021.csv"
)

#### NOVEMBER 2021 ####
contq_nov2021 <- GET(contq_urls[grep("TECR/2021-11", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_nov2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_nov2021 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2021-11.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_nov2021$endDate <- as.POSIXct(contq_nov2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_nov2021$collectDate <- as.POSIXct(contq_nov2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_nov2021, "contq_nov2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_nov2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_nov2021.csv"
)

#### DECEMBER 2021 ####
contq_dec2021 <- GET(contq_urls[grep("TECR/2021-12", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_dec2021, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_dec2021 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2021-12.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_dec2021$endDate <- as.POSIXct(contq_dec2021$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_dec2021$collectDate <- as.POSIXct(contq_dec2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_dec2021, "contq_dec2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_dec2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_dec2021.csv"
)
#### JANUARY 2022 ####
contq_jan2022 <- GET(contq_urls[grep("TECR/2022-01", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_jan2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_jan2022 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2022-01.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_jan2022$endDate <- as.POSIXct(contq_jan2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_jan2022$collectDate <- as.POSIXct(contq_jan2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_jan2022, "contq_jan2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_jan2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_jan2022.csv"
)
#### FEBRUARY 2022 ####
contq_feb2022 <- GET(contq_urls[grep("TECR/2022-02", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_feb2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_feb2022 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2022-02.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_feb2022$endDate <- as.POSIXct(contq_feb2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_feb2022$collectDate <- as.POSIXct(contq_feb2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_feb2022, "contq_feb2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_feb2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_feb2022.csv"
)
#### MARCH 2022 ####
contq_mar2022 <- GET(contq_urls[grep("TECR/2022-03", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_mar2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_mar2022 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2022-03.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_mar2022$endDate <- as.POSIXct(contq_mar2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_mar2022$collectDate <- as.POSIXct(contq_mar2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_mar2022, "contq_mar2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_mar2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_mar2022.csv"
)
#### APRIL 2022 ####
contq_apr2022 <- GET(contq_urls[grep("TECR/2022-04", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_apr2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_apr2022 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2022-04.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_apr2022$endDate <- as.POSIXct(contq_apr2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_apr2022$collectDate <- as.POSIXct(contq_apr2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_apr2022, "contq_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_apr2022.csv"
)
#### MAY 2022 ####
contq_may2022 <- GET(contq_urls[grep("TECR/2022-05", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_may2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_may2022 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2022-05.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_may2022$endDate <- as.POSIXct(contq_may2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_may2022$collectDate <- as.POSIXct(contq_may2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_may2022, "contq_may2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_may2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_may2022.csv"
)

#### JUNE 2022 ####
contq_june2022 <- GET(contq_urls[grep("TECR/2022-06", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_june2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_june2022 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2022-06.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_june2022$endDate <- as.POSIXct(contq_june2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_june2022$collectDate <- as.POSIXct(contq_june2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_june2022, "contq_june2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_june2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_june2022.csv"
)
#### JULY 2022 ####
contq_july2022 <- GET(contq_urls[grep("TECR/2022-07", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_july2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_july2022 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2022-07.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_july2022$endDate <- as.POSIXct(contq_july2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_july2022$collectDate <- as.POSIXct(contq_july2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_july2022, "contq_july2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_july2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_july2022.csv"
)
#### AUGUST 2022 ####
contq_aug2022 <- GET(contq_urls[grep("TECR/2022-08", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_aug2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_aug2022 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2022-08.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_aug2022$endDate <- as.POSIXct(contq_aug2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_aug2022$collectDate <- as.POSIXct(contq_aug2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_aug2022, "contq_aug2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_aug2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_aug2022.csv"
)

#### SEPTEMBER 2022 ####
contq_sept2022 <- GET(contq_urls[grep("TECR/2022-09", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_sept2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_sept2022 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2022-09.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_sept2022$endDate <- as.POSIXct(contq_sept2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_sept2022$collectDate <- as.POSIXct(contq_sept2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_sept2022, "contq_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_sept2022.csv"
)
#### OCTOBER 2022 ####
contq_oct2022 <- GET(contq_urls[grep("TECR/2022-10", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_oct2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_oct2022 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2022-10.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_oct2022$endDate <- as.POSIXct(contq_oct2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_oct2022$collectDate <- as.POSIXct(contq_oct2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_oct2022, "contq_oct2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_oct2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_oct2022.csv"
)
#### NOVEMBER 2022 ####
contq_nov2022 <- GET(contq_urls[grep("TECR/2022-11", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_nov2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_nov2022 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2022-11.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_nov2022$endDate <- as.POSIXct(contq_nov2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_nov2022$collectDate <- as.POSIXct(contq_nov2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_nov2022, "contq_nov2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_nov2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_nov2022.csv"
)
#### DECEMBER 2022 ####
contq_dec2022 <- GET(contq_urls[grep("TECR/2022-12", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_dec2022, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_dec2022 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2022-12.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_dec2022$endDate <- as.POSIXct(contq_dec2022$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_dec2022$collectDate <- as.POSIXct(contq_dec2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_dec2022, "contq_dec2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_dec2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_dec2022.csv"
)
#### JANUARY 2023 ####
contq_jan2023 <- GET(contq_urls[grep("TECR/2023-01", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_jan2023, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_jan2023 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2023-01.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_jan2023$endDate <- as.POSIXct(contq_jan2023$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_jan2023$collectDate <- as.POSIXct(contq_jan2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_jan2023, "contq_jan2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_jan2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_jan2023.csv"
)

#### FEBRUARY 2023 ####
contq_feb2023 <- GET(contq_urls[grep("TECR/2023-02", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_feb2023, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_feb2023 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2023-02.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_feb2023$endDate <- as.POSIXct(contq_feb2023$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_feb2023$collectDate <- as.POSIXct(contq_feb2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_feb2023, "contq_feb2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_feb2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_feb2023.csv"
)
#### MARCH 2023 ####
contq_mar2023 <- GET(contq_urls[grep("TECR/2023-03", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_mar2023, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_mar2023 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2023-03.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_mar2023$endDate <- as.POSIXct(contq_mar2023$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_mar2023$collectDate <- as.POSIXct(contq_mar2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_mar2023, "contq_mar2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_mar2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_mar2023.csv"
)
#### APRIL 2023 ####
contq_apr2023 <- GET(contq_urls[grep("TECR/2023-04", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_apr2023, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_apr2023 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2023-04.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_apr2023$endDate <- as.POSIXct(contq_apr2023$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_apr2023$collectDate <- as.POSIXct(contq_apr2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_apr2023, "contq_apr2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_apr2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_apr2023.csv"
)
#### MAY 2023 ####
contq_may2023 <- GET(contq_urls[grep("TECR/2023-05", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_may2023, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_may2023 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2023-05.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_may2023$endDate <- as.POSIXct(contq_may2023$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_may2023$collectDate <- as.POSIXct(contq_may2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_may2023, "contq_may2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_may2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_may2023.csv"
)
#### JUNE 2023 ####
contq_june2023 <- GET(contq_urls[grep("TECR/2023-06", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_june2023, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_june2023 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2023-06.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_june2023$endDate <- as.POSIXct(contq_june2023$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_june2023$collectDate <- as.POSIXct(contq_june2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_june2023, "contq_june2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_june2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_june2023.csv"
)
#### JULY 2023 ####
contq_july2023 <- GET(contq_urls[grep("TECR/2023-07", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_july2023, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_july2023 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2023-07.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_july2023$endDate <- as.POSIXct(contq_july2023$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_july2023$collectDate <- as.POSIXct(contq_july2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_july2023, "contq_july2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_july2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_july2023.csv"
)
#### AUGUST 2023 ####
contq_aug2023 <- GET(contq_urls[grep("TECR/2023-08", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_aug2023, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_aug2023 <- read.csv(contq_files$data$files$url
                           [grep("csd_continuousDischarge.2023-08.basic.",
                                 contq_files$data$files$name)])

# reformat Date and Time
contq_aug2023$endDate <- as.POSIXct(contq_aug2023$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_aug2023$collectDate <- as.POSIXct(contq_aug2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_aug2023, "contq_aug2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_aug2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_aug2023.csv"
)
#### SEPTEMBER 2023 ####
contq_sept2023 <- GET(contq_urls[grep("TECR/2023-09", contq_urls)])
contq_files <- jsonlite::fromJSON(content(contq_sept2023, as = "text"))
contq_files$data$files$name

# look at the basic (not expanded) data table for Continuous Discharge 
contq_sept2023 <- read.csv(contq_files$data$files$url
                          [grep("csd_continuousDischarge.2023-09.basic.",
                                contq_files$data$files$name)])

# reformat Date and Time
contq_sept2023$endDate <- as.POSIXct(contq_sept2023$endDate, format = "%Y-%m-%dT%H:%MZ")
# contq_sept2023$collectDate <- as.POSIXct(contq_sept2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(contq_sept2023, "contq_sept2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "contq_sept2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V"),
  name = "contq_sept2023.csv"
)
#### Read me ####
# -- As of 12/04/2025, all data after September 2023 were "provisional," and because 
# of this were not downloaded into the Google Drive.

#### Part 2: Read me ####
# The following code is used to combine all of the individual CSVs into one CSV,
# and to store that CSV in the same Google Drive folder for analysis.

#### NOTE: Continuous discharge data does not easily bind. Why do column names differ
# across month-long datasets?

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
contq <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1QZ7O7nIpGZQxsbIjHfsc2gJGg0hDgt7V")

# List and filter CSV files with "N" in their names
contq_files <- googledrive::drive_ls(path = contq, type = "csv")
contq_files <- contq_files[grepl("contq", contq_files$name), ]

# Create an empty list to store the cleaned data frames
contq_list <- lapply(seq_along(contq_files$name), function(i) {
  googledrive::drive_download(
    file = contq_files$id[i],
    path = paste0("googledrive/", contq_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file
  read.csv(paste0("googledrive/", contq_files$name[i]), header = TRUE)
})

# Assign names to the list elements based on the file names
names(contq_list) <- contq_files$name

# Check the contents of the list
str(contq_list)

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
  file = "all_contdischarge_data.csv",
  row.names = FALSE
)

#### Upload CSV to the specific Google Drive folder ####
folder_id <- drive_get("Continuous discharge")

drive_upload(
  "all_contdischarge_data.csv",
  path = folder_id,
)
