#### Read me ####
# -- The following code is what was used to access water chemistry data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Chemical properties of water." 

# Abstract: This data product contains the quality-controlled, native sampling 
# resolution data from NEON's surface water chemistry sampling protocol. 
# Subsamples are analyzed at NEON domain headquarters for alkalinity and acid 
# neutralizing capacity (ANC); other subsamples are sent to external facilities 
# for a broad suite of analytes, including dissolved and total nutrients 
# and carbon, cations and anions, and general chemistry. For additional details, 
# see the user guide, protocols, and science design listed in the Documentation 
# section in this data product's details webpage. 

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
# data product name: "Chemical properties of surface water" (DP1.20093.001)
chem_product <- GET("http://data.neonscience.org/api/v0/products/DP1.20093.001")
chem_product

# make the data readable by jsonlite
chem_text <- content(chem_product, as = "text")

# flatten json into a nested list
chem_avail <- jsonlite::fromJSON(chem_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
chem_avail$data$siteCodes

# TECR is 30th on the list
chem_avail$data$siteCodes$siteCode[[30]]

# check which months at TECR have available data
chem_avail$data$siteCodes$availableMonths[[30]]

# get a complete list of available data URLs
chem_urls <- unlist(chem_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(chem_urls)

# show the first 10 URLs available
chem_urls[1:10]

#### OCTOBER 2018 ####
chem_oct2018 <- GET(chem_urls[grep("TECR/2018-10", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_oct2018, as = "text"))
chem_files$data$files$name

# look at the basic data table for (surface) water chemistry
chem_oct2018 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2018-10.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_oct2018$startDate <- as.POSIXct(chem_oct2018$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_oct2018$collectDate <- as.POSIXct(chem_oct2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_oct2018, "chem_oct2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_oct2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_oct2018.csv"
)

#### NOVEMBER 2018 ####
chem_nov2018 <- GET(chem_urls[grep("TECR/2018-11", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_nov2018, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_nov2018 <- read.csv(chem_files$data$files$url
                      [grep("swc_externalLabDataByAnalyte.2018-11.basic.",
                            chem_files$data$files$name)])

# reformat Date and Time
chem_nov2018$startDate <- as.POSIXct(chem_nov2018$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_nov2018$collectDate <- as.POSIXct(chem_nov2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_nov2018, "chem_nov2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_nov2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_nov2018.csv"
)

#### DECEMBER 2018 ####
chem_dec2018 <- GET(chem_urls[grep("TECR/2018-12", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_dec2018, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_dec2018 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2018-12.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_dec2018$startDate <- as.POSIXct(chem_dec2018$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_dec2018$collectDate <- as.POSIXct(chem_dec2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_dec2018, "chem_dec2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_dec2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_dec2018.csv"
)

#### JANUARY 2019 ####
chem_jan2019 <- GET(chem_urls[grep("TECR/2019-01", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_jan2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_jan2019 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2019-01.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_jan2019$startDate <- as.POSIXct(chem_jan2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_jan2019$collectDate <- as.POSIXct(chem_jan2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_jan2019, "chem_jan2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_jan2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_jan2019.csv"
)

#### FEBRUARY 2019 ####
# no data
#### MARCH 2019 ####
chem_mar2019 <- GET(chem_urls[grep("TECR/2019-03", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_mar2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_mar2019 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2019-03.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_mar2019$startDate <- as.POSIXct(chem_mar2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_mar2019$collectDate <- as.POSIXct(chem_mar2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_mar2019, "chem_mar2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_mar2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_mar2019.csv"
)


#### APRIL 2019 ####
chem_apr2019 <- GET(chem_urls[grep("TECR/2019-04", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_apr2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_apr2019 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2019-04.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_apr2019$startDate <- as.POSIXct(chem_apr2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_apr2019$collectDate <- as.POSIXct(chem_apr2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_apr2019, "chem_apr2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_apr2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_apr2019.csv"
)
#### MAY 2019 ####
chem_may2019 <- GET(chem_urls[grep("TECR/2019-05", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_may2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_may2019 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2019-05.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_may2019$startDate <- as.POSIXct(chem_may2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_may2019$collectDate <- as.POSIXct(chem_may2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_may2019, "chem_may2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_may2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_may2019.csv"
)

#### JUNE 2019 ####
chem_june2019 <- GET(chem_urls[grep("TECR/2019-06", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_june2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_june2019 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2019-06.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_june2019$startDate <- as.POSIXct(chem_june2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_june2019$collectDate <- as.POSIXct(chem_june2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_june2019, "chem_june2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_june2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_june2019.csv"
)
#### JULY 2019 ####
chem_july2019 <- GET(chem_urls[grep("TECR/2019-07", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_july2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_july2019 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2019-07.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_july2019$startDate <- as.POSIXct(chem_july2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_july2019$collectDate <- as.POSIXct(chem_july2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_july2019, "chem_july2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_july2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_july2019.csv"
)
#### AUGUST 2019 ####
chem_aug2019 <- GET(chem_urls[grep("TECR/2019-08", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_aug2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_aug2019 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2019-08.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_aug2019$startDate <- as.POSIXct(chem_aug2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_aug2019$collectDate <- as.POSIXct(chem_aug2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_aug2019, "chem_aug2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_aug2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_aug2019.csv"
)
#### SEPTEMBER 2019 ####
chem_sept2019 <- GET(chem_urls[grep("TECR/2019-09", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_sept2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_sept2019 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2019-09.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_sept2019$startDate <- as.POSIXct(chem_sept2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_sept2019$collectDate <- as.POSIXct(chem_sept2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_sept2019, "chem_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_sept2019.csv"
)
#### OCTOBER 2019 ####
chem_oct2019 <- GET(chem_urls[grep("TECR/2019-10", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_oct2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_oct2019 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2019-10.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_oct2019$startDate <- as.POSIXct(chem_oct2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_oct2019$collectDate <- as.POSIXct(chem_oct2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_oct2019, "chem_oct2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_oct2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_oct2019.csv"
)
#### NOVEMBER 2019 ####
chem_nov2019 <- GET(chem_urls[grep("TECR/2019-11", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_nov2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_nov2019 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2019-11.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_nov2019$startDate <- as.POSIXct(chem_nov2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_nov2019$collectDate <- as.POSIXct(chem_nov2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_nov2019, "chem_nov2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_nov2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_nov2019.csv"
)
#### DECEMBER 2019 ####
chem_dec2019 <- GET(chem_urls[grep("TECR/2019-11", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_dec2019, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_dec2019 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2019-11.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_dec2019$startDate <- as.POSIXct(chem_dec2019$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_dec2019$collectDate <- as.POSIXct(chem_dec2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_dec2019, "chem_dec2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_dec2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_dec2019.csv"
)
#### JANUARY 2020 ####
chem_jan2020 <- GET(chem_urls[grep("TECR/2020-01", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_jan2020, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_jan2020 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2020-01.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_jan2020$startDate <- as.POSIXct(chem_jan2020$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_jan2020$collectDate <- as.POSIXct(chem_jan2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_jan2020, "chem_jan2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_jan2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_jan2020.csv"
)
#### FEBRUARY 2020 ####
chem_feb2020 <- GET(chem_urls[grep("TECR/2020-02", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_feb2020, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_feb2020 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2020-02.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_feb2020$startDate <- as.POSIXct(chem_feb2020$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_feb2020$collectDate <- as.POSIXct(chem_feb2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_feb2020, "chem_feb2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_feb2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_feb2020.csv"
)
#### MARCH 2020 ####
chem_mar2020 <- GET(chem_urls[grep("TECR/2020-03", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_mar2020, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_mar2020 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2020-03.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_mar2020$startDate <- as.POSIXct(chem_mar2020$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_mar2020$collectDate <- as.POSIXct(chem_mar2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_mar2020, "chem_mar2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_mar2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_mar2020.csv"
)

#### APRIL 2020 ####
# -- no data

#### MAY 2020 ####
# -- no data

#### JUNE 2020 ####
# -- no data

#### JULY 2020 ####
chem_july2020 <- GET(chem_urls[grep("TECR/2020-07", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_july2020, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_july2020 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2020-07.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_july2020$startDate <- as.POSIXct(chem_july2020$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_july2020$collectDate <- as.POSIXct(chem_july2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_july2020, "chem_july2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_july2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_july2020.csv"
)

#### AUGUST 2020 ####
# -- no data

#### SEPTEMBER 2020 ####
# -- no data
#### OCTOBER 2020 ####
chem_oct2020 <- GET(chem_urls[grep("TECR/2020-10", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_oct2020, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_oct2020 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2020-10.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_oct2020$startDate <- as.POSIXct(chem_oct2020$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_oct2020$collectDate <- as.POSIXct(chem_oct2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_oct2020, "chem_oct2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_oct2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_oct2020.csv"
)

#### NOVEMBER 2020 ####
chem_nov2020 <- GET(chem_urls[grep("TECR/2020-11", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_nov2020, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_nov2020 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2020-11.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_nov2020$startDate <- as.POSIXct(chem_nov2020$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_nov2020$collectDate <- as.POSIXct(chem_nov2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_nov2020, "chem_nov2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_nov2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_nov2020.csv"
)

#### DECEMBER 2020 ####
# -- no data

#### JANUARY 2021 ####
# -- no data

#### FEBURARY 2021 ####
chem_feb2021 <- GET(chem_urls[grep("TECR/2021-02", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_feb2021, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_feb2021 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2021-02.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_feb2021$startDate <- as.POSIXct(chem_feb2021$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_feb2021$collectDate <- as.POSIXct(chem_feb2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_feb2021, "chem_feb2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_feb2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_feb2021.csv"
)

#### MARCH 2021 ####
chem_mar2021 <- GET(chem_urls[grep("TECR/2021-03", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_mar2021, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_mar2021 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2021-03.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_mar2021$startDate <- as.POSIXct(chem_mar2021$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_mar2021$collectDate <- as.POSIXct(chem_mar2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_mar2021, "chem_mar2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_mar2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_mar2021.csv"
)

#### APRIL 2021 ####
# -- no data

#### MAY 2021 ####
chem_may2021 <- GET(chem_urls[grep("TECR/2021-05", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_may2021, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_may2021 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2021-05.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_may2021$startDate <- as.POSIXct(chem_may2021$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_may2021$collectDate <- as.POSIXct(chem_may2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_may2021, "chem_may2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_may2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_may2021.csv"
)

#### JUNE 2021 ####
chem_june2021 <- GET(chem_urls[grep("TECR/2021-06", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_june2021, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_june2021 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2021-06.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_june2021$startDate <- as.POSIXct(chem_june2021$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_june2021$collectDate <- as.POSIXct(chem_june2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_june2021, "chem_june2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_june2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_june2021.csv"
)

#### JULY 2021 ####
chem_july2021 <- GET(chem_urls[grep("TECR/2021-07", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_july2021, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_july2021 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2021-07.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_july2021$startDate <- as.POSIXct(chem_july2021$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_july2021$collectDate <- as.POSIXct(chem_july2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_july2021, "chem_july2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_july2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_july2021.csv"
)

#### AUGUST 2021 ####
chem_aug2021 <- GET(chem_urls[grep("TECR/2021-08", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_aug2021, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_aug2021 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2021-08.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_aug2021$startDate <- as.POSIXct(chem_aug2021$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_aug2021$collectDate <- as.POSIXct(chem_aug2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_aug2021, "chem_aug2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_aug2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_aug2021.csv"
)

#### SEPTEMBER 2021 ####
chem_sept2021 <- GET(chem_urls[grep("TECR/2021-09", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_sept2021, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_sept2021 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2021-09.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_sept2021$startDate <- as.POSIXct(chem_sept2021$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_sept2021$collectDate <- as.POSIXct(chem_sept2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_sept2021, "chem_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_sept2021.csv"
)

#### OCTOBER 2021 ####
chem_oct2021 <- GET(chem_urls[grep("TECR/2021-10", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_oct2021, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_oct2021 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2021-10.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_oct2021$startDate <- as.POSIXct(chem_oct2021$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_oct2021$collectDate <- as.POSIXct(chem_oct2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_oct2021, "chem_oct2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_oct2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_oct2021.csv"
)

#### NOVEMBER 2021 ####
chem_nov2021 <- GET(chem_urls[grep("TECR/2021-11", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_nov2021, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_nov2021 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2021-11.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_nov2021$startDate <- as.POSIXct(chem_nov2021$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_nov2021$collectDate <- as.POSIXct(chem_nov2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_nov2021, "chem_nov2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_nov2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_nov2021.csv"
)

#### DECEMBER 2021 ####
# -- no data
#### JANUARY 2022 ####
chem_jan2022 <- GET(chem_urls[grep("TECR/2022-01", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_jan2022, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_jan2022 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2022-01.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_jan2022$startDate <- as.POSIXct(chem_jan2022$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_jan2022$collectDate <- as.POSIXct(chem_jan2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_jan2022, "chem_jan2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_jan2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_jan2022.csv"
)

#### FEBRUARY 2022 ####
chem_feb2022 <- GET(chem_urls[grep("TECR/2022-02", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_feb2022, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_feb2022 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2022-02.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_feb2022$startDate <- as.POSIXct(chem_feb2022$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_feb2022$collectDate <- as.POSIXct(chem_feb2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_feb2022, "chem_feb2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_feb2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_feb2022.csv"
)

#### MARCH 2022 ####
chem_mar2022 <- GET(chem_urls[grep("TECR/2022-03", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_mar2022, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_mar2022 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2022-03.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_mar2022$startDate <- as.POSIXct(chem_mar2022$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_mar2022$collectDate <- as.POSIXct(chem_mar2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_mar2022, "chem_mar2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_mar2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_mar2022.csv"
)

#### APRIL 2022 ####
chem_apr2022 <- GET(chem_urls[grep("TECR/2022-04", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_apr2022, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_apr2022 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2022-04.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_apr2022$startDate <- as.POSIXct(chem_apr2022$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_apr2022$collectDate <- as.POSIXct(chem_apr2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_apr2022, "chem_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_apr2022.csv"
)

#### MAY 2022 ####
chem_may2022 <- GET(chem_urls[grep("TECR/2022-05", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_may2022, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_may2022 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2022-05.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_may2022$startDate <- as.POSIXct(chem_may2022$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_may2022$collectDate <- as.POSIXct(chem_may2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_may2022, "chem_may2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_may2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_may2022.csv"
)

#### JUNE 2022 ####
chem_june2022 <- GET(chem_urls[grep("TECR/2022-06", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_june2022, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_june2022 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2022-06.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_june2022$startDate <- as.POSIXct(chem_june2022$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_june2022$collectDate <- as.POSIXct(chem_june2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_june2022, "chem_june2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_june2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_june2022.csv"
)

#### JULY 2022 ####
chem_july2022 <- GET(chem_urls[grep("TECR/2022-07", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_july2022, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_july2022 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2022-07.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_july2022$startDate <- as.POSIXct(chem_july2022$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_july2022$collectDate <- as.POSIXct(chem_july2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_july2022, "chem_july2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_july2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_july2022.csv"
)

#### AUGUST 2022 ####
chem_aug2022 <- GET(chem_urls[grep("TECR/2022-08", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_aug2022, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_aug2022 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2022-08.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_aug2022$startDate <- as.POSIXct(chem_aug2022$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_aug2022$collectDate <- as.POSIXct(chem_aug2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_aug2022, "chem_aug2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_aug2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_aug2022.csv"
)

#### SEPTEMBER 2022 ####
chem_sept2022 <- GET(chem_urls[grep("TECR/2022-09", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_sept2022, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_sept2022 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2022-09.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_sept2022$startDate <- as.POSIXct(chem_sept2022$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_sept2022$collectDate <- as.POSIXct(chem_sept2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_sept2022, "chem_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_sept2022.csv"
)

#### OCTOBER 2022 ####
# -- no data

#### NOVEMBER 2022 ####
# -- no data

#### DECEMBER 2022 ####
# -- no data

#### JANUARY 2023 ####
# -- no data

#### FEBRUARY 2023 ####
# -- no data

#### MARCH 2023 ####
# -- no data

#### APRIL 2023 ####
# -- no data

#### MAY 2023 ####
# -- no data
#### JUNE 2023 ####
chem_june2023 <- GET(chem_urls[grep("TECR/2023-06", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_june2023, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_june2023 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2023-06.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_june2023$startDate <- as.POSIXct(chem_june2023$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_june2023$collectDate <- as.POSIXct(chem_june2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_june2023, "chem_june2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_june2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_june2023.csv"
)


#### JULY 2023 ####
chem_july2023 <- GET(chem_urls[grep("TECR/2023-07", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_july2023, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_july2023 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2023-07.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_july2023$startDate <- as.POSIXct(chem_july2023$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_july2023$collectDate <- as.POSIXct(chem_july2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_july2023, "chem_july2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_july2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_july2023.csv"
)

#### AUGUST 2023 ####
chem_aug2023 <- GET(chem_urls[grep("TECR/2023-08", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_aug2023, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_aug2023 <- read.csv(chem_files$data$files$url
                          [grep("swc_externalLabDataByAnalyte.2023-08.basic.",
                                chem_files$data$files$name)])

# reformat Date and Time
chem_aug2023$startDate <- as.POSIXct(chem_aug2023$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_aug2023$collectDate <- as.POSIXct(chem_aug2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_aug2023, "chem_aug2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_aug2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_aug2023.csv"
)

#### SEPTEMBER 2023 ####
# -- no data

#### OCTOBER 2023 ####
chem_oct2023 <- GET(chem_urls[grep("TECR/2023-10", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_oct2023, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_oct2023 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2023-10.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_oct2023$startDate <- as.POSIXct(chem_oct2023$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_oct2023$collectDate <- as.POSIXct(chem_oct2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_oct2023, "chem_oct2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_oct2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_oct2023.csv"
)

#### NOVEMBER 2023 ####
chem_nov2023 <- GET(chem_urls[grep("TECR/2023-11", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_nov2023, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_nov2023 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2023-11.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_nov2023$startDate <- as.POSIXct(chem_nov2023$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_nov2023$collectDate <- as.POSIXct(chem_nov2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_nov2023, "chem_nov2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_nov2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_nov2023.csv"
)

#### DECEMBER 2023 ####
chem_dec2023 <- GET(chem_urls[grep("TECR/2023-12", chem_urls)])
chem_files <- jsonlite::fromJSON(content(chem_dec2023, as = "text"))
chem_files$data$files$name

# look at the basic (not expanded) data table for Nitrates 
chem_dec2023 <- read.csv(chem_files$data$files$url
                         [grep("swc_externalLabDataByAnalyte.2023-12.basic.",
                               chem_files$data$files$name)])

# reformat Date and Time
chem_dec2023$startDate <- as.POSIXct(chem_dec2023$startDate, format = "%Y-%m-%dT%H:%MZ")
chem_dec2023$collectDate <- as.POSIXct(chem_dec2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(chem_dec2023, "chem_dec2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "chem_dec2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1dw_beKqjGDqkDi0vebf-n3QleoszV5yP"),
  name = "chem_dec2023.csv"
)

#### Read me ####
# -- As of 12/04/2025, all data after December 2023 were "provisional," and because 
# of this were not downloaded into the Google Drive.