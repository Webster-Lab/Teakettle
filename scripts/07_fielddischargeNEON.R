#### Read me ####
# -- The following code is what was used to access field discharge data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Discharge field collection." 

# Abstract: This data product contains the quality-controlled, native sampling 
# resolution data from NEON's stream and river discharge field collection protocol. 
# Individual discharge measurements are conducted by means of surveys that occur 
# in wadeable streams and rivers along permanently benchmarked cross-sections 
# at NEON aquatic sites. At the Toolik Lake site discharge is measured at the 
# lake outflow and inflow locations. At stream sites (and Toolik Lake inflow and outflow), 
# discharge is measured using a handheld flowmeter (when flows are very low) and 
# acoustic Doppler current profiler (ADCP) instrumentation (when flows are at or above baseflow). 
# During flowmeter measurements the waterbody is divided into lateral sub-sections 
# (of which there are typically 20-25 per cross-section). Within each subsection, 
# an instantaneous velocity magnitude is obtained via a handheld flowmeter and 
# transformed to a volumetric discharge magnitude by applying the velocity across 
# the full subsection area. Total stream discharge is then calculated by summing 
# the discrete volumetric discharges for each subsection. During ADCP measurements 
# the ADCP is placed in a trimaran float and pulled back and forth across the channel 
# using a tethered rope system. Velocity, depth, and area data are continuously measured, 
# and discharge is calculated for each transect. Total discharge for a given bout 
# is the mean discharge across all transects. At river sites discharge is measured 
# using ADCPs deployed in remote controlled boats that are driven back and forth 
# across the channel. Discharge is calculated in the same manner as the ADCP 
# method at stream sites, only the deployment method differs. If remote controlled 
# boats are not available a piloted boat method serves as a backup where the 
# ADCP is deployed off the side of a piloted boat.

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
# data product name: "Discharge field collection" (DP1.20048.001)
fieldq_product <- GET("http://data.neonscience.org/api/v0/products/DP1.20048.001")
fieldq_product

# make the data readable by jsonlite
fieldq_text <- content(fieldq_product, as = "text")

# flatten json into a nested list
fieldq_avail <- jsonlite::fromJSON(fieldq_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
fieldq_avail$data$siteCodes

# TECR is 30th on the list
fieldq_avail$data$siteCodes$siteCode[[24]]

# check which months at TECR have available data
fieldq_avail$data$siteCodes$availableMonths[[24]]

# get a complete list of available data URLs
fieldq_urls <- unlist(fieldq_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(fieldq_urls)

# show the first 10 URLs available
fieldq_urls[1:10]

#### MAY 2017 ####
fieldq_may2017 <- GET(fieldq_urls[grep("TECR/2017-05", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_may2017, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_may2017 <- read.csv(fieldq_files$data$files$url
                          [grep("dsc_fieldData.2017-05.basic.",
                                fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_may2017$collectDate <- as.POSIXct(fieldq_may2017$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_may2017$collectDate <- as.POSIXct(q_may2017$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_may2017, "fieldq_may2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_may2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_may2017.csv"
)

#### JUNE 2017 ####
fieldq_june2017 <- GET(fieldq_urls[grep("TECR/2017-06", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_june2017, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_june2017 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2017-06.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_june2017$collectDate <- as.POSIXct(fieldq_june2017$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_june2017$collectDate <- as.POSIXct(q_june2017$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_june2017, "fieldq_june2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_june2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_june2017.csv"
)


#### JULY 2017 ####
fieldq_july2017 <- GET(fieldq_urls[grep("TECR/2017-07", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_july2017, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_july2017 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2017-07.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_july2017$collectDate <- as.POSIXct(fieldq_july2017$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2017$collectDate <- as.POSIXct(q_july2017$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_july2017, "fieldq_july2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_july2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_july2017.csv"
)

#### AUGUST 2017 ####
fieldq_aug2017 <- GET(fieldq_urls[grep("TECR/2017-08", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_aug2017, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_aug2017 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2017-08.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_aug2017$collectDate <- as.POSIXct(fieldq_aug2017$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_aug2017$collectDate <- as.POSIXct(q_aug2017$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_aug2017, "fieldq_aug2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_aug2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_aug2017.csv"
)

#### SEPTEMBER 2017 ####
fieldq_sept2017 <- GET(fieldq_urls[grep("TECR/2017-09", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_sept2017, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_sept2017 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2017-09.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_sept2017$collectDate <- as.POSIXct(fieldq_sept2017$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_sept2017$collectDate <- as.POSIXct(q_sept2017$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_sept2017, "fieldq_sept2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_sept2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_sept2017.csv"
)


#### OCTOBER 2017 ####
fieldq_oct2017 <- GET(fieldq_urls[grep("TECR/2017-10", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_oct2017, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_oct2017 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2017-10.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_oct2017$collectDate <- as.POSIXct(fieldq_oct2017$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2017$collectDate <- as.POSIXct(q_oct2017$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_oct2017, "fieldq_oct2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_oct2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_oct2017.csv"
)
#### NOVEMBER 2017 ####
fieldq_nov2017 <- GET(fieldq_urls[grep("TECR/2017-11", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_nov2017, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_nov2017 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2017-11.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_nov2017$collectDate <- as.POSIXct(fieldq_nov2017$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2017$collectDate <- as.POSIXct(q_nov2017$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_nov2017, "fieldq_nov2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_nov2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_nov2017.csv"
)
#### DECEMBER 2017 ####
fieldq_dec2017 <- GET(fieldq_urls[grep("TECR/2017-12", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_dec2017, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_dec2017 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2017-12.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_dec2017$collectDate <- as.POSIXct(fieldq_dec2017$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_dec2017$collectDate <- as.POSIXct(q_dec2017$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_dec2017, "fieldq_dec2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_dec2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_dec2017.csv"
)
#### JANUARY 2018 ####
fieldq_jan2018 <- GET(fieldq_urls[grep("TECR/2018-01", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_jan2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_jan2018 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2018-01.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_jan2018$collectDate <- as.POSIXct(fieldq_jan2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_jan2018$collectDate <- as.POSIXct(q_jan2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_jan2018, "fieldq_jan2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_jan2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_jan2018.csv"
)
#### FEBRUARY 2018 ####
fieldq_feb2018 <- GET(fieldq_urls[grep("TECR/2018-02", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_feb2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_feb2018 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2018-02.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_feb2018$collectDate <- as.POSIXct(fieldq_feb2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_feb2018$collectDate <- as.POSIXct(q_feb2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_feb2018, "fieldq_feb2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_feb2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_feb2018.csv"
)
#### MARCH 2018 ####
fieldq_mar2018 <- GET(fieldq_urls[grep("TECR/2018-03", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_mar2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_mar2018 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2018-03.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_mar2018$collectDate <- as.POSIXct(fieldq_mar2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_mar2018$collectDate <- as.POSIXct(q_mar2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_mar2018, "fieldq_mar2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_mar2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_mar2018.csv"
)
#### APRIL 2018 ####
fieldq_apr2018 <- GET(fieldq_urls[grep("TECR/2018-04", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_apr2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_apr2018 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2018-04.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_apr2018$collectDate <- as.POSIXct(fieldq_apr2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_apr2018$collectDate <- as.POSIXct(q_apr2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_apr2018, "fieldq_apr2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_apr2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_apr2018.csv"
)
#### MAY 2018 ####
fieldq_may2018 <- GET(fieldq_urls[grep("TECR/2018-05", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_may2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_may2018 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2018-05.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_may2018$collectDate <- as.POSIXct(fieldq_may2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_may2018$collectDate <- as.POSIXct(q_may2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_may2018, "fieldq_may2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_may2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_may2018.csv"
)
#### JUNE 2018 ####
fieldq_june2018 <- GET(fieldq_urls[grep("TECR/2018-06", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_june2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_june2018 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2018-06.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_june2018$collectDate <- as.POSIXct(fieldq_june2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_june2018$collectDate <- as.POSIXct(q_june2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_june2018, "fieldq_june2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_june2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_june2018.csv"
)
#### JULY 2018 ####
fieldq_july2018 <- GET(fieldq_urls[grep("TECR/2018-07", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_july2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_july2018 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2018-07.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_july2018$collectDate <- as.POSIXct(fieldq_july2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2018$collectDate <- as.POSIXct(q_july2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_july2018, "fieldq_july2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_july2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_july2018.csv"
)
#### AUGUST 2018 ####
fieldq_aug2018 <- GET(fieldq_urls[grep("TECR/2018-08", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_aug2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_aug2018 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2018-08.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_aug2018$collectDate <- as.POSIXct(fieldq_aug2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_aug2018$collectDate <- as.POSIXct(q_aug2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_aug2018, "fieldq_aug2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_aug2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_aug2018.csv"
)
#### SEPTEMBER 2018 ####
fieldq_sept2018 <- GET(fieldq_urls[grep("TECR/2018-09", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_sept2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_sept2018 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2018-09.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_sept2018$collectDate <- as.POSIXct(fieldq_sept2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_sept2018$collectDate <- as.POSIXct(q_sept2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_sept2018, "fieldq_sept2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_sept2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_sept2018.csv"
)
#### OCTOBER 2018 ####
fieldq_oct2018 <- GET(fieldq_urls[grep("TECR/2018-10", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_oct2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_oct2018 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2018-10.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_oct2018$collectDate <- as.POSIXct(fieldq_oct2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2018$collectDate <- as.POSIXct(q_oct2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_oct2018, "fieldq_oct2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_oct2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_oct2018.csv"
)
#### NOVEMBER 2018 ####
fieldq_nov2018 <- GET(fieldq_urls[grep("TECR/2018-11", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_nov2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_nov2018 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2018-11.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_nov2018$collectDate <- as.POSIXct(fieldq_nov2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2018$collectDate <- as.POSIXct(q_nov2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_nov2018, "fieldq_nov2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_nov2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_nov2018.csv"
)
#### DECEMBER 2018 ####
fieldq_dec2018 <- GET(fieldq_urls[grep("TECR/2018-12", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_dec2018, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_dec2018 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2018-12.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_dec2018$collectDate <- as.POSIXct(fieldq_dec2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_dec2018$collectDate <- as.POSIXct(q_dec2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_dec2018, "fieldq_dec2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_dec2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_dec2018.csv"
)
#### JANUARY 2019 ####
fieldq_jan2019 <- GET(fieldq_urls[grep("TECR/2019-01", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_jan2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_jan2019 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2019-01.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_jan2019$collectDate <- as.POSIXct(fieldq_jan2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_jan2019$collectDate <- as.POSIXct(q_jan2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_jan2019, "fieldq_jan2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_jan2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_jan2019.csv"
)
#### FEBRUARY 2019 ####
# -- no data
#### MARCH 2019 ####
fieldq_mar2019 <- GET(fieldq_urls[grep("TECR/2019-03", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_mar2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_mar2019 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2019-03.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_mar2019$collectDate <- as.POSIXct(fieldq_mar2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_mar2019$collectDate <- as.POSIXct(q_mar2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_mar2019, "fieldq_mar2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_mar2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_mar2019.csv"
)

#### APRIL 2019 ####
fieldq_apr2019 <- GET(fieldq_urls[grep("TECR/2019-04", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_apr2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_apr2019 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2019-04.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_apr2019$collectDate <- as.POSIXct(fieldq_apr2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_apr2019$collectDate <- as.POSIXct(q_apr2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_apr2019, "fieldq_apr2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_apr2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_apr2019.csv"
)
#### MAY 2019 ####
fieldq_may2019 <- GET(fieldq_urls[grep("TECR/2019-05", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_may2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_may2019 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2019-05.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_may2019$collectDate <- as.POSIXct(fieldq_may2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_may2019$collectDate <- as.POSIXct(q_may2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_may2019, "fieldq_may2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_may2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_may2019.csv"
)
#### JUNE 2019 ####
fieldq_june2019 <- GET(fieldq_urls[grep("TECR/2019-06", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_june2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_june2019 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2019-06.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_june2019$collectDate <- as.POSIXct(fieldq_june2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_june2019$collectDate <- as.POSIXct(q_june2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_june2019, "fieldq_june2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_june2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_june2019.csv"
)
#### JULY 2019 ####
fieldq_july2019 <- GET(fieldq_urls[grep("TECR/2019-07", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_july2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_july2019 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2019-07.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_july2019$collectDate <- as.POSIXct(fieldq_july2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2019$collectDate <- as.POSIXct(q_july2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_july2019, "fieldq_july2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_july2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_july2019.csv"
)
#### AUGUST 2019 ####
fieldq_aug2019 <- GET(fieldq_urls[grep("TECR/2019-08", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_aug2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_aug2019 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2019-08.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_aug2019$collectDate <- as.POSIXct(fieldq_aug2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_aug2019$collectDate <- as.POSIXct(q_aug2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_aug2019, "fieldq_aug2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_aug2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_aug2019.csv"
)
#### SEPTEMBER 2019 ####
fieldq_sept2019 <- GET(fieldq_urls[grep("TECR/2019-09", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_sept2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_sept2019 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2019-09.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_sept2019$collectDate <- as.POSIXct(fieldq_sept2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_sept2019$collectDate <- as.POSIXct(q_sept2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_sept2019, "fieldq_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_sept2019.csv"
)
#### OCTOBER 2019 ####
fieldq_oct2019 <- GET(fieldq_urls[grep("TECR/2019-10", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_oct2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_oct2019 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2019-10.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_oct2019$collectDate <- as.POSIXct(fieldq_oct2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2019$collectDate <- as.POSIXct(q_oct2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_oct2019, "fieldq_oct2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_oct2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_oct2019.csv"
)
#### NOVEMBER 2019 ####
fieldq_nov2019 <- GET(fieldq_urls[grep("TECR/2019-11", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_nov2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_nov2019 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2019-11.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_nov2019$collectDate <- as.POSIXct(fieldq_nov2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2019$collectDate <- as.POSIXct(q_nov2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_nov2019, "fieldq_nov2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_nov2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_nov2019.csv"
)
#### DECEMBER 2019 ####
fieldq_dec2019 <- GET(fieldq_urls[grep("TECR/2019-12", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_dec2019, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_dec2019 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2019-12.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_dec2019$collectDate <- as.POSIXct(fieldq_dec2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_dec2019$collectDate <- as.POSIXct(q_dec2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_dec2019, "fieldq_dec2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_dec2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_dec2019.csv"
)
#### JANUARY 2020 ####
fieldq_jan2020 <- GET(fieldq_urls[grep("TECR/2020-01", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_jan2020, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_jan2020 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2020-01.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_jan2020$collectDate <- as.POSIXct(fieldq_jan2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_jan2020$collectDate <- as.POSIXct(q_jan2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_jan2020, "fieldq_jan2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_jan2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_jan2020.csv"
)
#### FEBRUARY 2020 ####
fieldq_feb2020 <- GET(fieldq_urls[grep("TECR/2020-02", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_feb2020, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_feb2020 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2020-02.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_feb2020$collectDate <- as.POSIXct(fieldq_feb2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_feb2020$collectDate <- as.POSIXct(q_feb2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_feb2020, "fieldq_feb2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_feb2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_feb2020.csv"
)
#### MARCH 2020 ####
fieldq_mar2020 <- GET(fieldq_urls[grep("TECR/2020-03", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_mar2020, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_mar2020 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2020-03.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_mar2020$collectDate <- as.POSIXct(fieldq_mar2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_mar2020$collectDate <- as.POSIXct(q_mar2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_mar2020, "fieldq_mar2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_mar2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_mar2020.csv"
)
#### APRIL 2020 ####
# -- no data
#### MAY 2020 ####
# -- no data
#### JUNE 2020 ####
# -- no data
#### JULY 2020 ####
fieldq_july2020 <- GET(fieldq_urls[grep("TECR/2020-07", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_july2020, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_july2020 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2020-07.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_july2020$collectDate <- as.POSIXct(fieldq_july2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2020$collectDate <- as.POSIXct(q_july2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_july2020, "fieldq_july2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_july2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_july2020.csv"
)

#### AUGUST 2020 ####
# -- no data
#### SEPTEMBER 2020 ####
# -- no data
#### OCTOBER 2020 ####
fieldq_oct2020 <- GET(fieldq_urls[grep("TECR/2020-10", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_oct2020, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_oct2020 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2020-10.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_oct2020$collectDate <- as.POSIXct(fieldq_oct2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2020$collectDate <- as.POSIXct(q_oct2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_oct2020, "fieldq_oct2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_oct2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_oct2020.csv"
)

#### NOVEMBER 2020 ####
# -- no data
#### DECEMBER 2020 ####
fieldq_dec2020 <- GET(fieldq_urls[grep("TECR/2020-12", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_dec2020, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_dec2020 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2020-12.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_dec2020$collectDate <- as.POSIXct(fieldq_dec2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_dec2020$collectDate <- as.POSIXct(q_dec2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_dec2020, "fieldq_dec2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_dec2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_dec2020.csv"
)

#### JANUARY 2021 ####
# -- no data
#### FEBRUARY 2021 ####
fieldq_feb2021 <- GET(fieldq_urls[grep("TECR/2021-02", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_feb2021, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_feb2021 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2021-02.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_feb2021$collectDate <- as.POSIXct(fieldq_feb2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_feb2021$collectDate <- as.POSIXct(q_feb2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_feb2021, "fieldq_feb2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_feb2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_feb2021.csv"
)


#### MARCH 2021 ####
fieldq_mar2021 <- GET(fieldq_urls[grep("TECR/2021-03", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_mar2021, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_mar2021 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2021-03.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_mar2021$collectDate <- as.POSIXct(fieldq_mar2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_mar2021$collectDate <- as.POSIXct(q_mar2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_mar2021, "fieldq_mar2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_mar2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_mar2021.csv"
)


#### APRIL 2021 ####
# -- no data
#### MAY 2021 ####
fieldq_may2021 <- GET(fieldq_urls[grep("TECR/2021-05", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_may2021, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_may2021 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2021-05.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_may2021$collectDate <- as.POSIXct(fieldq_may2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_may2021$collectDate <- as.POSIXct(q_may2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_may2021, "fieldq_may2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_may2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_may2021.csv"
)

#### JUNE 2021 ####
fieldq_june2021 <- GET(fieldq_urls[grep("TECR/2021-06", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_june2021, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_june2021 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2021-06.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_june2021$collectDate <- as.POSIXct(fieldq_june2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_june2021$collectDate <- as.POSIXct(q_june2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_june2021, "fieldq_june2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_june2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_june2021.csv"
)

#### JULY 2021 ####
fieldq_july2021 <- GET(fieldq_urls[grep("TECR/2021-07", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_july2021, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_july2021 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2021-07.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_july2021$collectDate <- as.POSIXct(fieldq_july2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2021$collectDate <- as.POSIXct(q_july2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_july2021, "fieldq_july2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_july2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_july2021.csv"
)

#### AUGUST 2021 ####
fieldq_aug2021 <- GET(fieldq_urls[grep("TECR/2021-08", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_aug2021, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_aug2021 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2021-08.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_aug2021$collectDate <- as.POSIXct(fieldq_aug2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_aug2021$collectDate <- as.POSIXct(q_aug2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_aug2021, "fieldq_aug2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_aug2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_aug2021.csv"
)

#### SEPTEMBER 2021 ####
fieldq_sept2021 <- GET(fieldq_urls[grep("TECR/2021-09", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_sept2021, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_sept2021 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2021-09.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_sept2021$collectDate <- as.POSIXct(fieldq_sept2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_sept2021$collectDate <- as.POSIXct(q_sept2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_sept2021, "fieldq_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_sept2021.csv"
)

#### OCTOBER 2021 ####
fieldq_oct2021 <- GET(fieldq_urls[grep("TECR/2021-10", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_oct2021, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_oct2021 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2021-10.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_oct2021$collectDate <- as.POSIXct(fieldq_oct2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2021$collectDate <- as.POSIXct(q_oct2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_oct2021, "fieldq_oct2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_oct2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_oct2021.csv"
)

#### NOVEMBER 2021 ####
fieldq_nov2021 <- GET(fieldq_urls[grep("TECR/2021-11", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_nov2021, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_nov2021 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2021-11.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_nov2021$collectDate <- as.POSIXct(fieldq_nov2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2021$collectDate <- as.POSIXct(q_nov2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_nov2021, "fieldq_nov2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_nov2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_nov2021.csv"
)

#### DECEMBER 2021 ####
fieldq_dec2021 <- GET(fieldq_urls[grep("TECR/2021-12", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_dec2021, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_dec2021 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2021-12.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_dec2021$collectDate <- as.POSIXct(fieldq_dec2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_dec2021$collectDate <- as.POSIXct(q_dec2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_dec2021, "fieldq_dec2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_dec2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_dec2021.csv"
)

#### JANUARY 2022 ####
# -- no data

#### FEBRUARY 2022 ####
fieldq_feb2022 <- GET(fieldq_urls[grep("TECR/2022-02", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_feb2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_feb2022 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2022-02.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_feb2022$collectDate <- as.POSIXct(fieldq_feb2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_feb2022$collectDate <- as.POSIXct(q_feb2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_feb2022, "fieldq_feb2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_feb2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_feb2022.csv"
)


#### MARCH 2022 ####
fieldq_mar2022 <- GET(fieldq_urls[grep("TECR/2022-03", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_mar2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_mar2022 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2022-03.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_mar2022$collectDate <- as.POSIXct(fieldq_mar2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_mar2022$collectDate <- as.POSIXct(q_mar2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_mar2022, "fieldq_mar2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_mar2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_mar2022.csv"
)

#### APRIL 2022 ####
fieldq_apr2022 <- GET(fieldq_urls[grep("TECR/2022-04", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_apr2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_apr2022 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2022-04.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_apr2022$collectDate <- as.POSIXct(fieldq_apr2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_apr2022$collectDate <- as.POSIXct(q_apr2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_apr2022, "fieldq_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_apr2022.csv"
)

#### MAY 2022 ####
fieldq_may2022 <- GET(fieldq_urls[grep("TECR/2022-05", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_may2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_may2022 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2022-05.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_may2022$collectDate <- as.POSIXct(fieldq_may2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_may2022$collectDate <- as.POSIXct(q_may2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_may2022, "fieldq_may2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_may2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_may2022.csv"
)

#### JUNE 2022 ####
fieldq_june2022 <- GET(fieldq_urls[grep("TECR/2022-06", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_june2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_june2022 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2022-06.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_june2022$collectDate <- as.POSIXct(fieldq_june2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_june2022$collectDate <- as.POSIXct(q_june2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_june2022, "fieldq_june2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_june2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_june2022.csv"
)

#### JULY 2022 ####
fieldq_july2022 <- GET(fieldq_urls[grep("TECR/2022-07", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_july2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_july2022 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2022-07.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_july2022$collectDate <- as.POSIXct(fieldq_july2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2022$collectDate <- as.POSIXct(q_july2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_july2022, "fieldq_july2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_july2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_july2022.csv"
)

#### AUGUST 2022 ####
fieldq_aug2022 <- GET(fieldq_urls[grep("TECR/2022-08", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_aug2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_aug2022 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2022-08.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_aug2022$collectDate <- as.POSIXct(fieldq_aug2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_aug2022$collectDate <- as.POSIXct(q_aug2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_aug2022, "fieldq_aug2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_aug2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_aug2022.csv"
)

#### SEPTEMBER 2022 ####
fieldq_sept2022 <- GET(fieldq_urls[grep("TECR/2022-09", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_sept2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_sept2022 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2022-09.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_sept2022$collectDate <- as.POSIXct(fieldq_sept2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_sept2022$collectDate <- as.POSIXct(q_sept2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_sept2022, "fieldq_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_sept2022.csv"
)

#### OCTOBER 2022 ####
fieldq_oct2022 <- GET(fieldq_urls[grep("TECR/2022-10", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_oct2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_oct2022 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2022-10.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_oct2022$collectDate <- as.POSIXct(fieldq_oct2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2022$collectDate <- as.POSIXct(q_oct2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_oct2022, "fieldq_oct2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_oct2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_oct2022.csv"
)

#### NOVEMBER 2022 ####
fieldq_nov2022 <- GET(fieldq_urls[grep("TECR/2022-11", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_nov2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_nov2022 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2022-11.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_nov2022$collectDate <- as.POSIXct(fieldq_nov2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2022$collectDate <- as.POSIXct(q_nov2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_nov2022, "fieldq_nov2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_nov2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_nov2022.csv"
)

#### DECEMBER 2022 ####
fieldq_dec2022 <- GET(fieldq_urls[grep("TECR/2022-12", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_dec2022, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_dec2022 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2022-12.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_dec2022$collectDate <- as.POSIXct(fieldq_dec2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_dec2022$collectDate <- as.POSIXct(q_dec2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_dec2022, "fieldq_dec2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_dec2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_dec2022.csv"
)

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
# -- no data

#### JULY 2023 ####
fieldq_july2023 <- GET(fieldq_urls[grep("TECR/2023-07", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_july2023, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_july2023 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2023-07.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_july2023$collectDate <- as.POSIXct(fieldq_july2023$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2023$collectDate <- as.POSIXct(q_july2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_july2023, "fieldq_july2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_july2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_july2023.csv"
)


#### AUGUST 2023 ####
fieldq_aug2023 <- GET(fieldq_urls[grep("TECR/2023-08", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_aug2023, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_aug2023 <- read.csv(fieldq_files$data$files$url
                            [grep("dsc_fieldData.2023-08.basic.",
                                  fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_aug2023$collectDate <- as.POSIXct(fieldq_aug2023$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_aug2023$collectDate <- as.POSIXct(q_aug2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_aug2023, "fieldq_aug2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_aug2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_aug2023.csv"
)

#### SEPTEMBER 2023 ####
fieldq_sept2023 <- GET(fieldq_urls[grep("TECR/2023-09", fieldq_urls)])
fieldq_files <- jsonlite::fromJSON(content(fieldq_sept2023, as = "text"))
fieldq_files$data$files$name

# look at the basic (not expanded) data table for Field Discharge 
fieldq_sept2023 <- read.csv(fieldq_files$data$files$url
                           [grep("dsc_fieldData.2023-09.basic.",
                                 fieldq_files$data$files$name)])

# reformat Date and Time
fieldq_sept2023$collectDate <- as.POSIXct(fieldq_sept2023$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_sept2023$collectDate <- as.POSIXct(q_sept2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(fieldq_sept2023, "fieldq_sept2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "fieldq_sept2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL"),
  name = "fieldq_sept2023.csv"
)

#### Read me ####
# # -- As of 12/05/2025, there were no data after September 2023.

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
fieldq <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1vYZBL9IYhINuo7vsnKxPFuOpZLjMS9EL")

# List and filter CSV files with "N" in their names
fieldq_files <- googledrive::drive_ls(path = fieldq, type = "csv")
fieldq_files <- fieldq_files[grepl("fieldq", fieldq_files$name), ]

# Create an empty list to store the cleaned data frames
fieldq_list <- lapply(seq_along(fieldq_files$name), function(i) {
  googledrive::drive_download(
    file = fieldq_files$id[i],
    path = paste0("googledrive/", fieldq_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file
  read.csv(paste0("googledrive/", fieldq_files$name[i]), header = TRUE)
})

# Assign names to the list elements based on the file names
names(fieldq_list) <- fieldq_files$name

# Check the contents of the list
str(fieldq_list)

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
  file = "all_fielddischarge_data.csv",
  row.names = FALSE
)

#### Upload CSV to the specific Google Drive folder ####
folder_id <- drive_get("Discharge field collection")

drive_upload(
  "all_fielddischarge_data.csv",
  path = folder_id,
)
