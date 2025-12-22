#### Read me ####
# -- The following code is what was used to access gases data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Dissolved gases in surface water." 

# Abstract: This data product contains the quality-controlled, native sampling 
# resolution data from NEON's surface water dissolved gas sampling protocol. 
# Water samples are equilibrated with air in the field. Samples of reference 
# air (pre-equilibration) and equilibrated air (post-equilibration) are sent to 
# external facilities for analysis to determine carbon dioxide, methane, and 
# nitrous oxide concentrations in the gas samples. Data users should refer to 
# the user guide for dissolved gases in surface water (NEON_dissolvGasInWater_UserGuide) 
# for suggestions on how to calculate dissolved concentrations of carbon dioxide, 
# methane, and nitrous oxide in the surface waters from which samples were 
# collected using Henry's Law and mass balance equations. For additional details, 
# see the user guide, protocols, and science design listed in the Documentation 
# section in this data product's details webpage.

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
# data product name: "Dissolved gases in surface water" (DP1.20097.001)
gases_product <- GET("http://data.neonscience.org/api/v0/products/DP1.20097.001")
gases_product

# make the data readable by jsonlite
gases_text <- content(gases_product, as = "text")

# flatten json into a nested list
gases_avail <- jsonlite::fromJSON(gases_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
gases_avail$data$siteCodes

# TECR is 30th on the list
gases_avail$data$siteCodes$siteCode[[30]]

# check which months at TECR have available data
gases_avail$data$siteCodes$availableMonths[[30]]

# get a complete list of available data URLs
gases_urls <- unlist(gases_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(gases_urls)

# show the first 10 URLs available
gases_urls[1:10]

#### OCTOBER 2018 ####
gases_oct2018 <- GET(gases_urls[grep("TECR/2018-10", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_oct2018, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_oct2018 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2018-10.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_oct2018$collectDate <- as.POSIXct(gases_oct2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2018$collectDate <- as.POSIXct(q_oct2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_oct2018, "gases_oct2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_oct2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_oct2018.csv"
)

#### NOVEMBER 2018 ####
gases_nov2018 <- GET(gases_urls[grep("TECR/2018-11", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_nov2018, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_nov2018 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2018-11.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_nov2018$collectDate <- as.POSIXct(gases_nov2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2018$collectDate <- as.POSIXct(q_nov2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_nov2018, "gases_nov2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_nov2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_nov2018.csv"
)

#### DECEMBER 2018 ####
gases_dec2018 <- GET(gases_urls[grep("TECR/2018-12", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_dec2018, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_dec2018 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2018-12.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_dec2018$collectDate <- as.POSIXct(gases_dec2018$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_dec2018$collectDate <- as.POSIXct(q_dec2018$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_dec2018, "gases_dec2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_dec2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_dec2018.csv"
)

#### JANUARY 2019 ####
gases_jan2019 <- GET(gases_urls[grep("TECR/2019-01", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_jan2019, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_jan2019 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2019-01.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_jan2019$collectDate <- as.POSIXct(gases_jan2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_jan2019$collectDate <- as.POSIXct(q_jan2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_jan2019, "gases_jan2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_jan2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_jan2019.csv"
)

#### FEBRUARY 2019 ####
# -- no data

#### MARCH 2019 ####
gases_mar2019 <- GET(gases_urls[grep("TECR/2019-03", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_mar2019, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_mar2019 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2019-03.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_mar2019$collectDate <- as.POSIXct(gases_mar2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_mar2019$collectDate <- as.POSIXct(q_mar2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_mar2019, "gases_mar2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_mar2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_mar2019.csv"
)

#### APRIL 2019 ####
gases_apr2019 <- GET(gases_urls[grep("TECR/2019-04", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_apr2019, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_apr2019 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2019-04.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_apr2019$collectDate <- as.POSIXct(gases_apr2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_apr2019$collectDate <- as.POSIXct(q_apr2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_apr2019, "gases_apr2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_apr2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_apr2019.csv"
)

#### MAY 2019 ####
gases_may2019 <- GET(gases_urls[grep("TECR/2019-05", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_may2019, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_may2019 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2019-05.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_may2019$collectDate <- as.POSIXct(gases_may2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_may2019$collectDate <- as.POSIXct(q_may2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_may2019, "gases_may2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_may2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_may2019.csv"
)

#### JUNE 2019 ####
gases_june2019 <- GET(gases_urls[grep("TECR/2019-06", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_june2019, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_june2019 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2019-06.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_june2019$collectDate <- as.POSIXct(gases_june2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_june2019$collectDate <- as.POSIXct(q_june2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_june2019, "gases_june2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_june2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_june2019.csv"
)

#### JULY 2019 ####
gases_july2019 <- GET(gases_urls[grep("TECR/2019-07", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_july2019, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_july2019 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2019-07.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_july2019$collectDate <- as.POSIXct(gases_july2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2019$collectDate <- as.POSIXct(q_july2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_july2019, "gases_july2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_july2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_july2019.csv"
)

#### AUGUST 2019 ####
gases_aug2019 <- GET(gases_urls[grep("TECR/2019-08", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_aug2019, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_aug2019 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2019-08.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_aug2019$collectDate <- as.POSIXct(gases_aug2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_aug2019$collectDate <- as.POSIXct(q_aug2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_aug2019, "gases_aug2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_aug2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_aug2019.csv"
)

#### SEPTEMBER 2019 ####
gases_sept2019 <- GET(gases_urls[grep("TECR/2019-09", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_sept2019, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_sept2019 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2019-09.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_sept2019$collectDate <- as.POSIXct(gases_sept2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_sept2019$collectDate <- as.POSIXct(q_sept2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_sept2019, "gases_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_sept2019.csv"
)

#### OCTOBER 2019 ####
gases_oct2019 <- GET(gases_urls[grep("TECR/2019-10", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_oct2019, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_oct2019 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2019-10.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_oct2019$collectDate <- as.POSIXct(gases_oct2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2019$collectDate <- as.POSIXct(q_oct2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_oct2019, "gases_oct2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_oct2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_oct2019.csv"
)

#### NOVEMBER 2019 ####
gases_nov2019 <- GET(gases_urls[grep("TECR/2019-11", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_nov2019, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_nov2019 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2019-11.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_nov2019$collectDate <- as.POSIXct(gases_nov2019$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2019$collectDate <- as.POSIXct(q_nov2019$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_nov2019, "gases_nov2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_nov2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_nov2019.csv"
)

#### DECEMBER 2019 ####
# -- no data

#### JANUARY 2020 ####
gases_jan2020 <- GET(gases_urls[grep("TECR/2020-01", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_jan2020, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_jan2020 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2020-01.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_jan2020$collectDate <- as.POSIXct(gases_jan2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_jan2020$collectDate <- as.POSIXct(q_jan2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_jan2020, "gases_jan2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_jan2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_jan2020.csv"
)

#### FEBRUARY 2020 ####
gases_feb2020 <- GET(gases_urls[grep("TECR/2020-02", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_feb2020, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_feb2020 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2020-02.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_feb2020$collectDate <- as.POSIXct(gases_feb2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_feb2020$collectDate <- as.POSIXct(q_feb2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_feb2020, "gases_feb2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_feb2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_feb2020.csv"
)

#### MARCH 2020 ####
gases_mar2020 <- GET(gases_urls[grep("TECR/2020-03", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_mar2020, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_mar2020 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2020-03.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_mar2020$collectDate <- as.POSIXct(gases_mar2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_mar2020$collectDate <- as.POSIXct(q_mar2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_mar2020, "gases_mar2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_mar2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_mar2020.csv"
)

#### APRIL 2020 ####
# -- no data

#### MAY 2020 ####
# -- no data

#### JUNE 2020 ####
# -- no data

#### JULY 2020 ####
gases_july2020 <- GET(gases_urls[grep("TECR/2020-07", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_july2020, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_july2020 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2020-07.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_july2020$collectDate <- as.POSIXct(gases_july2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2020$collectDate <- as.POSIXct(q_july2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_july2020, "gases_july2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_july2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_july2020.csv"
)

#### AUGUST 2020 ####
# -- no data

#### SEPTEMBER 2020 ####
# -- no data

#### OCTOBER 2020 ####
gases_oct2020 <- GET(gases_urls[grep("TECR/2020-10", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_oct2020, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_oct2020 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2020-10.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_oct2020$collectDate <- as.POSIXct(gases_oct2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2020$collectDate <- as.POSIXct(q_oct2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_oct2020, "gases_oct2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_oct2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_oct2020.csv"
)

#### NOVEMBER 2020 ####
gases_nov2020 <- GET(gases_urls[grep("TECR/2020-11", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_nov2020, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_nov2020 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2020-11.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_nov2020$collectDate <- as.POSIXct(gases_nov2020$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2020$collectDate <- as.POSIXct(q_nov2020$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_nov2020, "gases_nov2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_nov2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_nov2020.csv"
)

#### DECEMBER 2020 ####
# -- no data 

#### JANUARY 2021 ####
# -- no data

#### FEBRUARY 2021 ####
gases_feb2021 <- GET(gases_urls[grep("TECR/2021-02", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_feb2021, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_feb2021 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2021-02.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_feb2021$collectDate <- as.POSIXct(gases_feb2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_feb2021$collectDate <- as.POSIXct(q_feb2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_feb2021, "gases_feb2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_feb2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_feb2021.csv"
)

#### MARCH 2021 ####
gases_mar2021 <- GET(gases_urls[grep("TECR/2021-03", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_mar2021, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_mar2021 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2021-03.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_mar2021$collectDate <- as.POSIXct(gases_mar2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_mar2021$collectDate <- as.POSIXct(q_mar2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_mar2021, "gases_mar2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_mar2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_mar2021.csv"
)

#### APRIL 2021 ####
# -- no data

#### MAY 2021 ####
gases_may2021 <- GET(gases_urls[grep("TECR/2021-05", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_may2021, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_may2021 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2021-05.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_may2021$collectDate <- as.POSIXct(gases_may2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_may2021$collectDate <- as.POSIXct(q_may2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_may2021, "gases_may2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_may2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_may2021.csv"
)

#### JUNE 2021 ####
gases_june2021 <- GET(gases_urls[grep("TECR/2021-06", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_june2021, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_june2021 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2021-06.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_june2021$collectDate <- as.POSIXct(gases_june2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_june2021$collectDate <- as.POSIXct(q_june2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_june2021, "gases_june2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_june2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_june2021.csv"
)

#### JULY 2021 ####
gases_july2021 <- GET(gases_urls[grep("TECR/2021-07", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_july2021, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_july2021 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2021-07.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_july2021$collectDate <- as.POSIXct(gases_july2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2021$collectDate <- as.POSIXct(q_july2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_july2021, "gases_july2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_july2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_july2021.csv"
)

#### AUGUST 2021 ####
gases_aug2021 <- GET(gases_urls[grep("TECR/2021-08", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_aug2021, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_aug2021 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2021-08.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_aug2021$collectDate <- as.POSIXct(gases_aug2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_aug2021$collectDate <- as.POSIXct(q_aug2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_aug2021, "gases_aug2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_aug2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_aug2021.csv"
)

#### SEPTEMBER 2021 ####
gases_sept2021 <- GET(gases_urls[grep("TECR/2021-09", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_sept2021, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_sept2021 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2021-09.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_sept2021$collectDate <- as.POSIXct(gases_sept2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_sept2021$collectDate <- as.POSIXct(q_sept2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_sept2021, "gases_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_sept2021.csv"
)

#### OCTOBER 2021 ####
gases_oct2021 <- GET(gases_urls[grep("TECR/2021-10", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_oct2021, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_oct2021 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2021-10.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_oct2021$collectDate <- as.POSIXct(gases_oct2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2021$collectDate <- as.POSIXct(q_oct2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_oct2021, "gases_oct2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_oct2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_oct2021.csv"
)

#### NOVEMBER 2021 ####
gases_nov2021 <- GET(gases_urls[grep("TECR/2021-11", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_nov2021, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_nov2021 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2021-11.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_nov2021$collectDate <- as.POSIXct(gases_nov2021$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2021$collectDate <- as.POSIXct(q_nov2021$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_nov2021, "gases_nov2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_nov2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_nov2021.csv"
)

#### DECEMBER 2021 ####
# -- no data

#### JANUARY 2022 ####
gases_jan2022 <- GET(gases_urls[grep("TECR/2022-01", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_jan2022, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_jan2022 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2022-01.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_jan2022$collectDate <- as.POSIXct(gases_jan2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_jan2022$collectDate <- as.POSIXct(q_jan2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_jan2022, "gases_jan2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_jan2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_jan2022.csv"
)

#### FEBRUARY 2022 ####
gases_feb2022 <- GET(gases_urls[grep("TECR/2022-02", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_feb2022, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_feb2022 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2022-02.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_feb2022$collectDate <- as.POSIXct(gases_feb2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_feb2022$collectDate <- as.POSIXct(q_feb2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_feb2022, "gases_feb2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_feb2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_feb2022.csv"
)

#### MARCH 2022 ####
gases_mar2022 <- GET(gases_urls[grep("TECR/2022-03", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_mar2022, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_mar2022 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2022-03.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_mar2022$collectDate <- as.POSIXct(gases_mar2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_mar2022$collectDate <- as.POSIXct(q_mar2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_mar2022, "gases_mar2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_mar2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_mar2022.csv"
)

#### APRIL 2022 ####
gases_apr2022 <- GET(gases_urls[grep("TECR/2022-04", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_apr2022, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_apr2022 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2022-04.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_apr2022$collectDate <- as.POSIXct(gases_apr2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_apr2022$collectDate <- as.POSIXct(q_apr2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_apr2022, "gases_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_apr2022.csv"
)

#### MAY 2022 ####
gases_may2022 <- GET(gases_urls[grep("TECR/2022-05", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_may2022, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_may2022 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2022-05.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_may2022$collectDate <- as.POSIXct(gases_may2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_may2022$collectDate <- as.POSIXct(q_may2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_may2022, "gases_may2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_may2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_may2022.csv"
)

#### JUNE 2022 ####
gases_june2022 <- GET(gases_urls[grep("TECR/2022-06", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_june2022, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_june2022 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2022-06.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_june2022$collectDate <- as.POSIXct(gases_june2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_june2022$collectDate <- as.POSIXct(q_june2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_june2022, "gases_june2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_june2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_june2022.csv"
)

#### JULY 2022 ####
gases_july2022 <- GET(gases_urls[grep("TECR/2022-07", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_july2022, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_july2022 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2022-07.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_july2022$collectDate <- as.POSIXct(gases_july2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2022$collectDate <- as.POSIXct(q_july2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_july2022, "gases_july2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_july2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_july2022.csv"
)

#### AUGUST 2022 ####
gases_aug2022 <- GET(gases_urls[grep("TECR/2022-08", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_aug2022, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_aug2022 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2022-08.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_aug2022$collectDate <- as.POSIXct(gases_aug2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_aug2022$collectDate <- as.POSIXct(q_aug2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_aug2022, "gases_aug2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_aug2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_aug2022.csv"
)

#### SEPTEMBER 2022 ####
gases_sept2022 <- GET(gases_urls[grep("TECR/2022-09", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_sept2022, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_sept2022 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2022-09.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_sept2022$collectDate <- as.POSIXct(gases_sept2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_sept2022$collectDate <- as.POSIXct(q_sept2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_sept2022, "gases_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_sept2022.csv"
)

#### OCTOBER 2022 ####
# -- no data

#### NOVEMBER 2022 ####
gases_nov2022 <- GET(gases_urls[grep("TECR/2022-11", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_nov2022, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_nov2022 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2022-11.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_nov2022$collectDate <- as.POSIXct(gases_nov2022$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2022$collectDate <- as.POSIXct(q_nov2022$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_nov2022, "gases_nov2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_nov2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_nov2022.csv"
)

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
gases_june2023 <- GET(gases_urls[grep("TECR/2023-06", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_june2023, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_june2023 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2023-06.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_june2023$collectDate <- as.POSIXct(gases_june2023$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_june2023$collectDate <- as.POSIXct(q_june2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_june2023, "gases_june2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_june2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_june2023.csv"
)

#### JULY 2023 ####
gases_july2023 <- GET(gases_urls[grep("TECR/2023-07", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_july2023, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_july2023 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2023-07.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_july2023$collectDate <- as.POSIXct(gases_july2023$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_july2023$collectDate <- as.POSIXct(q_july2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_july2023, "gases_july2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_july2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_july2023.csv"
)

#### AUGUST 2023 ####
gases_aug2023 <- GET(gases_urls[grep("TECR/2023-08", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_aug2023, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_aug2023 <- read.csv(gases_files$data$files$url
                           [grep("sdg_externalLabData.2023-08.basic.",
                                 gases_files$data$files$name)])

# reformat Date and Time
gases_aug2023$collectDate <- as.POSIXct(gases_aug2023$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_aug2023$collectDate <- as.POSIXct(q_aug2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_aug2023, "gases_aug2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_aug2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_aug2023.csv"
)

#### SEPTEMBER 2023 ####
# -- no data

#### OCTOBER 2023 ####
gases_oct2023 <- GET(gases_urls[grep("TECR/2023-10", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_oct2023, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_oct2023 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2023-10.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_oct2023$collectDate <- as.POSIXct(gases_oct2023$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_oct2023$collectDate <- as.POSIXct(q_oct2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_oct2023, "gases_oct2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_oct2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_oct2023.csv"
)

#### NOVEMBER 2023 ####
gases_nov2023 <- GET(gases_urls[grep("TECR/2023-11", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_nov2023, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_nov2023 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2023-11.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_nov2023$collectDate <- as.POSIXct(gases_nov2023$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_nov2023$collectDate <- as.POSIXct(q_nov2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_nov2023, "gases_nov2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_nov2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_nov2023.csv"
)

#### DECEMBER 2023 ####
gases_dec2023 <- GET(gases_urls[grep("TECR/2023-12", gases_urls)])
gases_files <- jsonlite::fromJSON(content(gases_dec2023, as = "text"))
gases_files$data$files$name

# look at the basic (not expanded) data table for dissolved gases 
gases_dec2023 <- read.csv(gases_files$data$files$url
                          [grep("sdg_externalLabData.2023-12.basic.",
                                gases_files$data$files$name)])

# reformat Date and Time
gases_dec2023$collectDate <- as.POSIXct(gases_dec2023$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_dec2023$collectDate <- as.POSIXct(q_dec2023$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(gases_dec2023, "gases_dec2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "gases_dec2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN"),
  name = "gases_dec2023.csv"
)

#### Read me ####
# # -- As of 12/09/2025, all data after December 2023 were "provisional." 

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
gases <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1TTpoS4TFpXiGr2eBiwdbHI-GANDBwoTN")

# List and filter CSV files with "N" in their names
gases_files <- googledrive::drive_ls(path = gases, type = "csv")
gases_files <- gases_files[grepl("gases", gases_files$name), ]

# Create an empty list to store the cleaned data frames
gases_list <- lapply(seq_along(gases_files$name), function(i) {
  googledrive::drive_download(
    file = gases_files$id[i],
    path = paste0("googledrive/", gases_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file
  read.csv(paste0("googledrive/", gases_files$name[i]), header = TRUE)
})

# Assign names to the list elements based on the file names
names(gases_list) <- gases_files$name

# Check the contents of the list
str(gases_list)

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
  file = "all_dissolvedgases_data.csv",
  row.names = FALSE
)

#### Upload CSV to the specific Google Drive folder ####
folder_id <- drive_get("Dissolved gases in surface water")

drive_upload(
  "all_dissolvedgases_data.csv",
  path = folder_id,
)

