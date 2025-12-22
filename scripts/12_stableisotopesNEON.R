#### Read me ####
# -- The following code is what was used to access salt-based discharge data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Stable isotopes in surface water."

# Abstract: This data product contains the quality-controlled, native sampling 
# resolution data from NEON's stable isotopes in surface water sampling protocol. 
# Filters containing suspended particulate organic matter (POM) are sent to external 
# facilities for analysis to determine 15N/14N and 13C/12C particulate isotope 
# ratios and particulate carbon and nitrogen masses. Water samples are sent to 
# external facilities for analysis to determine 18O/12O and 2H/1H water isotope ratios. 
# For additional details, see the user guide, protocols, and science design listed 
# in the Documentation section in this data product's details webpage. 
# Latency: The expected time from data and/or sample collection in the field to 
# data publication is as follows, for each of the data tables (in days) in the 
# downloaded data package. See the Data Product User Guide for more information.

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
# data product name: "Stable isotopes in surface water" (DP1.20206.001)
isos_product <- GET("http://data.neonscience.org/api/v0/products/DP1.20206.001")
isos_product

# make the data readable by jsonlite
isos_text <- content(isos_product, as = "text")

# flatten json into a nested list
isos_avail <- jsonlite::fromJSON(isos_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
isos_avail$data$siteCodes

# TECR is 30th on the list
isos_avail$data$siteCodes$siteCode[[30]]

# check which months at TECR have available data
isos_avail$data$siteCodes$availableMonths[[30]]

# get a complete list of available data URLs
isos_urls <- unlist(isos_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(isos_urls)

# show the first 10 URLs available
isos_urls[1:10]

#### OCTOBER 2018 ####
isos_oct2018 <- GET(isos_urls[grep("TECR/2018-10", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_oct2018, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_oct2018 <- read.csv(isos_files$data$files$url
                           [grep("asi_externalLabH2OIsotopes.2018-10.expanded.",
                                 isos_files$data$files$name)])

# reformat Date and Time
isos_oct2018$collectDate <- as.POSIXct(isos_oct2018$collectDate, format = "%Y-%m-%d")
isos_oct2018$analysisDate <- as.POSIXct(isos_oct2018$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_oct2018, "isos_oct2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_oct2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_oct2018.csv"
)

#### NOVEMBER 2018 ####
isos_nov2018 <- GET(isos_urls[grep("TECR/2018-11", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_nov2018, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_nov2018 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2018-11.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_nov2018$collectDate <- as.POSIXct(isos_nov2018$collectDate, format = "%Y-%m-%d")
isos_nov2018$analysisDate <- as.POSIXct(isos_nov2018$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_nov2018, "isos_nov2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_nov2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_nov2018.csv"
)

#### DECEMBER 2018 ####
isos_dec2018 <- GET(isos_urls[grep("TECR/2018-12", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_dec2018, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_dec2018 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2018-12.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_dec2018$collectDate <- as.POSIXct(isos_dec2018$collectDate, format = "%Y-%m-%d")
isos_dec2018$analysisDate <- as.POSIXct(isos_dec2018$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_dec2018, "isos_dec2018.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_dec2018.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_dec2018.csv"
)

#### JANUARY 2019 ####
isos_jan2019 <- GET(isos_urls[grep("TECR/2019-01", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_jan2019, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_jan2019 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2019-01.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_jan2019$collectDate <- as.POSIXct(isos_jan2019$collectDate, format = "%Y-%m-%d")
isos_jan2019$analysisDate <- as.POSIXct(isos_jan2019$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_jan2019, "isos_jan2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_jan2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_jan2019.csv"
)

#### FEBRUARY 2019 ####
# -- no data

#### MARCH 2019 ####
isos_mar2019 <- GET(isos_urls[grep("TECR/2019-03", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_mar2019, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_mar2019 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2019-03.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_mar2019$collectDate <- as.POSIXct(isos_mar2019$collectDate, format = "%Y-%m-%d")
isos_mar2019$analysisDate <- as.POSIXct(isos_mar2019$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_mar2019, "isos_mar2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_mar2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_mar2019.csv"
)

#### APRIL 2019 ####
isos_apr2019 <- GET(isos_urls[grep("TECR/2019-04", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_apr2019, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_apr2019 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2019-04.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_apr2019$collectDate <- as.POSIXct(isos_apr2019$collectDate, format = "%Y-%m-%d")
isos_apr2019$analysisDate <- as.POSIXct(isos_apr2019$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_apr2019, "isos_apr2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_apr2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_apr2019.csv"
)

#### MAY 2019 ####
isos_may2019 <- GET(isos_urls[grep("TECR/2019-05", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_may2019, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_may2019 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2019-05.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_may2019$collectDate <- as.POSIXct(isos_may2019$collectDate, format = "%Y-%m-%d")
isos_may2019$analysisDate <- as.POSIXct(isos_may2019$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_may2019, "isos_may2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_may2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_may2019.csv"
)

#### JUNE 2019 ####
isos_june2019 <- GET(isos_urls[grep("TECR/2019-06", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_june2019, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_june2019 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2019-06.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_june2019$collectDate <- as.POSIXct(isos_june2019$collectDate, format = "%Y-%m-%d")
isos_june2019$analysisDate <- as.POSIXct(isos_june2019$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_june2019, "isos_june2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_june2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_june2019.csv"
)

#### JULY 2019 ####
isos_july2019 <- GET(isos_urls[grep("TECR/2019-07", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_july2019, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_july2019 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2019-07.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_july2019$collectDate <- as.POSIXct(isos_july2019$collectDate, format = "%Y-%m-%d")
isos_july2019$analysisDate <- as.POSIXct(isos_july2019$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_july2019, "isos_july2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_july2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_july2019.csv"
)

#### AUGUST 2019 ####
isos_aug2019 <- GET(isos_urls[grep("TECR/2019-08", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_aug2019, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_aug2019 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2019-08.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_aug2019$collectDate <- as.POSIXct(isos_aug2019$collectDate, format = "%Y-%m-%d")
isos_aug2019$analysisDate <- as.POSIXct(isos_aug2019$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_aug2019, "isos_aug2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_aug2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_aug2019.csv"
)

#### SEPTEMBER 2019 ####
isos_sept2019 <- GET(isos_urls[grep("TECR/2019-09", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_sept2019, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_sept2019 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2019-09.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_sept2019$collectDate <- as.POSIXct(isos_sept2019$collectDate, format = "%Y-%m-%d")
isos_sept2019$analysisDate <- as.POSIXct(isos_sept2019$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_sept2019, "isos_sept2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_sept2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_sept2019.csv"
)

#### OCTOBER 2019 ####
isos_oct2019 <- GET(isos_urls[grep("TECR/2019-10", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_oct2019, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_oct2019 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2019-10.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_oct2019$collectDate <- as.POSIXct(isos_oct2019$collectDate, format = "%Y-%m-%d")
isos_oct2019$analysisDate <- as.POSIXct(isos_oct2019$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_oct2019, "isos_oct2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_oct2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_oct2019.csv"
)

#### NOVEMBER 2019 ####
isos_nov2019 <- GET(isos_urls[grep("TECR/2019-11", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_nov2019, as = "text"))
isos_files$data$files$name

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_nov2019 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2019-11.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_nov2019$collectDate <- as.POSIXct(isos_nov2019$collectDate, format = "%Y-%m-%d")
isos_nov2019$analysisDate <- as.POSIXct(isos_nov2019$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_nov2019, "isos_nov2019.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_nov2019.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_nov2019.csv"
)

#### DECEMBER 2019 ####
# -- no data

#### JANUARY 2020 ####
isos_jan2020 <- GET(isos_urls[grep("TECR/2020-01", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_jan2020, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_jan2020 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2020-01.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_jan2020$collectDate <- as.POSIXct(isos_jan2020$collectDate, format = "%Y-%m-%d")
isos_jan2020$analysisDate <- as.POSIXct(isos_jan2020$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_jan2020, "isos_jan2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_jan2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_jan2020.csv"
)

#### FEBRUARY 2020 ####
isos_feb2020 <- GET(isos_urls[grep("TECR/2020-02", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_feb2020, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_feb2020 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2020-02.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_feb2020$collectDate <- as.POSIXct(isos_feb2020$collectDate, format = "%Y-%m-%d")
isos_feb2020$analysisDate <- as.POSIXct(isos_feb2020$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_feb2020, "isos_feb2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_feb2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_feb2020.csv"
)

#### MARCH 2020 ####
isos_mar2020 <- GET(isos_urls[grep("TECR/2020-03", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_mar2020, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_mar2020 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2020-03.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_mar2020$collectDate <- as.POSIXct(isos_mar2020$collectDate, format = "%Y-%m-%d")
isos_mar2020$analysisDate <- as.POSIXct(isos_mar2020$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_mar2020, "isos_mar2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_mar2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_mar2020.csv"
)

#### APRIL 2020 ####
# -- no data

#### MAY 2020 ####
# -- no data

#### JUNE 2020 ####
# -- no data

#### JULY 2020 ####
isos_july2020 <- GET(isos_urls[grep("TECR/2020-07", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_july2020, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_july2020 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2020-07.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_july2020$collectDate <- as.POSIXct(isos_july2020$collectDate, format = "%Y-%m-%d")
isos_july2020$analysisDate <- as.POSIXct(isos_july2020$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_july2020, "isos_july2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_july2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_july2020.csv"
)

#### AUGUST 2020 ####
# -- no data

#### SEPTEMBER 2020 ####
# -- no data

#### OCTOBER 2020 ####
isos_oct2020 <- GET(isos_urls[grep("TECR/2020-10", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_oct2020, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_oct2020 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2020-10.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_oct2020$collectDate <- as.POSIXct(isos_oct2020$collectDate, format = "%Y-%m-%d")
isos_oct2020$analysisDate <- as.POSIXct(isos_oct2020$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_oct2020, "isos_oct2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_oct2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_oct2020.csv"
)

#### NOVEMBER 2020 ####
isos_nov2020 <- GET(isos_urls[grep("TECR/2020-11", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_nov2020, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_nov2020 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2020-11.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_nov2020$collectDate <- as.POSIXct(isos_nov2020$collectDate, format = "%Y-%m-%d")
isos_nov2020$analysisDate <- as.POSIXct(isos_nov2020$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_nov2020, "isos_nov2020.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_nov2020.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_nov2020.csv"
)

#### DECEMBER 2020 ####
# -- no data

#### JANUARY 2021 ####
# -- no data

#### FEBRUARY 2021 ####
isos_feb2021 <- GET(isos_urls[grep("TECR/2021-02", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_feb2021, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_feb2021 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2021-02.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_feb2021$collectDate <- as.POSIXct(isos_feb2021$collectDate, format = "%Y-%m-%d")
isos_feb2021$analysisDate <- as.POSIXct(isos_feb2021$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_feb2021, "isos_feb2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_feb2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_feb2021.csv"
)

#### MARCH 2021 ####
isos_mar2021 <- GET(isos_urls[grep("TECR/2021-03", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_mar2021, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_mar2021 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2021-03.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_mar2021$collectDate <- as.POSIXct(isos_mar2021$collectDate, format = "%Y-%m-%d")
isos_mar2021$analysisDate <- as.POSIXct(isos_mar2021$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_mar2021, "isos_mar2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_mar2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_mar2021.csv"
)

#### APRIL 2021 ####
# -- no data

#### MAY 2021 ####
isos_may2021 <- GET(isos_urls[grep("TECR/2021-05", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_may2021, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_may2021 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2021-05.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_may2021$collectDate <- as.POSIXct(isos_may2021$collectDate, format = "%Y-%m-%d")
isos_may2021$analysisDate <- as.POSIXct(isos_may2021$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_may2021, "isos_may2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_may2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_may2021.csv"
)

#### JUNE 2021 ####
isos_june2021 <- GET(isos_urls[grep("TECR/2021-06", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_june2021, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_june2021 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2021-06.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_june2021$collectDate <- as.POSIXct(isos_june2021$collectDate, format = "%Y-%m-%d")
isos_june2021$analysisDate <- as.POSIXct(isos_june2021$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_june2021, "isos_june2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_june2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_june2021.csv"
)

#### JULY 2021 ####
isos_july2021 <- GET(isos_urls[grep("TECR/2021-07", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_july2021, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_july2021 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2021-07.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_july2021$collectDate <- as.POSIXct(isos_july2021$collectDate, format = "%Y-%m-%d")
isos_july2021$analysisDate <- as.POSIXct(isos_july2021$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_july2021, "isos_july2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_july2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_july2021.csv"
)

#### AUGUST 2021 ####
isos_aug2021 <- GET(isos_urls[grep("TECR/2021-08", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_aug2021, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_aug2021 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2021-08.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_aug2021$collectDate <- as.POSIXct(isos_aug2021$collectDate, format = "%Y-%m-%d")
isos_aug2021$analysisDate <- as.POSIXct(isos_aug2021$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_aug2021, "isos_aug2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_aug2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_aug2021.csv"
)

#### SEPTEMBER 2021 ####
isos_sept2021 <- GET(isos_urls[grep("TECR/2021-09", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_sept2021, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_sept2021 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2021-09.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_sept2021$collectDate <- as.POSIXct(isos_sept2021$collectDate, format = "%Y-%m-%d")
isos_sept2021$analysisDate <- as.POSIXct(isos_sept2021$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_sept2021, "isos_sept2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_sept2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_sept2021.csv"
)

#### OCTOBER 2021 ####
isos_oct2021 <- GET(isos_urls[grep("TECR/2021-10", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_oct2021, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_oct2021 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2021-10.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_oct2021$collectDate <- as.POSIXct(isos_oct2021$collectDate, format = "%Y-%m-%d")
isos_oct2021$analysisDate <- as.POSIXct(isos_oct2021$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_oct2021, "isos_oct2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_oct2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_oct2021.csv"
)

#### NOVEMBER 2021 ####
isos_nov2021 <- GET(isos_urls[grep("TECR/2021-11", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_nov2021, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_nov2021 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2021-11.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_nov2021$collectDate <- as.POSIXct(isos_nov2021$collectDate, format = "%Y-%m-%d")
isos_nov2021$analysisDate <- as.POSIXct(isos_nov2021$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_nov2021, "isos_nov2021.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_nov2021.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_nov2021.csv"
)

#### DECEMBER 2021 ####
# -- no data

#### JANUARY 2022 ####
isos_jan2022 <- GET(isos_urls[grep("TECR/2022-01", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_jan2022, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_jan2022 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2022-01.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_jan2022$collectDate <- as.POSIXct(isos_jan2022$collectDate, format = "%Y-%m-%d")
isos_jan2022$analysisDate <- as.POSIXct(isos_jan2022$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_jan2022, "isos_jan2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_jan2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_jan2022.csv"
)

#### FEBRUARY 2022 ####
isos_feb2022 <- GET(isos_urls[grep("TECR/2022-02", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_feb2022, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_feb2022 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2022-02.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_feb2022$collectDate <- as.POSIXct(isos_feb2022$collectDate, format = "%Y-%m-%d")
isos_feb2022$analysisDate <- as.POSIXct(isos_feb2022$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_feb2022, "isos_feb2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_feb2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_feb2022.csv"
)

#### MARCH 2022 ####
isos_mar2022 <- GET(isos_urls[grep("TECR/2022-03", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_mar2022, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_mar2022 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2022-03.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_mar2022$collectDate <- as.POSIXct(isos_mar2022$collectDate, format = "%Y-%m-%d")
isos_mar2022$analysisDate <- as.POSIXct(isos_mar2022$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_mar2022, "isos_mar2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_mar2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_mar2022.csv"
)

#### APRIL 2022 ####
isos_apr2022 <- GET(isos_urls[grep("TECR/2022-04", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_apr2022, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_apr2022 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2022-04.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_apr2022$collectDate <- as.POSIXct(isos_apr2022$collectDate, format = "%Y-%m-%d")
isos_apr2022$analysisDate <- as.POSIXct(isos_apr2022$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_apr2022, "isos_apr2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_apr2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_apr2022.csv"
)

#### MAY 2022 ####
isos_may2022 <- GET(isos_urls[grep("TECR/2022-05", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_may2022, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_may2022 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2022-05.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_may2022$collectDate <- as.POSIXct(isos_may2022$collectDate, format = "%Y-%m-%d")
isos_may2022$analysisDate <- as.POSIXct(isos_may2022$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_may2022, "isos_may2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_may2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_may2022.csv"
)

#### JUNE 2022 ####
isos_june2022 <- GET(isos_urls[grep("TECR/2022-06", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_june2022, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_june2022 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2022-06.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_june2022$collectDate <- as.POSIXct(isos_june2022$collectDate, format = "%Y-%m-%d")
isos_june2022$analysisDate <- as.POSIXct(isos_june2022$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_june2022, "isos_june2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_june2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_june2022.csv"
)

#### JULY 2022 ####
isos_july2022 <- GET(isos_urls[grep("TECR/2022-07", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_july2022, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_july2022 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2022-07.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_july2022$collectDate <- as.POSIXct(isos_july2022$collectDate, format = "%Y-%m-%d")
isos_july2022$analysisDate <- as.POSIXct(isos_july2022$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_july2022, "isos_july2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_july2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_july2022.csv"
)

#### AUGUST 2022 ####
isos_aug2022 <- GET(isos_urls[grep("TECR/2022-08", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_aug2022, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_aug2022 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2022-08.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_aug2022$collectDate <- as.POSIXct(isos_aug2022$collectDate, format = "%Y-%m-%d")
isos_aug2022$analysisDate <- as.POSIXct(isos_aug2022$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_aug2022, "isos_aug2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_aug2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_aug2022.csv"
)

#### SEPTEMBER 2022 ####
isos_sept2022 <- GET(isos_urls[grep("TECR/2022-09", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_sept2022, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_sept2022 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2022-09.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_sept2022$collectDate <- as.POSIXct(isos_sept2022$collectDate, format = "%Y-%m-%d")
isos_sept2022$analysisDate <- as.POSIXct(isos_sept2022$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_sept2022, "isos_sept2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_sept2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_sept2022.csv"
)

#### OCTOBER 2022 ####
# -- no data

#### NOVEMBER 2022 ####
isos_nov2022 <- GET(isos_urls[grep("TECR/2022-11", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_nov2022, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_nov2022 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2022-11.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_nov2022$collectDate <- as.POSIXct(isos_nov2022$collectDate, format = "%Y-%m-%d")
isos_nov2022$analysisDate <- as.POSIXct(isos_nov2022$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_nov2022, "isos_nov2022.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_nov2022.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_nov2022.csv"
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
isos_june2023 <- GET(isos_urls[grep("TECR/2023-06", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_june2023, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_june2023 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2023-06.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_june2023$collectDate <- as.POSIXct(isos_june2023$collectDate, format = "%Y-%m-%d")
isos_june2023$analysisDate <- as.POSIXct(isos_june2023$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_june2023, "isos_june2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_june2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_june2023.csv"
)

#### JULY 2023 ####
isos_july2023 <- GET(isos_urls[grep("TECR/2023-07", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_july2023, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_july2023 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2023-07.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_july2023$collectDate <- as.POSIXct(isos_july2023$collectDate, format = "%Y-%m-%d")
isos_july2023$analysisDate <- as.POSIXct(isos_july2023$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_july2023, "isos_july2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_july2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_july2023.csv"
)

#### AUGUST 2023 ####
isos_aug2023 <- GET(isos_urls[grep("TECR/2023-08", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_aug2023, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_aug2023 <- read.csv(isos_files$data$files$url
                          [grep("asi_externalLabH2OIsotopes.2023-08.expanded.",
                                isos_files$data$files$name)])

# reformat Date and Time
isos_aug2023$collectDate <- as.POSIXct(isos_aug2023$collectDate, format = "%Y-%m-%d")
isos_aug2023$analysisDate <- as.POSIXct(isos_aug2023$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_aug2023, "isos_aug2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_aug2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_aug2023.csv"
)

#### SEPTEMBER 2023 ####
# -- no data

#### OCTOBER 2023 ####
isos_oct2023 <- GET(isos_urls[grep("TECR/2023-10", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_oct2023, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_oct2023 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2023-10.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_oct2023$collectDate <- as.POSIXct(isos_oct2023$collectDate, format = "%Y-%m-%d")
isos_oct2023$analysisDate <- as.POSIXct(isos_oct2023$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_oct2023, "isos_oct2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_oct2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_oct2023.csv"
)

#### NOVEMBER 2023 ####
isos_nov2023 <- GET(isos_urls[grep("TECR/2023-11", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_nov2023, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_nov2023 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2023-11.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_nov2023$collectDate <- as.POSIXct(isos_nov2023$collectDate, format = "%Y-%m-%d")
isos_nov2023$analysisDate <- as.POSIXct(isos_nov2023$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_nov2023, "isos_nov2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_nov2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_nov2023.csv"
)

#### DECEMBER 2023 ####
isos_dec2023 <- GET(isos_urls[grep("TECR/2023-12", isos_urls)])
isos_files <- jsonlite::fromJSON(content(isos_dec2023, as = "text"))
isos_files$data$files$name 

# look at the expanded data table for isotopes 
# the expanded dataset (in this case) is more useful because it contains standard deviations and other metadata
isos_dec2023 <- read.csv(isos_files$data$files$url
                         [grep("asi_externalLabH2OIsotopes.2023-12.expanded.",
                               isos_files$data$files$name)])

# reformat Date and Time
isos_dec2023$collectDate <- as.POSIXct(isos_dec2023$collectDate, format = "%Y-%m-%d")
isos_dec2023$analysisDate <- as.POSIXct(isos_dec2023$analysisDate, format = "%Y-%m-%d")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(isos_dec2023, "isos_dec2023.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "isos_dec2023.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v"),
  name = "isos_dec2023.csv"
)

#### Read me ####
# # -- As of 12/16/2025, all data after December 2023 were "provisional." 

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
isotopes <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1hDLy_qtmEtq9mSNJFf8yPN9_R8Qi3s1v")

# List and filter CSV files with "N" in their names
isotopes_files <- googledrive::drive_ls(path = isotopes, type = "csv")
isotopes_files <- isotopes_files[grepl("isos", isotopes_files$name), ]

# Create an empty list to store the cleaned data frames
isotopes_list <- lapply(seq_along(isotopes_files$name), function(i) {
  googledrive::drive_download(
    file = isotopes_files$id[i],
    path = paste0("googledrive/", isotopes_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file
  read.csv(paste0("googledrive/", isotopes_files$name[i]), header = TRUE)
})

# Assign names to the list elements based on the file names
names(isotopes_list) <- isotopes_files$name

# Check the contents of the list
str(isotopes_list)

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
  file = "all_isotope_data.csv",
  row.names = FALSE
)

#### Upload CSV to the specific Google Drive folder ####
folder_id <- drive_get("Stable isotopes in surface water")

drive_upload(
  "all_isotope_data.csv",
  path = folder_id,
)
