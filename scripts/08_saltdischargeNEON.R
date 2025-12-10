#### Read me ####
# -- The following code is what was used to access salt-based discharge data through 
# NEON's API. Only published data were used. All CSVs were placed 
# in the Google Drive folder, "Salt-based stream discharge."

# Abstract: This data product contains the quality-controlled, native sampling 
# resolution data for NEON's Salt-based Discharge data product. 
# The data for this data product is collected as part of the wadeable stream 
# reaeration sampling protocol. Salt concentration data from both constant rate, 
# NaCl or NaBr, and slug, NaCl, injections at wadeable stream sites are published 
# as part of this data product. Data users should refer to the user guide for 
# reaeration and salt-based discharge (see NEON_ReaerSaltBasedQ_userGuide) for 
# suggestions on how to calculate discharge values from the published data packages. 
# For additional details, see the user guide, protocols, and science design listed 
# in the Documentation section in this data product's details webpage.

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
# data product name: "Salt-based stream discharge" (DP1.20193.001)
saltq_product <- GET("http://data.neonscience.org/api/v0/products/DP1.20193.001")
saltq_product

# make the data readable by jsonlite
saltq_text <- content(saltq_product, as = "text")

# flatten json into a nested list
saltq_avail <- jsonlite::fromJSON(saltq_text, simplifyDataFrame = T, flatten = T)

# check "site codes" in data frame to make sure TECR is included
saltq_avail$data$siteCodes

# TECR is 30th on the list
saltq_avail$data$siteCodes$siteCode[[22]]

# check which months at TECR have available data
saltq_avail$data$siteCodes$availableMonths[[22]]

# get a complete list of available data URLs
saltq_urls <- unlist(saltq_avail$data$siteCodes$availableDataUrls)

# total number of URLs
length(saltq_urls)

# show the first 10 URLs available
saltq_urls[1:10]

#### JUNE 2017 ####
saltq_june2017 <- GET(saltq_urls[grep("TECR/2017-06", saltq_urls)])
saltq_files <- jsonlite::fromJSON(content(saltq_june2017, as = "text"))
saltq_files$data$files$name

# look at the basic (not expanded) data table for Salt-based Discharge 
saltq_june2017 <- read.csv(saltq_files$data$files$url
                           [grep("sbd_fieldData.2017-06.basic.",
                                 saltq_files$data$files$name)])

# reformat Date and Time
saltq_june2017$collectDate <- as.POSIXct(saltq_june2017$collectDate, format = "%Y-%m-%dT%H:%MZ")
# q_june2017$collectDate <- as.POSIXct(q_june2017$collectDate, format = "%Y-%m-%dT%H:%MZ")

## Save the edited file to Google Drive ##
# Step 1: write the csv to a local file
write.csv(saltq_june2017, "saltq_june2017.csv", row.names = F)

# Step 2: upload to Drive
drive_upload(
  media = "saltq_june2017.csv",
  path = as_id("https://drive.google.com/drive/u/0/folders/1JyEtf_4KDG9SwtedySrDD-yt3mSVPfbZ"),
  name = "saltq_june2017.csv"
)
