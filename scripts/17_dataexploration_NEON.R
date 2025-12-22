#### Read me ####
# The following code is for exploratory data analysis of NEON data. It utilizes
# the "merged" datasets from the "Merged datasets" folder in Google Drive. 
# The only data not included here is the salt-based discharge dataset. See notes 
# in "08_saltdischargeNEON.R" for an explanation.

#### Libraries #### 
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("googledrive")
library(dplyr)
library(tidyr)
library(ggplot2)
library(googledrive) # Make sure Google Drive authentication is updated, otherwise the connection won't work.

#### Load data ####
drive_find(n_max = 10) 

# Download the desired file to the working directory
drive_download("all_dissolvedgases_data.csv", path = "all_dissolvedgases_data.csv", overwrite = TRUE)
drive_download("all_elevation_data.csv", path = "all_elevation_data.csv", overwrite = TRUE)
drive_download("all_fielddischarge_data.csv", path = "all_fielddischarge_data.csv", overwrite = TRUE)
drive_download("all_gaugeheight_data.csv", path = "all_gaugeheight_data.csv", overwrite = TRUE)
drive_download("all_isotope_data.csv", path = "all_isotope_data.csv", overwrite = TRUE)
drive_download("all_nitrate_data.csv", path = "all_nitrate_data.csv", overwrite = TRUE)
drive_download("all_ratingcurve_data.csv", path = "all_ratingcurve_data.csv", overwrite = TRUE)
drive_download("all_temp_data.csv", path = "all_temp_data.csv", overwrite = TRUE)
drive_download("all_waterchem_data.csv", path = "all_waterchem_data.csv", overwrite = TRUE)
drive_download("all_waterquality_data.csv", path = "all_waterquality_data.csv", overwrite = TRUE)

#### Load data into R ####
# omitted rating curve because it's... different
gases <- read.csv("all_dissolvedgases_data.csv")
elevation <- read.csv("all_elevation_data.csv")
fieldQ <- read.csv("all_fielddischarge_data.csv")
gaugeheight <- read.csv("all_gaugeheight_data.csv")
isotopes <- read.csv("all_isotope_data.csv")
nitrate <- read.csv("all_nitrate_data.csv")
temp <- read.csv("all_temp_data.csv")
chem <- read.csv("all_waterchem_data.csv")
wq <- read.csv("all_waterquality_data.csv")

#### Make plots ####
# list the datasets so you can use a "for" loop
data_list <- list(gases, elevation, fieldQ, gaugeheight, isotopes, nitrate, temp, chem, wq)

plots < list()