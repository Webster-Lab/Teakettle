#### Read me ####
# This script may be used for analyzing 2025 fuels data. I (Elliot) only went as
# far as pulling the data into R from Google Drive so that it could be stored
# in the GitHub repo.

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
drive_download("2025_fuelsData.csv", path = "2025_fuelsData.csv", overwrite = TRUE)

#### Load data into R ####
fuels25 <- read.csv("2025_fuelsData.csv")
