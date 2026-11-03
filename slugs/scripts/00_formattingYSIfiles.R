
##==============================================================================
## Project: Teakettle
## Script to format all the YSI files to a cleaner state
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##############
## Packages ##
##############
library(tidyverse) 


#####################
#### Import Data ####
#####################

# list and filter CSV files with "YSI" in their names
YSI_files <- list.files(
  path  = "in_use",
  pattern = "YSI.*\\.csv$",
  full.names = TRUE)

# Create an empty list to store the cleaned data frames
ysi_list <- lapply(YSI_files, function(file_path) {
  read.csv(
    file_path,
    sep = ";",
    header = TRUE,
    skip = 1,           # 👈 skip the "sep=;" line
    check.names = FALSE
  )
})

# assign names to the list elements based on the file names
names(ysi_list) <- basename(YSI_files)

# check the contents of the list
str(ysi_list)

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(ysi_list)) {
  # Access the current data frame
  df <- ysi_list[[i]]
  # make date into date format
  df$Date <- as.Date(df$Date, format = "%y/%m/%d")
  # update the data frame in the list
  ysi_list[[i]] <- df
}

#### Fix the SPC column name ####
ysi_list <- lapply(ysi_list, function(df) {
  names(df)[names(df) == "SPC(uS/cm)"] <- "SPC.uS.cm."
  df
})
####################################
#### Save edited slugs to in_use folder ####
####################################
#loop through each data frame in the list
for (i in seq_along(ysi_list)) {

  df <- ysi_list[[i]]
  
  # save to local "in_use" folder using existing filenames
  write.csv(
    df,
    file = file.path("in_use", names(ysi_list)[i]),
    row.names = FALSE,
    quote = FALSE
  )
}

