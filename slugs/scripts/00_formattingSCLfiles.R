##==============================================================================
## Project: Teakettle
## Script to format all the Solinst Dataloggers to a cleaner 
## This is currently set up to run locally on a computer with no access to google drive (to allow for field use)
## press Command+Option+O to collapse all sections and get an overview of the workflow!

##==============================================================================

##############
## Packages ##
##############
library(tidyverse)

#####################
#### Import Data ####
#####################

# list and filter CSV files with "SCL" in their names
SCL_files <- list.files(
  path  = "in_use",
  pattern = "SCL.*\\.csv$",
  full.names = TRUE)


# create a list of cleaned data frames from local CSV files
scl_list <- lapply(SCL_files, function(file_path) {
  # read the CSV
  df <- read.csv(file_path)
  return(df)
})


scl_list <- lapply(SCL_files, function(file_path) {
  read.csv(file_path, skip = 13, header = TRUE, check.names = FALSE)
})

# assign names to the list elements based on the file names
names(scl_list) <- basename(SCL_files)

# check the contents of the list
str(scl_list)

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(scl_list)) {
  # access the current data frame
  df <- scl_list[[i]]
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # update the data frame in the list
  scl_list[[i]] <- df
}

###################################
#### Format names to match YSI ####
###################################
for (i in seq_along(scl_list)) {
  # access the current data frame
  df <- scl_list[[i]]
  
  # rename columns
  df <- df %>%
    dplyr::rename(Temp.C. = TEMPERATURE,
           Conductivity = CONDUCTIVITY)
         
  scl_list[[i]] <-  df
}

# check the contents of the list
str(scl_list)

#########################################
#### Calculate specific conductivity ####
#########################################
for (i in seq_along(scl_list)) {
  # access the current data frame
  df <- scl_list[[i]]
  
  # calculate specific conductivity
  df <- df %>%
    mutate(SPC.uS.cm. = Conductivity/(1+0.02*(Temp.C. - 25)))

  scl_list[[i]] <-  df
}

####################################
#### Save edited slugs to in_use folder ####
####################################

# loop through each data frame in the list
for (i in seq_along(scl_list)) {
  
  df <- scl_list[[i]]

  # save to local "in_use" folder using existing filenames
  write.csv(
    df,
    file = file.path("in_use", names(scl_list)[i]),
    row.names = FALSE,
    quote = FALSE
  )
}

