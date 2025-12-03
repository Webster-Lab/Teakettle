#### Read me: Overview of salt slug code ####
# The purpose of this script is to 1) format salt slug data files, and 2) to compute discharge for each site. All data are from the 2025 field season. 
# The code used was replicated and adapted from the QuEST Project, with minor adjustments to account for project differences. 
# For most sites, data were trimmed to remove long tails, which were artifacts of long windows in which the loggers were programmed to run.
# Data were processed in the following order:
## a) "Good" SCL files. These files only required trimming of long tails to be usable. The curves were reliable with no deeper data cleaning required.
## b) "Probelmatic" SCL files. These files required both trimming of long tails and deeper data cleaning to be useful.
## c)  YSI files. These files were generally clean and reliable, requiring minor trimming and cleaning.
# After formatting, I computed discharge and generated figures for each group of files. 


## ------------------------------- SCL Good ------------------------------- ##

#### Libraries ####
library(googledrive) 
library(tidyverse)

#### Local folders ####
# List and delete all files in these folders...
files <- list.files(path = "slugs", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#### Load data ####
# Set up Google Drive folder
Saltslugs <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FSw7-6C_Nqf9W-fkH1yxvncargFQOniJ")

# List and filter CSV files with "SCL" in their names
SCL_files <- googledrive::drive_ls(path = Saltslugs, type = "csv")
SCL_files <- SCL_files[grepl("SCL", SCL_files$name), ]

# Create an empty list to store the cleaned data frames
scl_list <- lapply(seq_along(SCL_files$name), function(i) {
  googledrive::drive_download(
    file = SCL_files$id[i],
    path = paste0("googledrive/", SCL_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file, skipping the first 13 rows (header is on row 14)
  read.csv(paste0("googledrive/", SCL_files$name[i]), skip = 13, header = TRUE)
})

# Assign names to the list elements based on the file names
names(scl_list) <- SCL_files$name

# Check the contents of the list
str(scl_list)

#### Format date/time ####
# Loop through each data frame in the list
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  
  # Make date into date format
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  
  # Update the data frame in the list
  scl_list[[i]] <- df
}

#### Format names to match YSI ####
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  
  # Rename columns
  df <- df %>%
    dplyr::rename(Temp.C. = TEMPERATURE,
                  Conductivity = CONDUCTIVITY)
  
  scl_list[[i]] <-  df
}

#### Calculate specific conductivity ####
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  
  # Calculate specific conductivity
  df <- df %>%
    mutate(SPC.uS.cm. = Conductivity/(1+0.02*(Temp.C. - 25)))
  
  scl_list[[i]] <-  df
}

# Check the contents of the list
str(scl_list)

#### Remove zeros from SpC column ####
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  
  # Remove zeros from SpC column
  df <- filter(df, SPC.uS.cm. > 0)
  
  scl_list[[i]] <- df
}

# Check the contents
str(scl_list)

#### Save edited slugs to Google Drive ####
# Loop through each data frame in the list
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  
  # Save new data frame
  write.csv(df, paste0("slugs/", SCL_files$name[i]), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("slugs/", SCL_files$name[i])
  # this is the in use folder
  drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1so4_jvn7cBNmyLLEYGMpWSoxKuKFppAT"
  
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}


#### Libraries: calculating Q - SCL Good ####
library(changepoint) 
library(googledrive) 
library(googlesheets4)
library(tibble)

lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

#### Local folders ####
# List and delete all files in these folders...
files <- list.files(path = "saltslug_figs", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "slugs", full.names = TRUE)
file.remove(files)
#### Load data from Google drive ####
# This is the inuse folder
Saltslugs <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1so4_jvn7cBNmyLLEYGMpWSoxKuKFppAT")

# List all CSV files in the folder
Saltslugs_csvs <- googledrive::drive_ls(path = Saltslugs, type = "csv")

## Call all the files in the salt slugs folder ##
# Create empty list to store data frames
csv_list <- list()

# Loop over each file in the `Saltslugs_csvs` data frame
for (i in seq_along(Saltslugs_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", Saltslugs_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = Saltslugs_csvs$id[i],
    path = local_path,
    overwrite = T
  )
  # Read the CSV file and add it to the list
  csv_list[[Saltslugs_csvs$name[i]]] <- read.csv(local_path)
}

# Check the contents of the list
str(csv_list)

#### Format date/time ####
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  
  # Make date into date format
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  # Combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  
  # Update the data frame in the list
  csv_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(csv_list)
#### Plot curves ####
# Visualize
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + ggtitle(paste(Saltslugs_csvs$name[i])) 
  # Display the plot in the plot panel
  print(p)
}
#### Add data ID to loggers ####
# Loggers do not have a column indicating which site the measurement was made on
# So for those instruments that don't have a "DataID" column, we will create one here
# Function to extract the ID from the file name
extract_id <- function(file_name) {
  str_extract(file_name, "TE(CR|AK)\\d{2}[A-Z]?")
}

# Add the new DataID number
for (i in seq_along(csv_list)) {
  df <- csv_list[[i]]
  file_name <- Saltslugs_csvs$name[i]
  # Call the name extracted from the file
  extracted_id <- extract_id(file_name)
  # Add it to the data frame
  df <- add_column(df, DataID = extracted_id)
  # Update the data frame in the list
  csv_list[[i]] <- df
} 

# But remove the extra data column for YSIs
for (i in seq_along(csv_list)) {
  df <- csv_list[[i]]
  # Check if the column exists, then remove it
  if ("DataID.1" %in% colnames(df)) {
    df <- select(df, -DataID.1)
  }
  # Update the data frame in the list
  csv_list[[i]] <- df
}
#### Calculate average background SpC ####
# Using the change point time in the curve, we are going to calculate the background SpC
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  
  # Remove rows with NA values
  df <- na.omit(df)
  
  # Get the change point from the SPC
  cpt <- cpt.mean(df$SPC.uS.cm., penalty='SIC', method='PELT', minseglen=2)
  # Get the change point
  # Get the change point index
  changepoint_index <- cpts(cpt)[1]
  
  # Extract the date-time at the change point index
  changepoint_datetime <- df$DateTime[changepoint_index]
  
  # Calculate the median SPC from the start to the change point
  df$medianSPC <- median(df$SPC.uS.cm.[1:changepoint_index], na.rm = TRUE)
  
  # Add the change point index as a new column to the data frame
  df$changepoint_index <- changepoint_index
  
  # Add the change point date-time as a new column to the data frame
  df$changepoint_datetime <- changepoint_datetime
  
  # Update the data frame in the list
  csv_list[[i]] <- df
}

#### Determine background conductivity ####
## Select area before or after the salt wave which is constant
## you want to calculate the average background conductivity from the measurement

# Load data frame with salt grams per site and injection time

## Load data from Google drive ##
(Q <- drive_get("https://docs.google.com/spreadsheets/d/1-1b1mEK1ILnhD0aY9oi_EMqSNgQA4qPJS4sUOJMMZeY/edit?gid=249902212#gid=249902212"))

# Download the file as a csv file
drive_download(as_id(Q$id), path = "2025_SCL_goodsites", type = "csv", overwrite = T)

# Fetch the file
salt <- read.csv("2025_SCL_goodsites.csv")

# Clean the column names (this removes spaces, special characters, etc.)
salt <- salt %>%
  janitor::clean_names()

# Rename columns and convert types
salt <- salt %>%
  # Rename columns
  dplyr::rename(
    DataID = site_code,
    Date = date,
    Time24h = time,
    reach = distance_from_injection_to_logger_m,
    salt = salt_weight_g,
    inj_time = injection_time_local,
    background = background_sp_c
  ) %>%
  # Convert
  mutate(
    reach = as.numeric(as.character(reach)),
    salt = as.numeric(as.character(salt)),
    background = as.numeric(as.character(background)),
    # Convert the injection DateTime column to POSIXct
    injection_time = as.POSIXct(paste(Date, inj_time, sep = " "), format = "%Y-%m-%d %H:%M"),
    # Change date format
    Date = as.Date(Date, format = "%Y-%m-%d"),
    # 1 g salt in 1 L of water gives cond = 2100 uS/cm
    Cond_mass = salt * 2100
  )

colnames(salt)

# Remove rows that I don't want
# salt <- salt[-1,] # removed row because site did not receive a slug

# Replace empties with NA
salt["notes"][salt["notes"] == ''] <- NA
salt["flag_notes"][salt["flag_notes"] == ''] <- NA
#### Combine salt info to csvs ####
# Function to combine info
combine_info <- function(df, info) {
  df <- df %>%
    mutate(Date = as.Date(Date))
  info <- info %>%
    mutate(Date = as.Date(Date))
  
  merged_df <- df %>%
    left_join(info, by = c("DataID", "Date"))
  
  return(merged_df)
}

# Function to combine info
combine_info <- function(df, info) {
  # Ensure that the columns 'DataID' and 'Date' are present in both data frames
  if (!all(c("DataID", "Date") %in% colnames(df))) {
    stop("Columns 'DataID' and 'Date' not found in the data frame.")
  }
  if (!all(c("DataID", "Date") %in% colnames(info))) {
    stop("Columns 'DataID' and 'Date' not found in the info data frame.")
  }
  
  # Convert 'Date' columns to Date type if they are not already
  if (!inherits(df$Date, "Date")) {
    df$Date <- as.Date(df$Date)
  }
  if (!inherits(info$Date, "Date")) {
    info$Date <- as.Date(info$Date)
  }
  # Merge the data frames
  merged_df <- merge(df, info, by = c("DataID", "Date"), all.x = TRUE)
  return(merged_df)
}

# Apply the function to the list of data frames
csvs <- lapply(csv_list, function(df) {
  tryCatch({
    combine_info(df, info = salt)
  }, error = function(e) {
    cat("Error in merging data frame:\n")
    print(e)
    return(df)
  })
})

#### Visualize where the cut is to start measuring ####
# Create line color vector
legend_colors <- c("changepoint" = "red", "injection" = "blue")

# Loop through each data frame in the list
for (i in seq_along(csvs)) { 
  # Access the current data frame
  df <- csvs[[i]]
  
  # Create a data frame for the vertical lines
  vlines <- data.frame(
    xintercept = c(df$changepoint_datetime[1], df$inj_time[1]),
    type = c("changepoint", "injection")
  )
  
  # Create the plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + 
    ggtitle(Saltslugs_csvs$name[i]) +
    geom_vline(data = vlines, aes(xintercept = xintercept, color = type, linetype = type)) +
    scale_color_manual(values = legend_colors) +
    scale_linetype_manual(values = c("changepoint" = "dashed", "injection" = "dashed")) +
    labs(color = "colors", linetype = "colors")
  # Save the plot as a PNG file
  ggsave(paste0("saltslug_figs/", Saltslugs_csvs$name[i], ".png"), plot = p)
  # Display the plot in the plot panel
  print(p)
}
#### Find lowest point after the peak ####
# Initialize a new data frame to store results
result_df <- data.frame(DataID = character(), medianSPC = numeric(), final_SPC = numeric(), Date = as.Date(character()), final_index = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(csvs)) {
  # Access the current data frame
  df <- csvs[[i]]
  
  # Calculate condcorr and peak index
  condcorr <- df$SPC.uS.cm. - df$medianSPC[1]
  
  # Find the peak of the slug
  peak_index <- which.max(condcorr)
  
  # Find the final index (SPC closest to zero after the peak)
  condcorr_after_peak <- condcorr[(peak_index + 1):length(condcorr)]
  closest_to_zero_index_after_peak <- which.min(abs(condcorr_after_peak))
  final_index <- peak_index + closest_to_zero_index_after_peak
  
  # Get the SPC value at final_index
  final_SPC <- df$SPC.uS.cm.[final_index]
  
  # Add to the results data frame
  result_df <- rbind(result_df, data.frame(DataID = df$DataID[1], Date = df$Date[1], medianSPC = df$medianSPC[1], final_SPC = final_SPC, final_index = final_index))
}

#### Calculate difference in tail ####
result_df$SPC_difference = result_df$medianSPC - result_df$final_SPC

#### Remove curve tail ####
# Create empty list to store cleaned data frames
csv_clean <- list()

for (i in seq_along(csvs)) {
  # Access the current data frame
  df <- csvs[[i]]
  
  # Perform cleaning or processing (e.g., trimming rows)
  df <- df[1:result_df$final_index[i], ]
  
  # Use the corresponding name from Saltslugs_csvs as the list name
  name <- gsub(".csv", "", Saltslugs_csvs$name[i])  # Strip the .csv extension
  csv_clean[[name]] <- df  # Assign the cleaned data frame with the corresponding name
}
#### Plot curves after removing tail ####
# Visualize
# Loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # Access the current data frame
  df <- csv_clean[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + ggtitle(paste(Saltslugs_csvs$name[i])) +
    geom_point() + ggtitle(paste(Saltslugs_csvs$name[i])) 
  # Display the plot in the plot panel
  print(p)
}
#### Estimate Q ####
# Create empty df to store Q data 
Q_list <- data.frame(DataID = c(), 
                     Q = c())

## Equation ##
Qint<-function(time, cond, bkg, condmass){
  condcorr<-cond-bkg
  
  ##below routine integrates
  ydiff<- condcorr[-1]+ condcorr[-length(condcorr)]
  condint<-sum(diff(time)*ydiff/2)
  
  Q<-condmass/condint
  Q
  
}

## Apply the equation to each salt slug in your folder ##
# Loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # Access the current data frame
  df <- csv_clean[[i]]
  # Indicate all variables and values for the equation 
  df$Q<- Qint(as.numeric(df$DateTime), df$SPC.uS.cm., df$medianSPC[[1]], df$Cond_mass[[1]])
  
  csv_clean[[i]] <-  df
}
#### Add Q values to table ####
# Specify the columns to select
selected_columns <- c("DataID", "Date", "salt", "background", "medianSPC", "Q", "flag_notes")

# Extract the first value from specified columns in each data frame
first_values <- lapply(csv_clean, function(df) {
  df[selected_columns] %>% slice(1)
})

# Combine these values into a new single data frame
combined_data_frame <- bind_rows(first_values)

#### Add SpC values to salt table ####
mergedQ <- combined_data_frame %>%
  left_join(result_df, by = c("DataID", "Date"))

# But remove the extra medianSPC column
mergedQ <- select(mergedQ, -medianSPC.y)

#### Combine SpC info to csvs ####
# Function to combine info
combine_info <- function(df, info) {
  # Merge the data frames
  merged_df <- merge(df, info, by = c("DataID", "Date", "medianSPC"), all.x = TRUE)
  return(merged_df)
}

# Apply the function to the list of data frames
csv_clean <- lapply(csv_clean, function(df) {
  tryCatch({
    combine_info(df, info = result_df)
  }, error = function(e) {
    cat("Error in merging data frame:\n")
    print(e)
    return(df)
  })
})

#### Plot curves with flags ####
# Loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # Access the current data frame
  df <- csv_clean[[i]]
  
  # Extract values from the data frame
  flag_note <- df$flag_notes[1]
  median_spc <- df$medianSPC[1]
  final_spc <- df$final_SPC
  background_value <- df$background[1]
  Q <- df$Q[1]
  
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + 
    ggtitle(paste(Saltslugs_csvs$name[i])) +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.), 
             label = flag_note, color = "#CD2626", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.1 * max(df$SPC.uS.cm.), 
             label = paste(median_spc, "median SPC"), color = "#00688B", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.15 * max(df$SPC.uS.cm.), 
             label = paste(final_spc, "final SPC"), color = "plum", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.2 * max(df$SPC.uS.cm.), 
             label = paste(background_value, "bckg"), color = "#008B00", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.25 * max(df$SPC.uS.cm.), 
             label = paste(Q, "L/sec"), color = "goldenrod2", size = 4)
  # Display the plot in the plot panel
  print(p)
  
  # Save the plot as a PNG file
  ggsave(paste0("saltslug_figs/", Saltslugs_csvs$name[i], ".png"), plot = p)
}
#### Save images to Google Drive ####
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "saltslug_figs/"
drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1GxLig7V_lpzpkV1AZXfK1W9TVj0FmFkB"

# List all files in the local folder
files <- list.files(local_folder, full.names = TRUE)

# Upload each file to the specified Google Drive folder
lapply(files, function(file) {
  drive_upload(
    media = file,
    path = as_id(drive_folder_id)
  )
})

#### Save Q data frame to Google Drive ####
## Load data from Google drive ##
# (Q <- drive_get("https://drive.google.com/drive/folders/1UkRaYRBePgY9XU90_3DvURNGGEWbCew0"))

# List all files in the folder
# Q_files <- drive_ls(Q)

# Download the most recent CSV file
# drive_download(as_id(Q_files$id), path = "field_data/Q.csv", overwrite = TRUE)

# Fetch the file
# Q <- read.csv("field_data/Q.csv")

# Convert the injection DateTime column to POSIXct
# Q$Date <- as.Date(Q$Date, format = "%m/%d/%y")

# Combine by row
# Q <- rbind(Q, mergedQ)

# Save new data frame
write.csv(mergedQ, "field_data/2025_SCL_goodsites.csv", row.names=FALSE, quote=FALSE) 

# Define the local folder path and the target folder ID in Google Drive
file <- "field_data/2025_SCL_goodsites.csv"
drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1DqiX5xFcZnjoVZakAXhw0lZt-B8f6Jsd"

# Upload file to the specified Google Drive folder
drive_upload(
  media = file,
  path = as_id(drive_folder_id)
)
#### Save edited slugs to Google Drive ####
# Loop through each data frame in the list
for (i in seq_along(csvs)) {
  # Access the current data frame
  df <- csvs[[i]]
  
  # Save new data frame
  write.csv(df, paste0("slugs/", Saltslugs_csvs$name[i]), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("slugs/", Saltslugs_csvs$name[i])
  drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1H2yezNu-6i3r9uM4JoJY9H4Pi64B-8EX"
  
  # Upload file to the specified Google Drive folder
  drive_upload(
    media = file,
    path = as_id(drive_folder_id)
  )
}


## ----------------------------- SCL Problematic --------------------------- ##

#### Libraries ####
library(googledrive) 
library(tidyverse)

#### Local folders ####
# List and delete all files in these folder
files <- list.files(path = "slugs", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#### Load data ####
# Set up Google Drive folder
Saltslugs <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1dkA5RWaoIg0aRxHvnLaeacLx998FoMfQ")

# List and filter CSV files with "SCL" in their names
SCL_files <- googledrive::drive_ls(path = Saltslugs, type = "csv")
SCL_files <- SCL_files[grepl("SCL", SCL_files$name), ]

# Create an empty list to store the cleaned data frames
scl_list <- lapply(seq_along(SCL_files$name), function(i) {
  googledrive::drive_download(
    file = SCL_files$id[i],
    path = paste0("googledrive/", SCL_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file, skipping the first 13 rows (header is on row 14)
  read.csv(paste0("googledrive/", SCL_files$name[i]), skip = 13, header = TRUE)
})

# Assign names to the list elements based on the file names
names(scl_list) <- SCL_files$name

# Check the contents of the list
str(scl_list)

#### Format date/time ####
# Loop through each data frame in the list
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  
  # Make date into date format
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  
  # Update the data frame in the list
  scl_list[[i]] <- df
}
#### Format names to match YSI ####
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  
  # Rename columns
  df <- df %>%
    dplyr::rename(Temp.C. = TEMPERATURE,
                  Conductivity = CONDUCTIVITY)
  
  scl_list[[i]] <-  df
}

#### Calculate specific conductivity ####
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  
  # Calculate specific conductivity
  df <- df %>%
    mutate(SPC.uS.cm. = Conductivity/(1+0.02*(Temp.C. - 25)))
  
  scl_list[[i]] <-  df
}

# Check the contents of the list
str(scl_list)

#### Remove zeros from SpC column ####
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  
  # Remove zeros from SpC column
  df <- filter(df, SPC.uS.cm. > 0)
  
  scl_list[[i]] <- df
}

# Check the contents
str(scl_list)

#### Save edited slugs to Google Drive ####
# Loop through each data frame in the list
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  
  # Save new data frame
  write.csv(df, paste0("slugs/", SCL_files$name[i]), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("slugs/", SCL_files$name[i])
  # this is the in use folder
  drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1rJ8cb6qk_26oIlX1gJ_VW0LPmHRIzAx7"
  
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}


#### Libraries ####
library(changepoint) # Calculate changepoint in data
library(googledrive) 
library(googlesheets4)
library(tibble)

lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

#### Local folders ####
# List and delete all files in these folder
files <- list.files(path = "saltslug_figs", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "slugs", full.names = TRUE)
file.remove(files)

#### Load data from Google Drive ####
# This is the inuse folder
Saltslugs <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1DTTJxr2wRg2WGG_8uY7p1nb-IDg2fEX3")

# List all CSV files in the folder
Saltslugs_csvs <- googledrive::drive_ls(path = Saltslugs, type = "csv")

## Call all the files in the salt slugs folder ##
# Create empty list to store data frames
csv_list <- list()

# Loop over each file in the `Saltslugs_csvs` data frame
for (i in seq_along(Saltslugs_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", Saltslugs_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = Saltslugs_csvs$id[i],
    path = local_path,
    overwrite = T
  )
  # Read the CSV file and add it to the list
  csv_list[[Saltslugs_csvs$name[i]]] <- read.csv(local_path)
}

# Check the contents of the list
str(csv_list)


#### Format date/time ####
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  
  # Make date into date format
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  # Combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
  
  # Update the data frame in the list
  csv_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(csv_list)
str(df)

#### Plot curves ####
# Visualize
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + ggtitle(paste(Saltslugs_csvs$name[i])) 
  # Display the plot in the plot panel
  print(p)
}

#### Add data ID to loggers ####
# Loggers do not have a column indicating which site the measurement was made on
# So for those instruments that don't have a "DataID" column, we will create one here
# Function to extract the ID from the file name
extract_id <- function(file_name) {
  str_extract(file_name, "TE(CR|AK)\\d{2}")
}

# Add the new DataID number
for (i in seq_along(csv_list)) {
  df <- csv_list[[i]]
  file_name <- Saltslugs_csvs$name[i]
  # Call the name extracted from the file
  extracted_id <- extract_id(file_name)
  # Add it to the data frame
  df <- add_column(df, DataID = extracted_id)
  # Update the data frame in the list
  csv_list[[i]] <- df
} 

# But remove the extra data column for YSIs
for (i in seq_along(csv_list)) {
  df <- csv_list[[i]]
  # Check if the column exists, then remove it
  if ("DataID.1" %in% colnames(df)) {
    df <- select(df, -DataID.1)
  }
  # Update the data frame in the list
  csv_list[[i]] <- df
}

#### Calculate average background SpC ####
# Using the change point time in the curve, we are going to calculate the background SpC
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  
  # Remove rows with NA values
  df <- na.omit(df)
  
  # Get the change point from the SPC
  cpt <- cpt.mean(df$SPC.uS.cm., penalty='SIC', method='PELT', minseglen=2)
  # Get the change point
  # Get the change point index
  changepoint_index <- cpts(cpt)[1]
  
  # Extract the date-time at the change point index
  changepoint_datetime <- df$DateTime[changepoint_index]
  
  # Calculate the median SPC from the start to the change point
  df$medianSPC <- median(df$SPC.uS.cm.[1:changepoint_index], na.rm = TRUE)
  
  # Add the change point index as a new column to the data frame
  df$changepoint_index <- changepoint_index
  
  # Add the change point date-time as a new column to the data frame
  df$changepoint_datetime <- changepoint_datetime
  
  # Update the data frame in the list
  csv_list[[i]] <- df
}

#### Determine background conductivity ####
## Select area before or after the salt wave which is constant
## you want to calculate the average background conductivity from the measurement

# Load data frame with salt grams per site and injection time

## Load data from Google drive ##
(Q <- drive_get("https://docs.google.com/spreadsheets/d/1-1b1mEK1ILnhD0aY9oi_EMqSNgQA4qPJS4sUOJMMZeY/edit?gid=230509760#gid=230509760"))

# Download the file as a csv file
drive_download(as_id(Q$id), path = "2025_SCL_salvagesites", type = "csv", overwrite = T)

# Fetch the file
salt <- read.csv("2025_SCL_salvagesites.csv")

# Clean the column names (this removes spaces, special characters, etc.)
salt <- salt %>%
  janitor::clean_names()

# Rename columns and convert types
salt <- salt %>%
  # Rename columns
  dplyr::rename(
    DataID = site_code,
    Date = date,
    Time24h = time,
    reach = distance_from_injection_to_logger_m,
    salt = salt_weight_g,
    inj_time = injection_time_local,
    background = background_sp_c
  ) %>%
  # Convert
  mutate(
    reach = as.numeric(as.character(reach)),
    salt = as.numeric(as.character(salt)),
    background = as.numeric(as.character(background)),
    # Convert the injection DateTime column to POSIXct
    injection_time = as.POSIXct(paste(Date, inj_time, sep = " "), format = "%Y-%m-%d %H:%M"),
    # Change date format
    Date = as.Date(Date, format = "%Y-%m-%d"),
    # 1 g salt in 1 L of water gives cond = 2100 uS/cm
    Cond_mass = salt * 2100
  )

colnames(salt)

# Remove rows that I don't want
# salt <- salt[-1,] # removed row because site did not receive a slug

# Replace empties with NA
salt["notes"][salt["notes"] == ''] <- NA
salt["flag_notes"][salt["flag_notes"] == ''] <- NA

#### Combine salt info to csvs ####
# Function to combine info
combine_info <- function(df, info) {
  df <- df %>%
    mutate(Date = as.Date(Date))
  info <- info %>%
    mutate(Date = as.Date(Date))
  
  merged_df <- df %>%
    left_join(info, by = c("DataID", "Date"))
  
  return(merged_df)
}

# Function to combine info
combine_info <- function(df, info) {
  # Ensure that the columns 'DataID' and 'Date' are present in both data frames
  if (!all(c("DataID", "Date") %in% colnames(df))) {
    stop("Columns 'DataID' and 'Date' not found in the data frame.")
  }
  if (!all(c("DataID", "Date") %in% colnames(info))) {
    stop("Columns 'DataID' and 'Date' not found in the info data frame.")
  }
  
  # Convert 'Date' columns to Date type if they are not already
  if (!inherits(df$Date, "Date")) {
    df$Date <- as.Date(df$Date)
  }
  if (!inherits(info$Date, "Date")) {
    info$Date <- as.Date(info$Date)
  }
  # Merge the data frames
  merged_df <- merge(df, info, by = c("DataID", "Date"), all.x = TRUE)
  return(merged_df)
}

# Apply the function to the list of data frames
csvs <- lapply(csv_list, function(df) {
  tryCatch({
    combine_info(df, info = salt)
  }, error = function(e) {
    cat("Error in merging data frame:\n")
    print(e)
    return(df)
  })
})

#### Visualize where the cut is to start measuring ####
# Create line color vector
legend_colors <- c("changepoint" = "red", "injection" = "blue")

# Loop through each data frame in the list
for (i in seq_along(csvs)) { 
  # Access the current data frame
  df <- csvs[[i]]
  
  # Create a data frame for the vertical lines
  vlines <- data.frame(
    xintercept = c(df$changepoint_datetime[1], df$inj_time[1]),
    type = c("changepoint", "injection")
  )
  
  # Create the plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + 
    ggtitle(Saltslugs_csvs$name[i]) +
    geom_vline(data = vlines, aes(xintercept = xintercept, color = type, linetype = type)) +
    scale_color_manual(values = legend_colors) +
    scale_linetype_manual(values = c("changepoint" = "dashed", "injection" = "dashed")) +
    labs(color = "colors", linetype = "colors")
  # Save the plot as a PNG file
  ggsave(paste0("saltslug_figs/", Saltslugs_csvs$name[i], ".png"), plot = p)
  # Display the plot in the plot panel
  print(p)
}

#### Find lowest point after the peak ####
# Initialize a new data frame to store results
result_df <- data.frame(DataID = character(), medianSPC = numeric(), final_SPC = numeric(), Date = as.Date(character()), final_index = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(csvs)) {
  # Access the current data frame
  df <- csvs[[i]]
  
  # Calculate condcorr and peak index
  condcorr <- df$SPC.uS.cm. - df$medianSPC[1]
  
  # Find the peak of the slug
  peak_index <- which.max(condcorr)
  
  # Find the final index (SPC closest to zero after the peak)
  condcorr_after_peak <- condcorr[(peak_index + 1):length(condcorr)]
  closest_to_zero_index_after_peak <- which.min(abs(condcorr_after_peak))
  final_index <- peak_index + closest_to_zero_index_after_peak
  
  # Get the SPC value at final_index
  final_SPC <- df$SPC.uS.cm.[final_index]
  
  # Add to the results data frame
  result_df <- rbind(result_df, data.frame(DataID = df$DataID[1], Date = df$Date[1], medianSPC = df$medianSPC[1], final_SPC = final_SPC, final_index = final_index))
}

#### Calculate difference in tail ####
result_df$SPC_difference = result_df$medianSPC - result_df$final_SPC

#### Remove curve tail ####
# Create empty list to store cleaned data frames
csv_clean <- list()

for (i in seq_along(csvs)) {
  # Access the current data frame
  df <- csvs[[i]]
  
  # Perform cleaning or processing (e.g., trimming rows)
  df <- df[1:result_df$final_index[i], ]
  
  # Use the corresponding name from Saltslugs_csvs as the list name
  name <- gsub(".csv", "", Saltslugs_csvs$name[i])  # Strip the .csv extension
  csv_clean[[name]] <- df  # Assign the cleaned data frame with the corresponding name
}

#### Plot curves after removing tail ####
# Visualize
# Loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # Access the current data frame
  df <- csv_clean[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + ggtitle(paste(Saltslugs_csvs$name[i])) +
    geom_point() + ggtitle(paste(Saltslugs_csvs$name[i])) 
  # Display the plot in the plot panel
  print(p)
}

#### Estimate Q ####
## Calculate Q!
## Units = L/sec
#example of how it looks for ONE salt slug
#Q_USF5 <- Qint(as.numeric(log_20240524_USF5$DateTime), log_20240524_USF5$SPC, USF5, USF5_Cond_mass)

# Create empty df to store Q data 
Q_list <- data.frame(DataID = c(), 
                     Q = c())

## Equation ##
Qint<-function(time, cond, bkg, condmass){
  condcorr<-cond-bkg
  
  ##below routine integrates
  ydiff<- condcorr[-1]+ condcorr[-length(condcorr)]
  condint<-sum(diff(time)*ydiff/2)
  
  Q<-condmass/condint
  Q
  
}

## Apply the equation to each salt slug in your folder ##
# Loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # Access the current data frame
  df <- csv_clean[[i]]
  # Indicate all variables and values for the equation 
  df$Q<- Qint(as.numeric(df$DateTime), df$SPC.uS.cm., df$medianSPC[[1]], df$Cond_mass[[1]])
  
  csv_clean[[i]] <-  df
}

#### Add Q values to table ####
# Specify the columns to select
selected_columns <- c("DataID", "Date", "salt", "background", "medianSPC", "Q", "flag_notes")

# Extract the first value from specified columns in each data frame
first_values <- lapply(csv_clean, function(df) {
  df[selected_columns] %>% slice(1)
})

# Combine these values into a new single data frame
combined_data_frame <- bind_rows(first_values)

#### Add SpC values to salt table ####
mergedQ <- combined_data_frame %>%
  left_join(result_df, by = c("DataID", "Date"))

# But remove the extra medianSPC column
mergedQ <- select(mergedQ, -medianSPC.y)

#### Combine SpC info to csvs ####
# Function to combine info
combine_info <- function(df, info) {
  # Merge the data frames
  merged_df <- merge(df, info, by = c("DataID", "Date", "medianSPC"), all.x = TRUE)
  return(merged_df)
}

# Apply the function to the list of data frames
csv_clean <- lapply(csv_clean, function(df) {
  tryCatch({
    combine_info(df, info = result_df)
  }, error = function(e) {
    cat("Error in merging data frame:\n")
    print(e)
    return(df)
  })
})

#### Plot curves with flags ####
# Loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # Access the current data frame
  df <- csv_clean[[i]]
  
  # Extract values from the data frame
  flag_note <- df$flag_notes[1]
  median_spc <- df$medianSPC[1]
  final_spc <- df$final_SPC
  background_value <- df$background[1]
  Q <- df$Q[1]
  
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + 
    ggtitle(paste(Saltslugs_csvs$name[i])) +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.), 
             label = flag_note, color = "#CD2626", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.1 * max(df$SPC.uS.cm.), 
             label = paste(median_spc, "median SPC"), color = "#00688B", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.15 * max(df$SPC.uS.cm.), 
             label = paste(final_spc, "final SPC"), color = "plum", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.2 * max(df$SPC.uS.cm.), 
             label = paste(background_value, "bckg"), color = "#008B00", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.25 * max(df$SPC.uS.cm.), 
             label = paste(Q, "L/sec"), color = "goldenrod2", size = 4)
  # Display the plot in the plot panel
  print(p)
  
  # Save the plot as a PNG file
  ggsave(paste0("saltslug_figs/", Saltslugs_csvs$name[i], ".png"), plot = p)
}

#### Save images to Google Drive ####
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "saltslug_figs/"
drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1RgQa4fUOawnoSTg-Rl0ADMUytqKG4cQo"

# List all files in the local folder
files <- list.files(local_folder, full.names = TRUE)

# Upload each file to the specified Google Drive folder
lapply(files, function(file) {
  drive_upload(
    media = file,
    path = as_id(drive_folder_id)
  )
})

#### Save Q data frame to Google Drive ####
## Load data from Google drive ##
# (Q <- drive_get("https://drive.google.com/drive/folders/1UkRaYRBePgY9XU90_3DvURNGGEWbCew0"))

# List all files in the folder
# Q_files <- drive_ls(Q)

# Download the most recent CSV file
# drive_download(as_id(Q_files$id), path = "field_data/2025_SCL_salvagesites.csv", overwrite = TRUE)

# Fetch the file
# Q <- read.csv("field_data/Q.csv")

# Convert the injection DateTime column to POSIXct
# Q$Date <- as.Date(Q$Date, format = "%m/%d/%y")

# Combine by row
# Q <- rbind(Q, mergedQ)

# Save new data frame
write.csv(mergedQ, "field_data/2025_SCL_salvagesites.csv", row.names=FALSE, quote=FALSE) 

# Define the local folder path and the target folder ID in Google Drive
file <- "field_data/2025_SCL_salvagesites.csv"
drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1nZFdNvsjzwQu4iMm_rNK2Xn6ZuI2Pvrf"

# Upload file to the specified Google Drive folder
drive_upload(
  media = file,
  path = as_id(drive_folder_id)
)

#### Save edited slugs to Google Drive ####
# Loop through each data frame in the list
for (i in seq_along(csvs)) {
  # Access the current data frame
  df <- csvs[[i]]
  
  # Save new data frame
  write.csv(df, paste0("slugs/", Saltslugs_csvs$name[i]), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("slugs/", Saltslugs_csvs$name[i])
  drive_folder_id <- "https://drive.google.com/drive/u/0/folders/18UUIyROnoA0ksDRh08jcoMXjavroOh2n"
  
  # Upload file to the specified Google Drive folder
  drive_upload(
    media = file,
    path = as_id(drive_folder_id)
  )
}



## ---------------------------------- YSI ---------------------------------- ##

#### Libraries ####
library(googledrive) 

#### Local folders ####
# List and delete all files in these folders
files <- list.files(path = "slugs", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#### Load data ####
# Set up Google Drive folder
Saltslugs <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1-dLOmWMU7FvkYVfSL-8YBYHvMjRtFvY3")

# List and filter CSV files with "YSI" in their names
YSI_files <- googledrive::drive_ls(path = Saltslugs, type = "csv")
YSI_files <- YSI_files[grepl("YSI", YSI_files$name), ]

# Create an empty list to store the cleaned data frames
ysi_list <- lapply(seq_along(YSI_files$name), function(i) {
  googledrive::drive_download(
    file = YSI_files$id[i],
    path = paste0("googledrive/", YSI_files$name[i]),
    overwrite = TRUE
  )
  
  # Read the CSV file, skipping the first row (header is on row 2)
  read.csv(paste0("googledrive/", YSI_files$name[i]), sep = ",", header = TRUE)
})

# Assign names to the list elements based on the file names
names(ysi_list) <- YSI_files$name

# Check the contents of the list
str(ysi_list)

#### Format date/time ####
# Loop through each data frame in the list
for (i in seq_along(ysi_list)) {
  # Access the current data frame
  df <- ysi_list[[i]]
  
  # Make date into date format
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # Update the data frame in the list
  ysi_list[[i]] <- df
}

# Check the contents of the list
str(ysi_list)

#### Save edited slugs to Google Drive ####
# Loop through each data frame in the list
for (i in seq_along(ysi_list)) {
  # Access the current data frame
  df <- ysi_list[[i]]
  
  # Save new data frame
  write.csv(df, paste0("slugs/", YSI_files$name[i]), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("slugs/", YSI_files$name[i])
  # This is the inuse folder
  drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1grfiDNt1exWPntOd-JodQCsFzJzQQeV_"
  
  # Upload (overwrite) file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}

#### Libraries ####
library(changepoint) # Calculate changepoint in data
library(googledrive) 
library(googlesheets4)
library(tibble)

lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

#### Local folders ####
# List and delete all files in these folders
files <- list.files(path = "saltslug_figs", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "slugs", full.names = TRUE)
file.remove(files)

#### Load data from Google Drive ####
# This is the inuse folder
Saltslugs <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1QFG2-k094Bhb2WNCTb_Pjgfl_lA9c_rT")

# List all CSV files in the folder
Saltslugs_csvs <- googledrive::drive_ls(path = Saltslugs, type = "csv")

## Call all the files in the salt slugs folder ##
# Create empty list to store data frames
csv_list <- list()

# Loop over each file in the `Saltslugs_csvs` data frame
for (i in seq_along(Saltslugs_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", Saltslugs_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = Saltslugs_csvs$id[i],
    path = local_path,
    overwrite = T
  )
  # Read the CSV file and add it to the list
  csv_list[[Saltslugs_csvs$name[i]]] <- read.csv(local_path)
}

# Check the contents of the list
str(csv_list)

#### Format date/time ####
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  # Combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # Convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # Update the data frame in the list
  csv_list[[i]] <- df
  
  # Make date into date fomat
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # Update the data frame in the list
  csv_list[[i]] <- df
}

# Check the contents of the list and make sure there are no NAs
str(csv_list)

#### Plot curves ####
# Visualize
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + ggtitle(paste(Saltslugs_csvs$name[i])) 
  # Display the plot in the plot panel
  print(p)
}

#### Calculate average background SpC ####
# Using the change point time in the curve, we are going to calculate the background SpC
# Loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # Access the current data frame
  df <- csv_list[[i]]
  
  # Remove rows with NA values
  df <- na.omit(df)
  
  # Get the change point from the SPC
  cpt <- cpt.mean(df$SPC.uS.cm., penalty='SIC', method='PELT', minseglen=2)
  # Get the change point
  # Get the change point index
  changepoint_index <- cpts(cpt)[1]
  
  # Extract the date-time at the change point index
  changepoint_datetime <- df$DateTime[changepoint_index]
  
  # Calculate the median SPC from the start to the change point
  df$medianSPC <- median(df$SPC.uS.cm.[1:changepoint_index], na.rm = TRUE)
  
  # Add the change point index as a new column to the data frame
  df$changepoint_index <- changepoint_index
  
  # Add the change point date-time as a new column to the data frame
  df$changepoint_datetime <- changepoint_datetime
  
  # Update the data frame in the list
  csv_list[[i]] <- df
}

#### Determine background conductivity ####
## Select area before or after the salt wave which is constant
## you want to calculate the average background conductivity from the measurement

# Load data frame with salt grams per site and injection time

## Load data from Google drive ##
(Q <- drive_get("https://docs.google.com/spreadsheets/d/1-1b1mEK1ILnhD0aY9oi_EMqSNgQA4qPJS4sUOJMMZeY/edit?gid=230509760#gid=230509760"))

# Download the file as a csv file
drive_download(as_id(Q$id), path = "2025_YSI_sites", type = "csv", overwrite = T)

# Fetch the file
salt <- read.csv("2025_YSI_sites.csv")

# Clean the column names (this removes spaces, special characters, etc.)
salt <- salt %>%
  janitor::clean_names()

# Rename columns and convert types
salt <- salt %>%
  # Rename columns
  dplyr::rename(
    DataID = site_code,
    Date = date,
    Time24h = time,
    reach = distance_from_injection_to_logger_m,
    salt = salt_weight_g,
    inj_time = injection_time_local,
    background = background_sp_c
  ) %>%
  # Convert
  mutate(
    reach = as.numeric(as.character(reach)),
    salt = as.numeric(as.character(salt)),
    background = as.numeric(as.character(background)),
    # Convert the injection DateTime column to POSIXct
    injection_time = as.POSIXct(paste(Date, inj_time, sep = " "), format = "%Y-%m-%d %H:%M"),
    # Change date format
    Date = as.Date(Date, format = "%Y-%m-%d"),
    # 1 g salt in 1 L of water gives cond = 2100 uS/cm
    Cond_mass = salt * 2100
  )

colnames(salt)

# Remove rows that I don't want
# salt <- salt[ , -(1:2)]

# Replace empties with NA
salt["notes"][salt["notes"] == ''] <- NA
salt["flag_notes"][salt["flag_notes"] == ''] <- NA

#### Combine salt info to csvs ####
# Function to combine info
combine_info <- function(df, info) {
  df <- df %>%
    mutate(Date = as.Date(Date))
  info <- info %>%
    mutate(Date = as.Date(Date))
  
  merged_df <- df %>%
    left_join(info, by = c("DataID", "Date"))
  
  return(merged_df)
}

# Function to combine info
combine_info <- function(df, info) {
  # Ensure that the columns 'DataID' and 'Date' are present in both data frames
  if (!all(c("DataID", "Date") %in% colnames(df))) {
    stop("Columns 'DataID' and 'Date' not found in the data frame.")
  }
  if (!all(c("DataID", "Date") %in% colnames(info))) {
    stop("Columns 'DataID' and 'Date' not found in the info data frame.")
  }
  
  # Convert 'Date' columns to Date type if they are not already
  if (!inherits(df$Date, "Date")) {
    df$Date <- as.Date(df$Date)
  }
  if (!inherits(info$Date, "Date")) {
    info$Date <- as.Date(info$Date)
  }
  # Merge the data frames
  merged_df <- merge(df, info, by = c("DataID", "Date"), all.x = TRUE)
  return(merged_df)
}

# Apply the function to the list of data frames
csvs <- lapply(csv_list, function(df) {
  tryCatch({
    combine_info(df, info = salt)
  }, error = function(e) {
    cat("Error in merging data frame:\n")
    print(e)
    return(df)
  })
})

#### Visualize where the cut is to start measuring ####
# Create line color vector
legend_colors <- c("changepoint" = "red", "injection" = "blue")

# Loop through each data frame in the list
for (i in seq_along(csvs)) { 
  # Access the current data frame
  df <- csvs[[i]]
  
  # Create a data frame for the vertical lines
  vlines <- data.frame(
    xintercept = c(df$changepoint_datetime[1], df$inj_time[1]),
    type = c("changepoint", "injection")
  )
  
  # Create the plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + 
    ggtitle(Saltslugs_csvs$name[i]) +
    geom_vline(data = vlines, aes(xintercept = xintercept, color = type, linetype = type)) +
    scale_color_manual(values = legend_colors) +
    scale_linetype_manual(values = c("changepoint" = "dashed", "injection" = "dashed")) +
    labs(color = "colors", linetype = "colors")
  # Save the plot as a PNG file
  ggsave(paste0("saltslug_figs/", Saltslugs_csvs$name[i], ".png"), plot = p)
  # Display the plot in the plot panel
  print(p)
}

#### Find lowest point after the peak ####
# Initialize a new data frame to store results
result_df <- data.frame(DataID = character(), medianSPC = numeric(), final_SPC = numeric(), Date = as.Date(character()), final_index = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(csvs)) {
  # Access the current data frame
  df <- csvs[[i]]
  
  # Calculate condcorr and peak index
  condcorr <- df$SPC.uS.cm. - df$medianSPC[1]
  
  # Find the peak of the slug
  peak_index <- which.max(condcorr)
  
  # Find the final index (SPC closest to zero after the peak)
  condcorr_after_peak <- condcorr[(peak_index + 1):length(condcorr)]
  closest_to_zero_index_after_peak <- which.min(abs(condcorr_after_peak))
  final_index <- peak_index + closest_to_zero_index_after_peak
  
  # Get the SPC value at final_index
  final_SPC <- df$SPC.uS.cm.[final_index]
  
  # Add to the results data frame
  result_df <- rbind(result_df, data.frame(DataID = df$DataID[1], Date = df$Date[1], medianSPC = df$medianSPC[1], final_SPC = final_SPC, final_index = final_index))
}

#### Calculate difference in tail ####
result_df$SPC_difference = result_df$medianSPC - result_df$final_SPC

#### Remove curve tail ####
# Create empty list to store cleaned data frames
csv_clean <- list()

for (i in seq_along(csvs)) {
  # Access the current data frame
  df <- csvs[[i]]
  
  # Perform cleaning or processing (e.g., trimming rows)
  df <- df[1:result_df$final_index[i], ]
  
  # Use the corresponding name from Saltslugs_csvs as the list name
  name <- gsub(".csv", "", Saltslugs_csvs$name[i])  # Strip the .csv extension
  csv_clean[[name]] <- df  # Assign the cleaned data frame with the corresponding name
}

#### Plot curves after removing tail ####
# Visualize
# Loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # Access the current data frame
  df <- csv_clean[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + ggtitle(paste(Saltslugs_csvs$name[i])) +
    geom_point() + ggtitle(paste(Saltslugs_csvs$name[i])) 
  # Display the plot in the plot panel
  print(p)
}

#### Estimate Q ####
## Calculate Q!
## Units = L/sec
#example of how it looks for ONE salt slug
#Q_USF5 <- Qint(as.numeric(log_20240524_USF5$DateTime), log_20240524_USF5$SPC, USF5, USF5_Cond_mass)

# Create empty df to store Q data 
Q_list <- data.frame(DataID = c(), 
                     Q = c())

## Equation ##
Qint<-function(time, cond, bkg, condmass){
  condcorr<-cond-bkg
  
  ##below routine integrates
  ydiff<- condcorr[-1]+ condcorr[-length(condcorr)]
  condint<-sum(diff(time)*ydiff/2)
  
  Q<-condmass/condint
  Q
  
}

## Apply the equation to each salt slug in your folder ##
# Loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # Access the current data frame
  df <- csv_clean[[i]]
  # Indicate all variables and values for the equation 
  df$Q<- Qint(as.numeric(df$DateTime), df$SPC.uS.cm., df$medianSPC[[1]], df$Cond_mass[[1]])
  
  csv_clean[[i]] <-  df
}

#### Add Q values to table ####
# Specify the columns to select
selected_columns <- c("DataID", "Date", "salt", "background", "medianSPC", "Q", "flag", "flag_notes", "Time")

# Extract the first value from specified columns in each data frame
first_values <- lapply(csv_clean, function(df) {
  df[selected_columns] %>% slice(1)
})

# Combine these values into a new single data frame
combined_data_frame <- bind_rows(first_values)

#### Add SpC values to salt table ####
mergedQ <- combined_data_frame %>%
  left_join(result_df, by = c("DataID", "Date"))

# But remove the extra medianSPC column
mergedQ <- select(mergedQ, -medianSPC.y)

#### Combine SpC info to csvs ####
# Function to combine info
combine_info <- function(df, info) {
  # Merge the data frames
  merged_df <- merge(df, info, by = c("DataID", "Date", "medianSPC"), all.x = TRUE)
  return(merged_df)
}

# Apply the function to the list of data frames
csv_clean <- lapply(csv_clean, function(df) {
  tryCatch({
    combine_info(df, info = result_df)
  }, error = function(e) {
    cat("Error in merging data frame:\n")
    print(e)
    return(df)
  })
})

#### Plot curves with flags ####
# Loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # Access the current data frame
  df <- csv_clean[[i]]
  
  # Extract values from the data frame
  flag_note <- df$flag_notes[1]
  median_spc <- df$medianSPC[1]
  final_spc <- df$final_SPC
  background_value <- df$background[1]
  Q <- df$Q[1]
  
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + 
    ggtitle(paste(Saltslugs_csvs$name[i])) +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.), 
             label = flag_note, color = "#CD2626", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.1 * max(df$SPC.uS.cm.), 
             label = paste(median_spc, "median SPC"), color = "#00688B", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.15 * max(df$SPC.uS.cm.), 
             label = paste(final_spc, "final SPC"), color = "plum", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.2 * max(df$SPC.uS.cm.), 
             label = paste(background_value, "bckg"), color = "#008B00", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.25 * max(df$SPC.uS.cm.), 
             label = paste(Q, "L/sec"), color = "goldenrod2", size = 4)
  # Display the plot in the plot panel
  print(p)
  
  # Save the plot as a PNG file
  ggsave(paste0("saltslug_figs/", Saltslugs_csvs$name[i], ".png"), plot = p)
}

#### Save images to Google Drive ####
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "saltslug_figs/"
drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1qjt81r2lEVpm7Gx3XkfCRKPOGRdziXDl" # the "take3" folder
# List all files in the local folder
files <- list.files(local_folder, full.names = TRUE)

# Upload each file to the specified Google Drive folder
lapply(files, function(file) {
  drive_upload(
    media = file,
    path = as_id(drive_folder_id)
  )
})

#### Save Q data frame to Google Drive ####
## Load data from Google drive ##
# Q <- drive_get("https://drive.google.com/drive/u/0/folders/1RfXnvND0mM87fbOLpJr5g2LujhpaqAMo")

# List all files in the folder
# Q_files <- drive_ls(Q)

# Download the most recent CSV file
# drive_download(as_id(Q_files$id), path = "field_data/Q.csv", overwrite = TRUE)

# Fetch the file
# Q <- read.csv("2025_YSI_sites.csv")

# Convert the injection DateTime column to POSIXct
# Q$Date <- as.Date(Q$Date, format = "%m/%d/%y")

# Combine by row
# Q <- rbind(Q, mergedQ)

# Save new data frame 
write.csv(mergedQ, "field_data/2025_YSI_sites.csv", row.names=FALSE, quote=FALSE) 

# Define the local folder path and the target folder ID in Google Drive
file <- "field_data/2025_YSI_sites.csv"
drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1IB-Xh8i4ibv3WC2tsX59ElhDoeV2hqKY"

# Upload file to the specified Google Drive folder
drive_upload(
  media = file,
  path = as_id(drive_folder_id)
)

#### Save edited slugs to Google Drive ####
# Loop through each data frame in the list
for (i in seq_along(csvs)) {
  # Access the current data frame
  df <- csvs[[i]]
  
  # Save new data frame
  write.csv(df, paste0("slugs/", Saltslugs_csvs$name[i]), row.names=FALSE, quote=FALSE)
  
  # Define the local folder path and the target folder ID in Google Drive
  file <- paste0("slugs/", Saltslugs_csvs$name[i])
  drive_folder_id <- "https://drive.google.com/drive/u/0/folders/1aCtM0QyHqxP62LWRosyCh1cVXftjd42v"
  
  # Upload file to the specified Google Drive folder
  drive_upload(
    media = file,
    path = as_id(drive_folder_id)
  )
}
