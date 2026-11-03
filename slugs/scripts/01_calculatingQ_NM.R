##==============================================================================
## Project: Teakettle
## Script to estimate discharge using dilution gauging for multiple files at a time for the NM site
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

## Explanation based off: Moore, R.D. 2005. Salt Injection Using Salt in Solution.
## "Streamline Watershed Management Bulletin". Volume 8 (2)

## Two assumptions:
## (1) All the injected mass is recovered downstream
## (2) Tracer is completely mixed in the channel

## The time required for the peak of the wave to move past an observation point 
## depends inversely on the mean velocity of streamflow
## The duration of the salt wave depends on the amount of longitudinal dispersion,
## which depends on how variable velocities are across the stream

## at any time (t) while tracer is passing in the salt wave,
## the discharge of the tracer solution is: q(t) = Q*RC(t)
## Where Q is stream discharge (L/s), and RC(t) is the relative
## concentration of the tracer solution (L/L) at t

## Integrate over the salt wave to get discharge (Q):
## Q = V/integral(RC(t))dt

##################
#### Packages ####
##################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

library(changepoint) # calculate changepoint in data
library(googledrive) 
library(googlesheets4)
library(tibble)
library(googledrive)

#### Import & Visualize Data ####
#################################

# specify the local folder where your CSVs are stored
local_folder <- "in_use"  # adjust to your folder path

# list all CSV files in the folder
Saltslugs_csvs <- list.files(path = local_folder, pattern = "\\.csv$", full.names = TRUE)

# create an empty list to store the data frames
csv_list <- list()

# loop over each CSV file
for (file_path in Saltslugs_csvs) {
  # read the CSV file
  df <- read.csv(file_path, header = TRUE, check.names = FALSE)
  
  # use the file name as the list element name
  file_name <- basename(file_path)
  csv_list[[file_name]] <- df
}

# check structure
str(csv_list)






#### combine and format Date and Time into one column #### 
# loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # access the current data frame
  df <- csv_list[[i]]
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # convert the DateTime column to POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # update the data frame in the list
  csv_list[[i]] <- df
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  # update the data frame in the list
  csv_list[[i]] <- df
}

# check the contents of the list and make sure there are no NAs
str(csv_list)

### We want date to be POSIXct, and Cond and TempC to be numeric ###

#####################
#### Plot curves ####
#####################
# loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # access the current data frame
  df <- csv_list[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + ggtitle(paste(names(csv_list)[i]))
  # save the plot as a PNG file
  # ggsave(paste0("saltslug_figs/", Saltslugs_csvs$name[i], ".png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

###############################
#### Add DataID to loggers ####
###############################

# loggers do not have a column indicating which site the measurement was made on
# so for those instruments that don't have a "DataID" column, we will create one here
# function to extract the ID from the file name
extract_id <- function(file_name) {
  str_extract(file_name, "(TECR|TEAK|GRAN)\\d+")
}

# add the new DataID number
# get the file name from the list element names
for (i in seq_along(csv_list)) {
  
  df <- csv_list[[i]]
  file_name <- names(csv_list)[i]
  
  if (!"DataID" %in% names(df)) {
    extracted_id <- extract_id(file_name)
    df <- add_column(df, DataID = extracted_id, .before = 1)
  }
  
  csv_list[[i]] <- df
}


# but remove the extra data column for YSIs
for (i in seq_along(csv_list)) {
  df <- csv_list[[i]]
  # check if the column exists, then remove it
  if ("DataID.1" %in% colnames(df)) {
    df <- select(df, -DataID.1)
  }
  # update the data frame in the list
  csv_list[[i]] <- df
}

##########################################
#### Calculate average background SpC ####
##########################################
# using the change point time in the curve, we are going to calculate the background SpC
# loop through each data frame in the list
for (i in seq_along(csv_list)) {
  # access the current data frame
  df <- csv_list[[i]]
  
  # remove rows with NA values
  df <- na.omit(df)
  
  cat("Processing:", names(csv_list)[i], "\n")
  
  # get the change point from the SPC
  cpt <- cpt.mean(df$SPC.uS.cm., penalty='SIC', method='PELT', minseglen=2)
  # get the change point
  # get the change point index
  changepoint_index <- cpts(cpt)[1]
  
  # extract the date-time at the change point index
  changepoint_datetime <- df$DateTime[changepoint_index]
  
  # calculate the median SPC from the start to the change point
  df$medianSPC <- median(df$SPC.uS.cm.[1:changepoint_index], na.rm = TRUE)
  
  # add the change point index as a new column to the data frame
  df$changepoint_index <- changepoint_index
  
  # add the change point date-time as a new column to the data frame
  df$changepoint_datetime <- changepoint_datetime
  
  # update the data frame in the list
  csv_list[[i]] <- df
}

###############################################
#### Determine the background conductivity ####
###############################################
## select area before or after the salt wave which is constant
## you want to calculate the average background conductivity from the measurement

# load data frame with salt grams per site and injection time

#### load data from Google drive ####

# fetch the file
salt <- read.csv("salt_updated.csv")

# clean the column names (this removes spaces, special characters, etc.)
salt <- salt %>%
  janitor::clean_names()

# rename columns and convert types
salt <- salt %>%
  # rename columns
  dplyr::rename(
    DataID = data_id,
    Date = date,
    Time24h = time24h
  ) %>%
  # convert
  mutate(
    reach = as.numeric(as.character(reach)),
    salt = as.numeric(as.character(salt)),
    background = as.numeric(as.character(background)),
    # convert the injection DateTime column to POSIXct
    injection_time = as.POSIXct(paste(Date, Time24h, sep = " "), format = "%m/%d/%Y %H:%M:%S"),
    # change date format
    Date = as.Date(Date, format = "%m/%d/%Y"),
    # 1 g salt in 1 L of water gives cond = 2100 uS/cm
    Cond_mass = salt * 2100
  )

colnames(salt)

# remove rows that I don't want
#salt <- salt[ , -(1:2)]

# replace empties with NA
salt["notes"][salt["notes"] == ''] <- NA
salt["flag_notes"][salt["flag_notes"] == ''] <- NA

###################################
#### Combine salt info to csvs ####
###################################
# function to combine info
combine_info <- function(df, info) {
  df <- df %>%
    mutate(Date = as.Date(Date))
  info <- info %>%
    mutate(Date = as.Date(Date))
  
  merged_df <- df %>%
    left_join(info, by = c("DataID", "Date"))
  
  return(merged_df)
}

# function to combine info
combine_info <- function(df, info) {
  # ensure that the columns 'DataID' and 'Date' are present in both data frames
  if (!all(c("DataID", "Date") %in% colnames(df))) {
    stop("Columns 'DataID' and 'Date' not found in the data frame.")
  }
  if (!all(c("DataID", "Date") %in% colnames(info))) {
    stop("Columns 'DataID' and 'Date' not found in the info data frame.")
  }
  
  # convert 'Date' columns to Date type if they are not already
  if (!inherits(df$Date, "Date")) {
    df$Date <- as.Date(df$Date)
  }
  if (!inherits(info$Date, "Date")) {
    info$Date <- as.Date(info$Date)
  }
  # merge the data frames
  merged_df <- merge(df, info, by = c("DataID", "Date"), all.x = TRUE)
  return(merged_df)
}



# apply the function to the list of data frames
csvs <- lapply(csv_list, function(df) {
  tryCatch({
    combine_info(df, info = salt)
  }, error = function(e) {
    cat("Error in merging data frame:\n")
    print(e)
    return(df)
  })
})




#######################################################
#### Visualize where the cut is to start measuring ####
#######################################################
# create line color vector
legend_colors <- c("changepoint" = "red", "injection" = "blue")

# loop through each data frame in the list
for (i in seq_along(csvs)) { 
  # access the current data frame
  df <- csvs[[i]]
  
  # create a data frame for the vertical lines
  vlines <- data.frame(
    xintercept = c(df$changepoint_datetime[1], df$inj_time[1]),
    type = c("changepoint", "injection")
  )
  
  # create the plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + 
    ggtitle(paste(names(csv_list)[i]))+
    geom_vline(data = vlines, aes(xintercept = xintercept, color = type, linetype = type)) +
    scale_color_manual(values = legend_colors) +
    scale_linetype_manual(values = c("changepoint" = "dashed", "injection" = "dashed")) +
    labs(color = "colors", linetype = "colors")
  # save the plot as a PNG file
  ggsave(paste0("saltslug_figs/", csv_list$name[i], ".png"), plot = p)
  # display the plot in the plot panel
  print(p)
}

######################################
#### Find lowest point after peak ####
######################################
# initialize a new data frame to store results
result_df <- data.frame(DataID = character(), medianSPC = numeric(), final_SPC = numeric(), Date = as.Date(character()), final_index = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(csvs)) {
  # access the current data frame
  df <- csvs[[i]]
  
  # calculate condcorr and peak index
  condcorr <- df$SPC.uS.cm. - df$medianSPC[1]
  
  # find the peak of the slug
  peak_index <- which.max(condcorr)
  
  # find the final index (SPC closest to zero after the peak)
  condcorr_after_peak <- condcorr[(peak_index + 1):length(condcorr)]
  closest_to_zero_index_after_peak <- which.min(abs(condcorr_after_peak))
  final_index <- peak_index + closest_to_zero_index_after_peak
  
  # get the SPC value at final_index
  final_SPC <- df$SPC.uS.cm.[final_index]
  
  # add to the results data frame
  result_df <- rbind(result_df, data.frame(DataID = df$DataID[1], Date = df$Date[1], medianSPC = df$medianSPC[1], final_SPC = final_SPC, final_index = final_index))
}

######################################
#### Calculate difference in tail ####
######################################
result_df$SPC_difference = result_df$medianSPC - result_df$final_SPC

###########################
#### Remove curve tail ####
###########################
# create empty list to store cleaned data frames
csv_clean <- list()

for (i in seq_along(csvs)) {
  # access the current data frame
  df <- csvs[[i]]
  
  # perform cleaning or processing (e.g., trimming rows)
  df <- df[1:result_df$final_index[i], ]
  
  # use the corresponding name from Saltslugs_csvs as the list name
  name <- gsub(".csv", "", names(csv_list)[i]) # strip the .csv extension
  csv_clean[[name]] <- df  # assign the cleaned data frame with the corresponding name
}

#########################################
#### Plot curves after removing tail ####
#########################################
# visualize
# loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # access the current data frame
  df <- csv_clean[[i]]
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + ggtitle(paste(csv_list$name[i])) +
  geom_point() + ggtitle(paste(names(csv_list)[i])) 
  # display the plot in the plot panel
  print(p)
}

####################
#### Estimate Q ####
####################
## calculate Q!
## units = L/sec
#example of how it looks for ONE salt slug
#Q_USF5 <- Qint(as.numeric(log_20240524_USF5$DateTime), log_20240524_USF5$SPC, USF5, USF5_Cond_mass)

# create empty df to store Q data 
Q_list <- data.frame(DataID = c(), 
                     Q = c())

### equation ###
Qint<-function(time, cond, bkg, condmass){
  condcorr<-cond-bkg
  
  ##below routine integrates
  ydiff<- condcorr[-1]+ condcorr[-length(condcorr)]
  condint<-sum(diff(time)*ydiff/2)
  
  Q<-condmass/condint
  Q
  
}

### apply the equation to each salt slug in your folder ###
# loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # access the current data frame
  df <- csv_clean[[i]]
  # indicate all variables and values for the equation 
  df$Q<- Qint(as.numeric(df$DateTime), df$SPC.uS.cm., df$medianSPC[[1]], df$Cond_mass[[1]])
  
  csv_clean[[i]] <-  df
}

###############################
#### Add Q values to table ####
###############################
# specify the columns to select
selected_columns <- c("DataID", "Date", "salt", "background", "medianSPC", "Q", "flag_notes", "Time24h")

# extract the first value from specified columns in each data frame
first_values <- lapply(csv_clean, function(df) {
  df[selected_columns] %>% slice(1)
})

# combine these values into a new single data frame
combined_data_frame <- bind_rows(first_values)

######################################
#### Add SPC values to salt table ####
######################################
mergedQ <- combined_data_frame %>%
  left_join(result_df, by = c("DataID", "Date"))

# but remove the extra medianSPC column
mergedQ <- select(mergedQ, -medianSPC.y)

mergedQ <- mergedQ %>%
  dplyr::rename(medianSpC = medianSPC.x)



###################################
#### Combine SPC info to csvs ####
###################################
# function to combine info
combine_info <- function(df, info) {
  # merge the data frames
  merged_df <- merge(df, info, by = c("DataID", "Date", "medianSPC"), all.x = TRUE)
  return(merged_df)
}

# apply the function to the list of data frames
csv_clean <- lapply(csv_clean, function(df) {
  tryCatch({
    combine_info(df, info = result_df)
  }, error = function(e) {
    cat("Error in merging data frame:\n")
    print(e)
    return(df)
  })
})

################################
#### Plot curves with flags ####
################################
# loop through each data frame in the list
for (i in seq_along(csv_clean)) {
  # access the current data frame
  df <- csv_clean[[i]]
  
  # extract values from the data frame
  flag_note <- df$flag_notes[1]
  median_spc <- df$medianSPC[1]
  final_spc <- df$final_SPC
  background_value <- df$background[1]
  Q <- df$Q[1]
  
  # plot
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) + 
    geom_point() + 
    ggtitle(paste(names(csv_list)[i])) +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.), 
             label = flag_note, color = "#CD2626", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.1 * max(df$SPC.uS.cm.), 
             label = paste(median_spc, "median SPC"), color = "#00688B", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.15 * max(df$SPC.uS.cm.), 
             label = paste(final_spc, "final SPC"), color = "plum", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.2 * max(df$SPC.uS.cm.), 
             label = paste(background_value, "bckg"), color = "#008B00", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) - 0.25 * max(df$SPC.uS.cm.), 
             label = paste(round(Q, 2), "L/sec"), color = "goldenrod2", size = 4)
  # display the plot in the plot panel
  print(p)
  
  # save the plot as a PNG file
  ggsave(paste0("saltslug_figs/", names(csv_list)[i], "flags.png"), plot = p)
}


#Prep merged Q to merge with master Q dataset

#rename time column, add flag column and Campaign column
mergedQ <- mergedQ %>%
  #dplyr::rename(Time = Time24h)%>%
  mutate(flag = NA_character_)%>%
  mutate(Campaign = format(Date, "%B"))

#Add Catchment column 
mergedQ <- mergedQ %>%
  mutate(
    Catchment = case_when(
      startsWith(DataID, "OUT")  ~ "OUTLET",
      startsWith(DataID, "TECR") ~ "TECR",
      startsWith(DataID, "TEAK") ~ "TEAK",
      TRUE ~ NA_character_
    )
  )

#Add type column
mergedQ <- mergedQ %>%
  mutate(
    Type = case_when(
      DataID %in% c(
        "TEAK01","TEAK02","TEAK05","TEAK07","TEAK09","TEAK11","TEAK13","TEAK14",
        "TECR01","TECR02","TECR04","TECR09","TECR11","TECR13"
      ) ~ "Main",
      
      DataID %in% c(
        "TEAK03","TEAK03B","TEAK04","TEAK06","TEAK08","TEAK10","TEAK12",
        "TECR03","TECR05","TECR06","TECR07","TECR08","TECR10","TECR12","TECR12B"
      ) ~ "Trib",
      
      DataID == "OUT" ~ "Outlet",
  
      TRUE ~ NA_character_
    )
  )
  

####################################
#### Save Q data frame ####
####################################

# fetch the file
Q <- read.csv("Q.csv")

# convert the injection DateTime column to POSIXct
Q$Date <- as.Date(Q$Date, format = "%Y-%m-%d")

# combine by row
Q <- bind_rows(Q, mergedQ)

# save new data frame
write.csv(Q, "Q.csv", row.names=FALSE, quote=FALSE) 

####################################
#### Save edited slugs to Drive ####

### Once finished, move files manually from the "in_use" folder to the "edited" folder
####################################

