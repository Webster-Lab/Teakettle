##===================================================================
  ## Project: TEAKETTLE
  ## Script to format all the Pressure Transducer (Level Logger) to a cleaner state and upload them back to Drive
  ## press Command+Option+O to collapse all sections and get an overview of the workflow!
  ##==============================================================================

##############
## Packages ##
##############
library(googledrive) 
library(tidyverse)

## Part 1: Merging and Timestamps

###################################
## Clear folders that we will use ##
###################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import Data ####
#####################
# set up Google Drive folder
pt <- googledrive::as_id("1_f7I40tZq0trjxeIqOctxAzBfXi3Wg6j")

# list and filter CSV files with "pt" in their names
pt_files <- googledrive::drive_ls(path = pt, type = "csv")

pt_files <- pt_files[!grepl("Baro", pt_files$name), ]




pt_list <- lapply(seq_along(pt_files$name), function(i) {
  path <- file.path("googledrive", pt_files$name[i])
  
  googledrive::drive_download(file = pt_files$id[i], path = path, overwrite = TRUE)
  
  # find the header row: the first line containing "Date"
  lines <- readLines(path, n = 40, warn = FALSE)
  hdr_row <- grep("Date", lines)[1]
  message(pt_files$name[i], ": header on row ", hdr_row)
  
  df <- read_csv(path, skip = hdr_row - 1, show_col_types = FALSE)
  df <- df |> select(-matches("^\\.\\.\\.\\d+$"))   # drop stray trailing-comma columns
  df$source_file <- pt_files$name[i]                # handy for tracing later
  df
})



# assign names to the list elements based on the file names
names(pt_list) <- pt_files$name

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # Access the current data frame
  df <- pt_list[[i]]
  
  # make date into date format
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # update the data frame in the list
  pt_list[[i]] <- df
}

##########################################################
#### Combine and format Date and Time into one column #### 
##########################################################
# loop through each data frame in the list
for (i in seq_along(pt_list)) {
  # access the current data frame
  df <- pt_list[[i]]
  # combine Date and Time columns into a new DateTime column
  df$DateTime <- paste(df$Date, df$Time, sep = " ")
  
  # convert the DateTime column to POSIXct.  We're using PST 
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")
  df$DateTime <- round_date(df$DateTime, "15 minutes")
  # update the data frame in the list
  pt_list[[i]] <- df
  
  # update the data frame in the list
  pt_list[[i]] <- df
}

####################################
#### Combine data for each site ####
####################################
# site names
site_names <- c("TEAK01", "Outlet")
# group files in `pt_list` by matching `site_names` in file names
pt_list_by_site <- lapply(site_names, function(site) {
  # names(pt_list) gives the names of all files in pt_list
  site_files <- names(pt_list)[grepl(site, names(pt_list))] 
  # grep checks if the current site (e.g., DVSB1) appears in each file name in pt_list 
  # this returns a logical vector (TRUE for matches, FALSE otherwise).
  pt_list[site_files] # select only the files for this site
  # the [ ] indexing selects only the file names where the match is TRUE.
})

# name the list by site
names(pt_list_by_site) <- site_names

# combine data for each site
combined_by_site <- lapply(pt_list_by_site, function(site_data_list) {
  # bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # ensure chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

### Ok for some reason, from October to May, the pressure is in KPa so I need to convert that.

cutoff <- as.POSIXct("2026-05-19 10:45:00", tz = "Etc/GMT+8")

combined_by_site <- lapply(combined_by_site, function(df) {
  df %>%
    mutate(LEVEL = case_when(
      DateTime < cutoff ~ LEVEL * 0.101972,   # before cutoff: convert kPa to m
      TRUE              ~ LEVEL               # after cutoff: leave as is
    ))
})

###########################################
#### Save combined files back to Drive ####
###########################################
# write files to local data folder
lapply(names(combined_by_site), function(site) {
  # define file path
  file <- paste0("", site, ".csv")
  # save each data frame
  write.csv(combined_by_site[[site]], file, row.names = FALSE, quote = FALSE)
  # this is the "merged_days" folder
  drive_folder_id <- "1xeV94kM-TyYwhJ01kMSZxmseqe05eESp"
  # upload the file to Google Drive
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})






###############
### Now for the Air PT (these are HOBO logger files)

pt <- googledrive::as_id("https://drive.google.com/drive/folders/1_f7I40tZq0trjxeIqOctxAzBfXi3Wg6j")

# List and filter CSV files with "pt" in their names
pt_files <- googledrive::drive_ls(path = pt, type = "csv")
pt_files <- pt_files[grepl("Baro", pt_files$name), ]

# Create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files$name), function(i) {
  googledrive::drive_download(
    file = pt_files$id[i],
    path = paste0("googledrive/", pt_files$name[i]),
    overwrite = TRUE
  )
  
  read.csv(paste0("googledrive/", pt_files$name[i]), header = TRUE)
})


#Clean up files and edit names to match Solonist data
pt_list <- lapply(pt_list, function(df) {
  df %>%
    select(DateTime = 2, AIR_LEVEL = 3, AIR_TEMPERATURE = 4)
})

# Assign names to the list elements based on the file names
names(pt_list) <- pt_files$name


#### Format date column ####


library(lubridate)

# Peek at the raw strings (run before converting)
lapply(pt_list, function(df) head(df$DateTime, 3))

# Parser that handles 12-hour (AM/PM) and 24-hour strings in the same file
parse_hobo_dt <- function(x) {
  x <- trimws(as.character(x))
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "Etc/GMT+8")
  
  ampm <- grepl("AM|PM", x, ignore.case = TRUE)
  
  # rows with AM/PM -> 12-hour clock
  out[ampm] <- parse_date_time(
    x[ampm],
    orders = c("mdy IMS p", "mdy IM p", "Ymd IMS p", "Ymd IM p"),
    tz = "Etc/GMT+8"
  )
  
  # rows without AM/PM -> 24-hour clock or date-only
  out[!ampm] <- parse_date_time(
    x[!ampm],
    orders = c("Ymd HMS", "Ymd HM", "Ymd",
               "mdy HMS", "mdy HM", "mdy"),
    tz = "Etc/GMT+8"
  )
  out
}

pt_list <- lapply(pt_list, function(df) {
  df$DateTime        <- parse_hobo_dt(df$DateTime)
  df$AIR_LEVEL       <- as.numeric(df$AIR_LEVEL)
  df$AIR_TEMPERATURE <- as.numeric(df$AIR_TEMPERATURE)
  df
})


# combine the 3 files
Air_pt_combined <- bind_rows(pt_list) %>%
  distinct(DateTime, .keep_all = TRUE) %>%   # remove duplicate timestamps
  arrange(DateTime)                          # chronological order


# save locally
write.csv(Air_pt_combined, "BaroTEAK01.csv", row.names = FALSE, quote = FALSE)

# upload to Google Drive (only if you still want this)
library(googledrive)
drive_put(
  media = "BaroTEAK01.csv",
  path  = as_id("1xeV94kM-TyYwhJ01kMSZxmseqe05eESp")
)










#########################################################
#####Part 2: Compensating with Air PT data #########
#######################################################

air_data <- Air_pt_combined

#####################################
#### Change all units to meters and round to 15 min ####
#####################################
## air pressure in kPa to m
#1 kPa = 0.101972 m
air_data <- air_data %>%
  mutate(LEVEL_air.m = (AIR_LEVEL * 0.101972)) %>%
  mutate(DateTime = round_date(DateTime, "15 minutes")) %>%
  distinct(DateTime, .keep_all = TRUE)



#####################
#### Plot curves ####
#####################
  p <- ggplot(data = air_data, aes(x = DateTime, y = LEVEL_air.m)) + 
    geom_line() 
  # display the plot in the plot panel
  print(p)



######################################
#### Merging PT with Air pressure ####
######################################
  
  combined_with_air <- lapply(combined_by_site, function(site_df) {
    site_df %>%
      mutate(DateTime = round_date(DateTime, "15 minutes")) %>%
      left_join(air_data, by = "DateTime")
  })
  
  
  
  
  
  
########################################
#### Manual Barometric Compensation ####
########################################
## Once the units for each column are the same, subtract the barometric column from the Levelogger data 
# to get the true net water level recorded by the Levelogger.

  combined_with_air <- lapply(combined_with_air, function(df) {
    df %>%
      mutate(LEVEL_comp.m = LEVEL - LEVEL_air.m)
  })

  
  
  all_sites <- bind_rows(combined_with_air, .id = "Site")
  
  # 1. Compensated water level (the final product)
 p<- ggplot(subset(all_sites, LEVEL_comp.m > 0), aes(DateTime, LEVEL_comp.m)) +
    geom_line() +
    facet_wrap(~ Site, ncol = 1, scales = "free_y") +
    labs(
      y = "Compensated level (m)",
      title = "Compensated water level"
    )+
    scale_x_datetime(
      date_breaks = "2 weeks",
      date_minor_breaks = "1 day",
      date_labels = "%m/%d",
      expand = c(0.01, 0)          # trims empty space at the edges
    ) +
    theme_minimal()
  
  ggsave("PT.png", p, width = 16, height = 8, dpi = 300)
  
  
#########################################
#### Save compensated files to Drive ####
#########################################
# loop through each data frame in the list
for (i in seq_along(compensated_list)) {
  # access the current data frame
  df <- compensated_list[[i]]
  
  # save new data frame
  write.csv(df, paste0("data/", names(compensated_list)[i]), row.names=FALSE, quote=FALSE)
  
  # define the local folder path and the target folder ID in Google Drive
  file <- paste0("data/", names(compensated_list)[i])
  # this is the "compensated" folder
  drive_folder_id <- "1SAtC_CJd6KC2yWtJB_VdTebMBDSjy-Pk"
  
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
}


# See what would be deleted first
csvs <- list.files(pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
csvs

# Delete them
file.remove(csvs)


