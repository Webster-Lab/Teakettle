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
library(dplyr)
library(ggplot2)
library(patchwork)

#########################################
##### Part 1: File Merging and Timestamps ####
#########################################


###################################
## Clear folders that we will use ##
###################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import PT Data ####
#####################
# set up Google Drive folder
pt <- googledrive::as_id("1_f7I40tZq0trjxeIqOctxAzBfXi3Wg6j")

# list and filter CSV files with "pt" in their names
pt_files <- googledrive::drive_ls(path = pt, type = "csv")

pt_files <- pt_files[!grepl("Baro", pt_files$name), ]




pt_list <- lapply(seq_along(pt_files$name), function(i) {
  path <- file.path("googledrive", pt_files$name[i])
  
  googledrive::drive_download(file = pt_files$id[i], path = path, overwrite = TRUE)
  
  # find the header row: the first line containing "Date".  This varies for different data files
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
  
  # convert the DateTime column to POSIXct.  We're using PST -- easier for handling daylights savings time
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+7")
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

### Ok for some reason, from October to May, the pressure is in KPa so I need to convert that to meters for consistency

cutoff <- as.POSIXct("2026-05-19 10:45:00", tz = "Etc/GMT+7")

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






#####################################################
### Now for the Air PT (these are HOBO logger files)
######################################################

#define folder
pt <- googledrive::as_id("https://drive.google.com/drive/folders/1_f7I40tZq0trjxeIqOctxAzBfXi3Wg6j")

# List and filter CSV files with "Baro" in their names so we know they are air PTs
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


#Clean up files and edit names
pt_list <- lapply(pt_list, function(df) {
  df %>%
    select(DateTime = 2, AIR_LEVEL = 3, AIR_TEMPERATURE = 4)
})

# Assign names to the list elements based on the file names
names(pt_list) <- pt_files$name


#### Format datetime column.  We also want these in ETC GMT+7 to match ####
library(lubridate)

# Parser that handles 12-hour (AM/PM) and 24-hour strings in the same file.  Why we have so many different formats, I will never know
parse_hobo_dt <- function(x) {
  x <- trimws(as.character(x))
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "Etc/GMT+7")
  
  ampm <- grepl("AM|PM", x, ignore.case = TRUE)
  
  # rows with AM/PM -> 12-hour clock
  out[ampm] <- parse_date_time(
    x[ampm],
    orders = c("mdy IMS p", "mdy IM p", "Ymd IMS p", "Ymd IM p"),
    tz = "Etc/GMT+7"
  )
  
  # rows without AM/PM -> 24-hour clock or date-only
  out[!ampm] <- parse_date_time(
    x[!ampm],
    orders = c("Ymd HMS", "Ymd HM", "Ymd",
               "mdy HMS", "mdy HM", "mdy"),
    tz = "Etc/GMT+7"
  )
  out
}

pt_list <- lapply(pt_list, function(df) {
  df$DateTime        <- parse_hobo_dt(df$DateTime)
  df$AIR_LEVEL       <- as.numeric(df$AIR_LEVEL)
  df$AIR_TEMPERATURE <- as.numeric(df$AIR_TEMPERATURE)
  df
})


# combine the 3 baro files
Air_pt_combined <- bind_rows(pt_list) %>%
  distinct(DateTime, .keep_all = TRUE) %>%   # remove duplicate timestamps
  arrange(DateTime)                          # chronological order


# save locally
write.csv(Air_pt_combined, "BaroTEAK01.csv", row.names = FALSE, quote = FALSE)

# upload the combined file to Google Drive
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
#### Change all units to meters and round to every 15 min ####
#####################################
## air pressure in kPa to m
#1 kPa = 0.101972 m
air_data <- air_data %>%
  mutate(LEVEL_air.m = (AIR_LEVEL * 0.101972)) %>%
  mutate(DateTime = round_date(DateTime, "15 minutes")) %>%
  distinct(DateTime, .keep_all = TRUE)



######################################
#### Merging PT with Air pressure ####
######################################
  
#again, we want this rounded to every 15 minutes

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
  


#####################################
#### Plot it up! #####################
######################################


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
  for (i in seq_along(combined_with_air)) {
    # access the current data frame
    df <- combined_with_air[[i]]
    
    # save new data frame
    write.csv(df, paste0("", names(combined_with_air)[i]), row.names=FALSE, quote=FALSE)
    
    # define the local folder path and the target folder ID in Google Drive
    file <- paste0("", names(combined_with_air)[i])
    # this is the "compensated" folder
    drive_folder_id <- "1SGEuQCL4Men6CX1L3rttQdkK_XFDvasS"
    
    # upload file to the specified Google Drive folder
    drive_put(
      media = file,
      path = as_id(drive_folder_id)
    )
  }
  

  
  ################################################
  ######## Fix Sensor Offets (moving PT) #########
  ###############################################
  
  
  #### Manuela's reusable sensor-move offset fn ####
  
  apply_offset <- function(df, level_col, move_time, window_hours = 3) {
    window_secs <- window_hours * 3600
    
    before <- df %>%
      filter(DateTime >= move_time - window_secs, DateTime < move_time) %>%
      summarise(m = mean(.data[[level_col]], na.rm = TRUE)) %>% pull(m)
    
    after <- df %>%
      filter(DateTime >= move_time, DateTime < move_time + window_secs) %>%
      summarise(m = mean(.data[[level_col]], na.rm = TRUE)) %>% pull(m)
    
    offset <- after - before
    if (is.na(offset)) stop("No data in the before/after window at ", format(move_time))
    
    df[[level_col]] <- if_else(df$DateTime >= move_time,
                               df[[level_col]] - offset,
                               df[[level_col]])
    df
  }
  

  
  
  #TEAK01
  
  TEAK01_out_of_water_points <- as.POSIXct(c(
    "2026-05-19 10:30:00",
    "2026-05-19 10:45:00",
    "2026-05-19 11:00:00",
    "2026-05-19 11:15:00",
    
    "2026-05-25 09:45:00",
    "2026-05-25 10:00:00",
    "2026-05-25 10:15:00",
    "2026-05-25 10:30:00" ), tz = "Etc/GMT+7")
  
  combined_with_air$TEAK01$LEVEL_comp.m[combined_with_air$TEAK01$DateTime %in% TEAK01_out_of_water_points] <- NA
  
  cut_start <- as.POSIXct("2025-10-26 16:00:00", tz = "Etc/GMT+7")
  
  combined_with_air$TEAK01$LEVEL_comp.m[
    combined_with_air$TEAK01$DateTime < cut_start
  ] <- NA
  
  combined_with_air$TEAK01$Final_Corrected_Lvl <- combined_with_air$TEAK01$LEVEL_comp.m
  
  move_time1 <- as.POSIXct("2026-05-19 10:30:00", tz = "Etc/GMT+7")
  move_time2 <- as.POSIXct("2026-05-25 09:45:00", tz = "Etc/GMT+7")
  
  combined_with_air$TEAK01 <- apply_offset(combined_with_air$TEAK01, "Final_Corrected_Lvl", move_time1)
  combined_with_air$TEAK01 <- apply_offset(combined_with_air$TEAK01, "Final_Corrected_Lvl", move_time2)
  
  ggplot(combined_with_air$TEAK01, aes(x = DateTime, y = Final_Corrected_Lvl)) +
    geom_line() +
    ggtitle(paste( "final corrected level"))


  
  #OUTLET
  
  
  OUT_out_of_water_points <- as.POSIXct(c(
    "2026-05-19 09:00:00",
    "2026-05-19 09:30:00",
    "2026-05-19 09:45:00",
    "2026-05-19 10:00:00",
    "2026-05-19 10:15:00",
    "2026-05-19 10:30:00",
    
    "2026-05-25 08:45:00",
    
    "2026-09-01 09:00:00"), tz = "Etc/GMT+7")
  
  combined_with_air$Outlet$LEVEL_comp.m[combined_with_air$Outlet$DateTime %in% OUT_out_of_water_points] <- NA
  
  cut_start <- as.POSIXct("2025-10-26 17:45:00", tz = "Etc/GMT+7")
  
  combined_with_air$Outlet$LEVEL_comp.m[
    combined_with_air$Outlet$DateTime < cut_start
  ] <- NA
  
  combined_with_air$Outlet$Final_Corrected_Lvl <- combined_with_air$Outlet$LEVEL_comp.m
  
  move_time3 <- as.POSIXct("2026-05-19 09:00:00", tz = "Etc/GMT+7")
  move_time4 <- as.POSIXct("2026-05-25 08:45:00", tz = "Etc/GMT+7")
  
  combined_with_air$Outlet <- apply_offset(combined_with_air$Outlet, "Final_Corrected_Lvl", move_time3)
  combined_with_air$Outlet <- apply_offset(combined_with_air$Outlet, "Final_Corrected_Lvl", move_time4)
  
  ggplot(combined_with_air$Outlet, aes(x = DateTime, y = Final_Corrected_Lvl)) +
    geom_line() +
    ggtitle(paste( "final corrected level"))
  
  
  
  
  
  
  
  
  
  ################################################
  ######### Merge PT data with field Q data ########
  ###############################################

  #Get Q data ready
  file <- drive_get("2026_Q")
  
  drive_download(
    file,
    path = "Q.csv",
    type = "csv",
    overwrite = TRUE
  )
  
  Q <- read.csv ("Q.csv")
  
  #Pull out bad slugs, filter for the sites we want, and format DateTime
  Q_PT_sites <- Q %>%
    filter(DataID %in% c("TEAK01", "OUT")) %>%
    filter(flag != "not good") %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      DateTime = parse_date_time(
        paste(Date, Time),
        orders = c("Y-m-d H:M:S", "Y-m-d H:M")
      )
    ) %>%
    filter(Date > as.Date("2025-10-01"))

  
  #convert from L/s to m3/s
  Q_PT_sites$Q_m3s <- as.numeric(Q_PT_sites$Q) / 1000
  
  
  #Merge it together with PT data
  
  # Rename the list element (Outlet -> OUT)
  names(combined_with_air)[names(combined_with_air) == "Outlet"] <- "OUT"
  
  # Stack, using the list names as DataID
  loggers <- bind_rows(combined_with_air, .id = "DataID") %>%
    select(-Date, -Time, -ms)   # avoid clashing with injections' Date/Time
  
  # Match DateTime type/timezone in both
  tz_use <- "America/Denver"   # change to your logger timezone
  loggers    <- loggers    %>% mutate(DateTime = force_tz(as.POSIXct(DateTime), tz_use))
  Q_PT_sites <- Q_PT_sites %>% mutate(DateTime = force_tz(as.POSIXct(DateTime), tz_use))
  
  # Join
  joined <- Q_PT_sites %>%
    left_join(loggers, by = c("DataID", "DateTime"))
  
  
  
  #How does it look?


  
  tz_use  <- "Etc/GMT+8"   # same as before
  t_start <- as.POSIXct("2026-05-01 00:00:00", tz = tz_use)
  t_end   <- as.POSIXct("2026-10-01 00:00:00", tz = tz_use)
  
  plot_site <- function(site) {
    lv <- loggers    %>% filter(DataID == site, LEVEL_comp.m > 0,
                                DateTime >= t_start, DateTime < t_end)
    q  <- Q_PT_sites %>% filter(DataID == site, Q_m3s > 0,
                                DateTime >= t_start, DateTime < t_end)
    
    k <- max(lv$LEVEL_comp.m) / max(q$Q_m3s)
    
    ggplot() +
      geom_line(data = lv, aes(DateTime, LEVEL_comp.m), color = "steelblue") +
      geom_point(data = q,  aes(DateTime, Q_m3s * k), color = "firebrick", size = 2.5) +
      scale_y_continuous(
        name = "LEVEL_comp (m)",
        sec.axis = sec_axis(~ . / k, name = "Q (m³/s)")
      ) +
      scale_x_datetime(limits = c(t_start, t_end), date_breaks = "1 month",
                       date_labels = "%b") +
      labs(title = site, x = NULL) +
      theme_bw() +
      theme(
        axis.title.y.left  = element_text(color = "steelblue"),
        axis.title.y.right = element_text(color = "firebrick")
      )
  }
  
  sites <- unique(Q_PT_sites$DataID)
  wrap_plots(lapply(sites, plot_site), ncol = 1)


#file.remove(list.files(pattern = "\\.png$", ignore.case = TRUE))
file.remove(list.files(pattern = "\\.csv$", ignore.case = TRUE))

#also clear out google drive downloads

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)


