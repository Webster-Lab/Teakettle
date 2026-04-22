#### Read me ####
# The following code is for data compilation and plotting of USFS chem data for T003. 
# The files used include KRFinalDataSet excels (with chemistries post-lab) and field data from the Access database. 

#### Load libraries ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(googledrive) # Make sure Google Drive authentication is updated, otherwise the connection won't work.
library(openxlsx)
library(lubridate)
library(hms)

#### Download and merge lab chem data ####
folder_id <- googledrive::as_id("https://drive.google.com/drive/folders/1suit0uVMgR5Rq2RM8VWa0tfDaBpPMFHC")

# List all files in the folder
files <- drive_ls(path = folder_id, pattern = "\\.xlsx?$")

dir.create("Excels", showWarnings = FALSE, recursive = TRUE)

# Download to project folder "Excels"
for (file_name in files$name) {
  drive_download(
    file = file_name,
    path = file.path("Excels", file_name),
    overwrite = TRUE
  )
}

files <- list.files("Excels", pattern = "\\.xlsx$", full.names = TRUE)

data_list <- lapply(files, function(f) {
  df <- read.xlsx(f, sheet = 1)
  
  # Convert factors to characters (have to do this because some of the data is in letter codes but most is in numbers)
  df[] <- lapply(df, as.character)
  
  return(df)
})

# Combine all data frames
chems <- bind_rows(data_list)






#### Download and merge field chem data ####
folder_id <- googledrive::as_id("https://drive.google.com/drive/folders/14gJM-JhuzwA4-26NgC4h2-6yIZ4aQ-ou")

# List all files in the folder
files <- drive_ls(path = folder_id, pattern = "\\.xlsx?$")

dir.create("Field_Chems", showWarnings = FALSE, recursive = TRUE)

# Download to project folder "Field_Chems"
for (file_name in files$name) {
  drive_download(
    file = file_name,
    path = file.path("Field_Chems", file_name),
    overwrite = TRUE
  )
}

files <- list.files("Field_Chems", pattern = "\\.xls", full.names = TRUE)

data_list <- lapply(files, function(f) {
  df <- read.xlsx(f, sheet = 1)
  
  if (all(c("Date", "Time") %in% names(df))) {
    
    # Convert Excel numeric Date to Date
    if (is.numeric(df$Date)) {
      df$Date <- as.Date(df$Date, origin = "1899-12-30")
    }
    
    # Convert Time to character "HH:MM:SS"
    if (is.numeric(df$Time)) {
      # Excel numeric time = fraction of a day
      df$Time <- format(as.POSIXct(df$Time * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
    }
    
    # Combine Date and Time into POSIXct
    df$Date_time <- as.POSIXct(paste(df$Date, df$Time), format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")
  }
  if ("Notes" %in% names(df)) {
    df$Notes <- as.character(df$Notes)
  }
  
  return(df)
})

#Combine all data frames
field_chems <- bind_rows(data_list)

#filter for just data from T003
field_chems <- field_chems %>%
  filter(Site == "T003") %>%
  select(SampleID, Site, SampleType, Date, Time, Volume, pH, EC, Temp)

field_chems$pH <- as.numeric(field_chems$pH)
field_chems$EC <- as.numeric(field_chems$EC)
field_chems$Temp <- as.numeric(field_chems$Temp)
field_chems$SampleID <- as.numeric(field_chems$SampleID)




#### Join lab & field data, format date-time ####

#left join the lab data to the field chem data for T003
chems$SampleID <- as.numeric(chems$Sample)
joined_chems <- left_join(field_chems, chems, by = "SampleID")


# Convert Time to proper time format
joined_chems$Time <- hms::parse_hm(joined_chems$Time)

# Combine Date + Time
joined_chems$Date_time <- as.POSIXct(joined_chems$Date) + as.numeric(joined_chems$Time)

# Set timezone
attr(joined_chems$Date_time, "tzone") <- "Etc/GMT+8"

joined_chems <- joined_chems %>%
  select(-c(Site, SampleType, Date, Time, Volume, Sample, Field.ID, X2))

joined_chems$Date_Time_PT <- joined_chems$Date_time

#round to the closet 15 min

joined_chems$Date_Time_PT <- round_date(
  joined_chems$Date_Time_PT,
  unit = "15 minutes"
)

#OKAY TEMPORARY FIX: Make MRL and ND 0s.  This needs to be corrected long-term. 

chem_cols <- c("Alkalinity", "Chloride", "Nitrate", "Phosphate", "Sulfate", "Ammonium", "Calcium", "Magnesium", "Potassium", "Sodium")  # change to yours

joined_chems[chem_cols] <- lapply(joined_chems[chem_cols], function(x) {
  x[x %in% c("MRL", "ND")] <- 0
  as.numeric(x)
})

joined_chems <- joined_chems %>%
  select(-Date_time)

# Join long and pivot!

#lets pivot longer 
df_long <- joined_chems %>%
  pivot_longer(
    cols = c("pH", "EC", "Temp", "Alkalinity", "Chloride", "Nitrate", "Phosphate", "Sulfate", "Ammonium", "Calcium", "Magnesium", "Potassium", "Sodium"),
    names_to = "variable",
    values_to = "value"
  ) %>%
  arrange(Date_Time_PT)

df_long$value <- as.numeric(df_long$value)


#### Make faceted chem plot ####
chems_faceted <- ggplot(df_long, aes(x = Date_Time_PT, y = value))+
  geom_point()+
  geom_line()+
  facet_wrap(~ variable, scales = "free_y")+
  theme_minimal()
#yikes. too many facets

#upload this full plot to the google drive
ggsave(
  filename = "USFS_faceted_chems.png",
  plot = chems_faceted,
  width = 12,
  height = 10,
  dpi = 300)

drive_upload(
  media = "USFS_faceted_chems.png",
  path = as_id("1jX1_HWiE3c_qIc1pU8-f6MV7N-WAwUId")
)




#### Bring in ISCO data (15 min temperature and turbidity) ####

drive_find(n_max = 10) 

# Download the desired file to the working directory
#turbidity data 2006-2025
drive_download("Compiled_turb_data.csv", path = "Compiled_turb_data.csv", overwrite = TRUE)
turb <- read.csv("Compiled_turb_data.csv")

#temp data 2006-2025
drive_download("Compiled_temp_data.csv", path = "Compiled_temp_data.csv", overwrite = TRUE)
temp <- read.csv("Compiled_temp_data.csv")

#format date/time.  These times are in Pacific time but do not shift for daylights savings, so timezone is ETC/GMT+8
turb$Date_Time_PT <- as.POSIXct(turb$Date_Time, format="%m/%d/%Y %H:%M", tz="Etc/GMT+8")
temp$Date_Time_PT <- as.POSIXct(temp$Date_Time, format="%m/%d/%Y %H:%M", tz="Etc/GMT+8")

#merge temp and turb

temp_turb <- full_join(temp, turb, by = "Date_Time_PT")

temp_turb <- temp_turb %>%
  select(Date_Time_PT, Temperature, Turbidity_NTU)

#### Bring in discharge data ####

drive_download("t003.2003.2025.discharge.csv", path = "t003.2003.2025.discharge.csv", overwrite = TRUE)
discharge <- read.csv("t003.2003.2025.discharge.csv")

#this one I had converted to "America/Los Angeles" time so that it did switch for daylights savings
discharge$Date_Time_PT <- ymd_hms(
  discharge$Date_time_PT,
  tz = "America/Los_Angeles"
)

#I guess we'll switch it back to match the ISCO data.  Should really get consistent with this. 
discharge$Date_Time_PT <- with_tz(
  discharge$Date_Time_PT,
  "Etc/GMT+8"
)

discharge <- discharge %>%
  select(Date_Time_PT, T003_lps)

#### Merge everything all together & upload data ####
merged <- left_join(discharge, temp_turb, by = "Date_Time_PT")
merged <- left_join(merged, joined_chems, by = "Date_Time_PT")

merged <- merged %>%
  select( -SampleID)


#lets save this dataset so we have a version on the googledrive
write.csv(merged, "USFS_chem_discharge_data_sortaclean.csv", row.names = FALSE)

drive_upload(
  media = "USFS_chem_discharge_data_sortaclean.csv",
  path = as_id("1yp3OUqcn7_jnWTgrRLorTvlw-DiJuooC"),
  name = "USFS_chem_discharge_data_sortaclean.csv",
  overwrite = TRUE
)


#make long for plotting
merged_long <- merged %>%
  pivot_longer(cols = c(T003_lps, Temperature, Turbidity_NTU, pH, EC, Temp, Alkalinity, Chloride, Nitrate, Phosphate, Sulfate, Ammonium, Calcium, Potassium, Sodium, Magnesium),
               names_to = "variable", values_to = "value")

#### Make chemistry time-series plots ####

#Make time series plots for each different variable vs. discharge

vars_to_plot <- setdiff(unique(merged_long$variable), "T003_lps")

drive_folder <- as_id("1UboFvbCMv10VA8p3KuHIgIGGG4v_s-DW")

for (v in vars_to_plot) {
  
  df_sub <- merged_long %>%
    filter(variable %in% c("T003_lps", v)) %>%
    group_by(Date_Time_PT, variable) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  
  df_sub <- df_sub %>%
    arrange(Date_Time_PT)
  
    p <- ggplot(df_sub, aes(Date_Time_PT, value)) +
    
    geom_line(data = df_sub %>% filter(variable == "T003_lps"),
              color = "blue3", linewidth = 0.6) +
    
      #force lines to link different grab samples that have NA values between
      geom_line(
        data = df_sub %>% filter(variable == v & !is.na(value)),
        color = "firebrick",
        linewidth = 0.3
      ) + 
    
    geom_point(data = df_sub %>% filter(variable == v),
               color = "firebrick", size = 0.6, alpha = 0.6, na.rm = TRUE) +
    
      #free the y axes and make a single column of stacked figs
    facet_wrap(~variable, scales = "free_y", ncol = 1) +
    
    labs(
      title = paste("T003 Discharge vs", v),
      x = "Time",
      y = "Value",
      color = "Variable"
    ) +
      
      scale_x_datetime(
        date_breaks = "1 year",
        date_labels = "%Y"
      )+
    
    theme_minimal()
  
  # save locally
  file_name <- paste0("T003_discharge_vs_", v, ".png")
  temp_file <- file.path(tempdir(), file_name)
  
  ggsave(temp_file, plot = p, width = 15, height = 6)
  
  # upload to Google Drive
  drive_upload(
    media = temp_file,
    path = drive_folder,
    name = file_name,
    overwrite = TRUE
  )
}





#### Clean up ####

#We can finish by deleting the folders of excels before pushing to Github
unlink("Excels", recursive = TRUE, force = TRUE)
unlink("Field_Chems", recursive = TRUE, force = TRUE)

#also that png I made
png_files <- list.files(pattern = "\\.png$")  # all PNGs in working directory
file.remove(png_files)

csv_files <- list.files(pattern = "\\.csv$")
file.remove(csv_files)
