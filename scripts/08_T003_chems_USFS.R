#### Read me ####
# The following code is for data compilation and plotting of USFS chem data for T003. 
# The files used include KRFinalDataSet excels (with chemistries post-lab) and field data from the Access database. 

library(dplyr)
library(tidyr)
library(ggplot2)
library(googledrive) # Make sure Google Drive authentication is updated, otherwise the connection won't work.
library(openxlsx)
library(lubridate)
library(hms)


# Get folder ID from URL if needed
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









# Get folder ID from URL if needed
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

# Combine all data frames
field_chems <- bind_rows(data_list)

#filter for just data from T003
field_chems <- field_chems %>%
  filter(Site == "T003") %>%
  select(SampleID, Site, SampleType, Date, Time, Volume, pH, EC, Temp)


#left join the lab data to the field chem data for T003
chems$SampleID <- chems$Sample
joined_chems <- left_join(field_chems, chems, by = "SampleID")


# Convert Time to proper time format
joined_chems$Time <- hms::parse_hm(joined_chems$Time)

# Combine Date + Time
joined_chems$Date_time <- as.POSIXct(joined_chems$Date) + as.numeric(joined_chems$Time)

# Set timezone
attr(joined_chems$Date_time, "tzone") <- "America/Los_Angeles"

joined_chems <- joined_chems %>%
  select(-c(Site, SampleType, Date, Time, Volume, Sample, Field.ID, X2))

###Join long and pivot!

#lets pivot longer 
df_long <- joined_chems %>%
  pivot_longer(
    cols = -Date_time,
    names_to = "variable",
    values_to = "value"
  ) %>%
  arrange(Date_time)

df_long$value <- as.numeric(df_long$value)

chems_faceted <- ggplot(df_long, aes(x = Date_time, y = value))+
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




df_temp <- df_long %>%
  filter(variable == "Temp") %>%
  arrange(Date_time)

ggplot(df_temp, aes(x = Date_time, y = value))+
  geom_point()+
  geom_line()+
  theme_minimal()


#We can finish by deleting the folders of excels before pushing to Github
unlink("Excels", recursive = TRUE, force = TRUE)
unlink("Field_Chems", recursive = TRUE, force = TRUE)

#also that png I made
png_files <- list.files(pattern = "\\.png$")  # all PNGs in working directory
file.remove(png_files)

