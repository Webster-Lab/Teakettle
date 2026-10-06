##==============================================================================
## Project: Teakettle
## Script to merge scan files in one (using timestamp)
##==============================================================================

##########################
#### Import scan data ####
##########################
#### List and download all files in the folder ####
# This is the "raw" folder
library(readxl)
library(googledrive)
library(dplyr)
library(purrr)
library(ggplot2)

scan <- as_id("1PUtXFdVj6HuWuDPI4EzuPBHAb6oRevwn")

# Make sure the local download folder exists
dir.create("googledrive", showWarnings = FALSE)

# List files, then drop subfolders and keep only .xlsx
scan_files <- drive_ls(path = scan) %>%
  filter(map_chr(drive_resource, "mimeType") != "application/vnd.google-apps.folder",
         grepl("\\.xlsx$", name, ignore.case = TRUE))

scan_list <- list()

for (i in seq_len(nrow(scan_files))) {
  local_path <- file.path("googledrive", scan_files$name[i])
  
  drive_download(file = scan_files$id[i], path = local_path, overwrite = TRUE)
  
  # Header is in row 2
  header <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
  col_names <- as.character(unlist(header[1, ]))
  blank <- is.na(col_names) | col_names == ""
  col_names[blank] <- paste0("X", which(blank))
  
  # Data starts at row 5
  data <- read_excel(local_path, skip = 4, col_names = col_names)
  data$source_file <- scan_files$name[i]   # keep track of which file each row came from
  
  scan_list[[scan_files$name[i]]] <- data
}

scan_all <- bind_rows(scan_list)


####################################
#### Combine data for each site ####
####################################
# Loop through each data frame in the list to change DateTime column name
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Change names for easier handling
  colnames(df)[1] ="DateTime"
  
  # Update the data frame in the list
  scan_list[[i]] <- df
}

scan_all$DateTime <- scan_all$`Parameter:`

p <- ggplot(data = scan_all, aes(x = DateTime, y = `DOCeq [mg/l] - Measured value`)) + 
  geom_line() + 
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%m/%d") +
  theme(axis.text.x = element_text(angle=45))
print(p)




p <- ggplot(scan_all, aes(x = DateTime, y = `Temperature [°C] - Measured value`)) +
  geom_line(linewidth = 0.4, color = "steelblue") +
  scale_x_datetime(
    date_breaks = "1 week",
    date_minor_breaks = "1 day",
    date_labels = "%m/%d",
    expand = c(0.01, 0)          # trims empty space at the edges
  ) +
  labs(x = NULL, y = "Temperature (°C)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # hjust fixes label alignment
    panel.grid.minor.y = element_blank()
  )

ggsave("temperature.png", p, width = 16, height = 5, dpi = 300)


p <- ggplot(scan_all, aes(x = DateTime, y = `DOCeq [mg/l] - Measured value`)) +
  geom_line(linewidth = 0.4, color = "purple3") +
  scale_x_datetime(
    date_breaks = "1 week",
    date_minor_breaks = "1 day",
    date_labels = "%m/%d",
    expand = c(0.01, 0)          # trims empty space at the edges
  ) +
  labs(x = NULL, y = "DOC_uncalibrated") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # hjust fixes label alignment
    panel.grid.minor.y = element_blank()
  )

ggsave("DOC_uncalibrated.png", p, width = 16, height = 5, dpi = 300)

p <- ggplot(scan_all, aes(x = DateTime, y = `NO3eq [mg/l] - Measured value`)) +
  geom_line(linewidth = 0.4, color = "green4") +
  scale_x_datetime(
    date_breaks = "1 week",
    date_minor_breaks = "1 day",
    date_labels = "%m/%d",
    expand = c(0.01, 0)          # trims empty space at the edges
  ) +
  labs(x = NULL, y = "NO3_uncalibrated") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # hjust fixes label alignment
    panel.grid.minor.y = element_blank()
  )

ggsave("NO3_uncalibrated.png", p, width = 16, height = 5, dpi = 300)


p <- ggplot(scan_all, aes(x = DateTime, y = `TOCeq [mg/l] - Measured value`)) +
  geom_line(linewidth = 0.4, color = "darkblue") +
  scale_x_datetime(
    date_breaks = "1 week",
    date_minor_breaks = "1 day",
    date_labels = "%m/%d",
    expand = c(0.01, 0)          # trims empty space at the edges
  ) +
  labs(x = NULL, y = "TOCeq_uncalibrated") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # hjust fixes label alignment
    panel.grid.minor.y = element_blank()
  )

ggsave("TOC_uncalibrated.png", p, width = 16, height = 5, dpi = 300)



p <- ggplot(scan_all, aes(x = DateTime, y = `TSSeq [mg/l] - Measured value`)) +
  geom_line(linewidth = 0.4, color = "brown4") +
  scale_x_datetime(
    date_breaks = "1 week",
    date_minor_breaks = "1 day",
    date_labels = "%m/%d",
    expand = c(0.01, 0)          # trims empty space at the edges
  ) +
  labs(x = NULL, y = "TSS_uncalibrated") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # hjust fixes label alignment
    panel.grid.minor.y = element_blank()
  )

ggsave("TSS_uncalibrated.png", p, width = 16, height = 5, dpi = 300)
















#Haven't done this part yet




##==============================================================================
## Now we need to do the same thing but for the compensated abs tab
## abs file is in the same excel in second tab of file
##==============================================================================

##########################
#### Import scan data ####
##########################
# Create empty list to store data frames
scan_list <- list()

# Loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # Read the header row (row 2)
  header <- read_excel(local_path, sheet = 2, skip = 1, n_max = 1, col_names = FALSE)
  # Convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # Read the data starting from row 4 using the header as column names
  data <- read_excel(local_path,sheet = 2, skip = 4, col_names = col_names)
  
  # Store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}

####################################
#### Combine data for each site ####
####################################
# Change DateTime column name
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Change names for easier handling
  colnames(df)[1] ="DateTime"
  
  # Update the data frame in the list
  scan_list[[i]] <- df
}

# Site names
site_names <- c("NHCTB", "NHLMP07", "NHLMP27", "NHLMP72", "NHSBM", "NHNCBd")

# Group files in `scan_list` by matching `site_names` in file names
scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list) gives the names of all files in scan_list.
  site_files <- names(scan_list)[grepl(site, names(scan_list))] 
  # grep checks if the current site (e.g., SSM01) appears in each file name in scan_list. 
  # This returns a logical vector (TRUE for matches, FALSE otherwise).
  scan_list[site_files] # Select only the files for this site
  # The [ ] indexing selects only the file names where the match is TRUE.
})

# Name the list by site
names(scan_list_by_site) <- site_names

# Combine data for each site
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  # Bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # Ensure chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # Remove duplicates
})

##############################
#### Save combined files  ####
##############################
# Ensure DateTime column is properly formatted
combined_by_site <- lapply(combined_by_site, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") # Ensure consistent format
  return(df)
})

lapply(names(combined_by_site), function(site) {
  write.csv(combined_by_site[[site]], file.path("data", paste0(site, "_abs.csv")))
})

lapply(names(combined_by_site), function(site) {
  file <- paste0("data/", site, "_abs.csv")
  # this is the in use folder
  drive_folder_id <- "1llXcmKVhauTAHcnTuXuhhatPtEaMoeW2"
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})