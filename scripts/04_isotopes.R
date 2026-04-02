##==============================================================================
## Project: TEA
## Author: Carolina May
## This script downloads, joins, and plots isotope data from Teakettle 2025
## Last update: 2026-04-02
##===========================================================================
##############
## PACKAGES ##
##############

library(googledrive)
library(dplyr)
library(lubridate)

#### Download data from Google Drive & read-in ####

drive_auth()

#First isotope results
folder <- as_id("1Z1TNB7z_vk6gyMWdMS4axywbN44RwvW_")
files <- drive_ls(folder)
file <- files[files$name == "CSI_TEA_ISO_Results.csv", ]

drive_download(file, 
               path = "CSI_TEA_ISO_Results.csv", 
               type = "csv",
               overwrite = TRUE)

iso_data <- read.csv("CSI_TEA_ISO_Results.csv")

#Next get isotope sample reps from the sample sheet

file <- as_id("1mM3aLXfBIAr555XODXnmcu7r74yRFHKqQIe5ej94gBQ")

drive_download(file, 
               path = "CSI_TEA_ISO_Samples.csv", 
               type = "csv",
               overwrite = TRUE)


iso_samples <- read.csv("CSI_TEA_ISO_Samples.csv")


#Last get sample info from master datasheet
file<-as_id("1xxSKNQiXFZ-jtFHj2ruqwq5LqSrl9hc37rCcNp8gQ0s")

drive_download(
  file,
  path = "samples.csv",
  type = "csv",
  overwrite = TRUE
)

samples <- read.csv("samples.csv")


#### Join relevant data ####

# Join iso_data & iso_samples
iso_samples$Sample.ID <- iso_samples$CSI.Vial.ID
iso_data_joined <- left_join(iso_data, iso_samples, by = "Sample.ID")

#why did we get more sample data back than we sent in? Strange!  To make the join work, removing the extras
iso_data_joined <- iso_data_joined %>%
  filter(Sample.ID != "TEA_200" & Sample.ID != "TEA_201" & Sample.ID != "TEA_202" & Sample.ID != "TEA_203" & Sample.ID != "TEA_204" & Sample.ID != "TEA_205" & Sample.ID != "TEA_206" & Sample.ID != "TEA_207")

#Filter for iso bottle IDs and join iso_data & master samples datasheet
samples <- samples %>%
  filter(bottle_type == "iso_vial")

iso_data_joined$ID <- as.character(iso_data_joined$Rep..)
iso_data_joined <- left_join(iso_data_joined, samples, by = "ID")


#### Make plots and save ####

#Change date from character to date
iso_data_joined$Date <- as.Date(iso_data_joined$Date)

#First plots that show trends at each sampling location

p<- ggplot(iso_data_joined, aes(x = Date, y = dD, color = Site, group = Site)) +
  geom_line() +
  geom_point() +                    
  theme_minimal() +
  labs(
    title = "δD by Site (2025)",
    x = "Date",
    y = expression(delta*D~"(‰)"),
    color = "Site"
  )


ggsave(
  filename = "plots/dD_by_site_2025.png", 
  width = 8, height = 5, dpi = 300
)


p2<- ggplot(iso_data_joined, aes(x = Date, y = d18O, color = Site, group = Site)) +
  geom_line() +
  geom_point() +                    
  theme_minimal() +
  labs(
    title = "δ18O by Site (2025)",
    x = "Date",
    y = expression(delta*"18O"~"(‰)"),
    color = "Site"
  )
ggsave(
  filename = "plots/d18O_by_site_2025.png",  
  plot = p2,
  width = 8, height = 5, dpi = 300
)


#Next get mean of monthly campaign values & plot

#get month
iso_data_joined <- iso_data_joined %>%
  mutate(YearMonth = floor_date(Date, unit = "month"))

#Group by month and get mean values & confidence intervals
iso_summary_dD <- iso_data_joined %>%
  group_by(YearMonth) %>%
  summarise(
    mean_dD = mean(dD, na.rm = TRUE),
    sd_dD = sd(dD, na.rm = TRUE),
    n = sum(!is.na(dD)),
    se_dD = sd_dD / sqrt(n),
    ci_upper = mean_dD + qt(0.975, df = n-1) * se_dD,
    ci_lower = mean_dD - qt(0.975, df = n-1) * se_dD
  )


iso_summary_d18O <- iso_data_joined %>%
  group_by(YearMonth) %>%
  summarise(
    mean_d18O = mean(d18O, na.rm = TRUE),
    sd_d18O = sd(d18O, na.rm = TRUE),
    n = sum(!is.na(d18O)),
    se_d18O = sd_d18O / sqrt(n),
    ci_upper = mean_d18O + qt(0.975, df = n-1) * se_d18O,
    ci_lower = mean_d18O - qt(0.975, df = n-1) * se_d18O
  )


p3 <- ggplot(iso_summary_dD, aes(x = YearMonth, y = mean_dD)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "red") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(
    title = "Monthly Mean δD (All Sites 2025)",
    x = "Month",
    y = expression(delta*D~"(‰)"),
    caption = "Shaded area = 95% CI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "plots/dD_means_2025.png",  
  plot = p3,
  width = 8, height = 5, dpi = 300
)



p4<-ggplot(iso_summary_d18O, aes(x = YearMonth, y = mean_d18O)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "red") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(
    title = "Monthly Mean δ18O (All Sites 2025)",
    x = "Month",
    y = expression(delta*"18O"~"(‰)"),
    caption = "Shaded area = 95% CI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "plots/d18O_means_2025.png", 
  plot = p4,
  width = 8, height = 5, dpi = 300
)


#Alrighty delete all the csvs we downloaded to clean up the repo before committing and pushing
# List all CSV files in the current working directory (repo root)
csv_files <- list.files(path = ".", pattern = "\\.csv$", full.names = TRUE)

# Remove them
file.remove(csv_files)



