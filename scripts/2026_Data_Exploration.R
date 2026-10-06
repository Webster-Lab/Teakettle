##==============================================================================
## Project: TEA
## Author: Carolina May
## This script downloads, joins, and plots discharge (salt_slug) data from Teakettle 2026
## Last update: 2026-09-18
##===========================================================================
##############
## PACKAGES ##
##############

library(googledrive)
library(dplyr)
library(lubridate)
library(tidyverse)


#### Download data from Google Drive & read-in ####

drive_auth()


folder <- as_id("1TBqljxX63FxM2Hw6GL6DBPW6ZGkPPktC")
files <- drive_ls(folder)
file <- files[files$name == "2026_Q", ]

drive_download(file, 
               path = "2026_Q", 
               type = "csv",
               overwrite = TRUE)

Q <- read.csv("2026_Q.csv")

#Do a little cleaning
#Let's remove any values where the slug was flagged as "not good" since these are unreliable.  We will keep the "ok" flags for this analysis

Q <- Q %>%
  filter(flag != "not good")

#Next let's plot these up
#First, all plot's together 
Q <- Q |>
  mutate(
    Date     = as.Date(Date, format = "%m/%d/%Y"),
    Year     = factor(year(Date)),
    Date_cal = as.Date(format(Date, "2000-%m-%d"))  # same dummy year for all rows
  )
p <- ggplot(Q, aes(Date_cal, Q, color = Year, group = Year)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~DataID, scales = "free_y", ncol = 5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(x = NULL, y = "Q", color = "Year") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 20),
    legend.title    = element_text(size = 20)
  )
p
ggsave("plots/Q_seasonal_by_DataID.png", p, width = 14, height = 10, dpi = 200)




#Plot SPC, Temp, and pH

#read in samples from Webster Lab Master Sample Sheet

folder <- as_id("1rb2J-oNv-34ajvZjBKtxaew-ezmFEQ-Y")
files <- drive_ls(folder)
file <- files[files$name == "Webster Lab Samples Log Sheet", ]

drive_download(file, 
               path = "Samples", 
               type = "csv",
               overwrite = TRUE)

samples <- read.csv("Samples.csv")

#filter by TEA project and select just one sample rep to avoid duplicates
samples <- samples %>%
  filter(Project == "TEA", bottle_type == "60_mL", Rep == "Rep1")

#Read in TSS data as well

folder <- as_id("1-DBXgOD1P1RQW9xQXGKz4JIz_ARH-VQ9")
files <- drive_ls(folder)
file <- files[files$name == "TSS_calculations.csv", ]

drive_download(file, 
               path = "TSS_calculations", 
               type = "csv",
               overwrite = TRUE)

TSS <- read.csv("TSS_calculations.csv")

TSS <- TSS %>%
  mutate(Date = as.Date(Date_collected, format = "%m/%d/%Y"), Site = as.factor(Site))





samples_plot <- samples |>
  mutate(
    Date      = as.Date(Date),
    Year      = factor(year(Date)),
    Date_cal  = as.Date(format(Date, "2000-%m-%d")),  # dummy year for plotting
    Site      = as.factor(Site),
    SpC_uScm2 = as.numeric(SpC_uScm2),
    pH = as.numeric(pH),
    temp_C = as.numeric(temp_C),
    # remove bad TEAK03 spc point from when the computer melted down
    SpC_uScm2 = if_else(Site == "TEAK03" & Date == as.Date("2026-06-17"),
                        NA_real_, SpC_uScm2)
  ) |>
  filter(!is.na(SpC_uScm2))


samples_TSS <- left_join(samples_plot, TSS, by = c("Date", "Site"))




#Plot it up

p <- ggplot(samples_plot, aes(Date_cal, SpC_uScm2, color = Year)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Site, scales = "free_y", ncol = 5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(x = NULL, y = "SPC uS/cm2", color = "Year") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 20),
    legend.title    = element_text(size = 20)
  )
p

ggsave("plots/SPC_plot.png", p, width = 14, height = 10, dpi = 300)


p <- ggplot(samples_plot, aes(Date_cal, pH, color = Year)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Site, scales = "free_y", ncol = 5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(x = NULL, y = "pH", color = "Year") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 20),
    legend.title    = element_text(size = 20)
  )
p
p

p <- ggplot(samples_plot, aes(Date_cal, temp_C, color = Year)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Site, scales = "free_y", ncol = 5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(x = NULL, y = "Water Temperature (C)", color = "Year") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 20),
    legend.title    = element_text(size = 20)
  )
p

ggsave("plots/Temp_plot.png", p, width = 14, height = 10, dpi = 300)



#Calculate coeffecient of variation over space

cv <- function(x) sd(x) / mean(x)

##TEMP
#remove outlet monitoring and the 9th campaign (cannot compare september since it was not done in 2025)
samples_cv <- samples_plot %>%
  filter(Purpose == "synoptic", Campaign != 9) %>%
  group_by(Campaign, Year) %>%
  summarize(mean_temp = mean(temp_C, na.rm = TRUE), sd = sd(temp_C, na.rm = TRUE), CV = cv(temp_C))

ggplot(samples_cv, aes(x = factor(Campaign), y = CV, fill = Year)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("2025" = "grey60", "2026" = "firebrick")) +
  labs(x = "Campaign", y = "Coefficient of Variation Over Space: Water Temperature",
       title = "CV: Water Temp") +
  theme_bw()


##CONDUCTIVITY
samples_cv <- samples_plot %>%
  filter(Purpose == "synoptic", Campaign != 9) %>%
  group_by(Campaign, Year) %>%
  summarize(mean_temp = mean(SpC_uScm2, na.rm = TRUE), sd = sd(SpC_uScm2, na.rm = TRUE), CV = cv(SpC_uScm2))

ggplot(samples_cv, aes(x = factor(Campaign), y = CV, fill = Year)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("2025" = "grey60", "2026" = "firebrick")) +
  labs(x = "Campaign", y = "Coefficient of Variation Over Space: SpC",
       title = "CV: SpC") +
  theme_bw()


###TSS 
## This is a hot mess due to removal of some sites each campaign for bad data -- can't directly compare

cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

samples_TSS_cv <- samples_TSS %>%
  filter(Purpose == "synoptic", Campaign != 9) %>%
  group_by(Campaign, Year) %>%
  summarize(
    n_obs    = sum(!is.na(TSS_mg_per_L)),
    mean_TSS = mean(TSS_mg_per_L, na.rm = TRUE),
    sd       = sd(TSS_mg_per_L, na.rm = TRUE),
    CV       = cv(TSS_mg_per_L),
    .groups  = "drop"
  )

ggplot(samples_TSS_cv, aes(x = factor(Campaign), y = CV, fill = Year)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("2025" = "grey60", "2026" = "firebrick")) +
  labs(x = "Campaign", y = "Coefficient of Variation Over Space: TSS",
       title = "CV: TSS") +
  theme_bw()


#POC

samples_TSS_cv <- samples_TSS %>%
  filter(Purpose == "synoptic", Campaign != 9) %>%
  group_by(Campaign, Year) %>%
  summarize(
    n_obs    = sum(!is.na(C_mg_per_L)),
    mean_POC = mean(C_mg_per_L, na.rm = TRUE),
    sd       = sd(C_mg_per_L, na.rm = TRUE),
    CV       = cv(C_mg_per_L),
    .groups  = "drop"
  )

ggplot(samples_TSS_cv, aes(x = factor(Campaign), y = CV, fill = Year)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("2025" = "grey60", "2026" = "firebrick")) +
  labs(x = "Campaign", y = "Coefficient of Variation Over Space: POC",
       title = "CV: POC") +
  theme_bw()


#Alrighty delete all the csvs we downloaded to clean up the repo before committing and pushing
# List all CSV files in the current working directory (repo root)
csv_files <- list.files(path = ".", pattern = "\\.csv$", full.names = TRUE)

# Remove them
file.remove(csv_files)

