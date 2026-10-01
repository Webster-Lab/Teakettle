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

p <- ggplot(Q, aes(Date_cal, Q, color = Year)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~DataID, scales = "free_y", ncol = 5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(x = NULL, y = "Q", color = "Year") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("figures/Q_seasonal_by_DataID.png", p, width = 14, height = 10, dpi = 200)


