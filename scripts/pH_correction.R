
## Project: TEAKETTLE
## Script to correct pH data for YSI calibration drift
## press Command+Option+O to collapse all sections and get an overview of the workflow!



### Packages ####

library(googledrive) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)

### Read in ancillary data with YSI unit recorded ####


folder <- as_id("1AXN6d2Id_9gGBEnSTdIjaUFVmTzjbxU5")
files <- drive_ls(folder)
file <- files[files$name == "Ancillary data - YSI Unit Recorded", ]

drive_download(file, 
               path = "pH", 
               type = "csv",
               overwrite = TRUE)

samples <- read.csv("pH.csv")


#filter by TEA project and select just one sample rep to avoid duplicates
samples <- samples %>%
  filter(Project == "TEA", bottle_type == "60_mL", Rep == "Rep1")

samples_plot <- samples |>
  mutate(
    Date      = as.Date(Date),
    Year      = factor(year(Date)),
    Date_cal  = as.Date(format(Date, "2000-%m-%d")),  # dummy year for plotting
    Site      = as.factor(Site),
    SpC_uScm2 = as.numeric(SpC_uScm2),
    pH = as.numeric(pH),
    temp_C = as.numeric(temp_C))


### Plot Uncorrected Data ####
#### Ocillations month to month are indicative of different calibrations on the two different YSI units

p <- ggplot(samples_plot, aes(Date_cal, pH, color = Year)) +
  geom_line(na.rm = TRUE) +
  geom_point(aes(shape = YSI.Unit), size = 3, na.rm = TRUE) +
  facet_wrap(~Site, scales = "free_y", ncol = 5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_shape_manual(values = c("Earl Grey" = 16, "Oolong" = 17)) +
  labs(x = NULL, y = "pH", color = "Year", shape = "YSI unit") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 20),
    legend.title    = element_text(size = 20)
  )
p


#Lab testing of YSI's with standards in October 2026 showed that YSI unit Oolong was reading, on average, 0.5375 above Earl Grey.
#Attempted correction:

samples_plot <- samples_plot %>%
  mutate(corrected_ph = case_when(
           YSI.Unit %in% "Oolong" ~ pH - 0.5375,
           .default = pH
         ))

p <- ggplot(samples_plot, aes(Date_cal, corrected_ph, color = Year)) +
  geom_line(na.rm = TRUE) +
  geom_point(aes(shape = YSI.Unit), size = 3, na.rm = TRUE) +
  facet_wrap(~Site, scales = "free_y", ncol = 5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_shape_manual(values = c("Earl Grey" = 16, "Oolong" = 17)) +
  labs(x = NULL, y = "pH", color = "Year", shape = "YSI unit") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 20),
    legend.title    = element_text(size = 20)
  )
p





#What if we just remove Earl Grey and look at Oolong uncorrected?

samples_oolong <- samples_plot %>%
  filter(!(YSI.Unit %in% "Earl Grey" & year(Date) == 2026))

p <- ggplot(samples_oolong, aes(Date_cal, pH, color = Year)) +
  geom_line(na.rm = TRUE) +
  geom_point(aes(shape = YSI.Unit), size = 3, na.rm = TRUE) +
  facet_wrap(~Site, scales = "free_y", ncol = 5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_shape_manual(values = c("Earl Grey" = 16, "Oolong" = 17)) +
  labs(x = NULL, y = "pH", color = "Year", shape = "YSI unit") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 20),
    legend.title    = element_text(size = 20)
  )
p

#This data feels pretty unreliable and hard to validate


