#### Correcting Class 5 Log Diameters
#Author: Carolina May
#Last Updated: 2026-11-25

#Exploratory data plotting indicated class 5 logs have larger diameters than expected, inflating carbon estimates substantially
#This is likely due to overestimating the diameter of slumped, heavily decayed logs
# To correct for this, this script calculates diameter quantiles of class 1-4 logs and reassigns diameters to class 5 logs to match the distribution of 1-4 logs
# This reduces the diameters of class 5 logs substantially and allows for carbon loads to be within reasonable expected ranges
# For more exploratory code investigating this issue, see archived scripts

#Use Ctrl + Shift + O to see script overview by section

library(dplyr)
library(tidyverse)
library(ggplot2)
library(googledrive)
library(stringr)
library(patchwork)


#### Read in datasets  -------------------------------------

# 2025 data
folder <- as_id("12iF1t9FRETTb08eRjNvXjJkYHZGOTMeu")
files <- drive_ls(folder)

file <- files[files$name == "Additional_Fuels_2025_Compiled.csv", ]

drive_download(
  file,
  path = "Additional_Fuels_2025_Compiled.csv",
  overwrite = TRUE
)

fuel_2025 <- read.csv("Additional_Fuels_2025_Compiled.csv")


### Individual log data -------------------------------------

# Species.Code is the species column in Additional_Fuels_2025_Compiled.csv.
fuel_2025_logs <- fuel_2025 %>%
  transmute(
    Year = 2025,
    Transect,
    Species.Code = as.character(Species.Code),
    Diameter = as.numeric(Diameter..cm.),
    Decay = as.numeric(as.character(Decay.Class))
  ) %>%
  filter(!is.na(Diameter)) %>%
  filter(!is.na(Decay))




#### Correct class 5 diameter distributions -------------------------------------
df <- fuel_2025_logs %>%
  mutate(ClassGroup = factor(case_when(Decay <= 3 ~ "Class 1-3 (Sound)",
                                       Decay == 4 ~ "Class 4",
                                       Decay == 5 ~ "Class 5"),
                             levels = c("Class 1-3 (Sound)", "Class 4", "Class 5")))




# Mean and median per class group
stats <- df %>%
  group_by(ClassGroup) %>%
  summarise(Mean = mean(Diameter), Median = median(Diameter), .groups = "drop") %>%
  pivot_longer(c(Mean, Median), names_to = "Statistic", values_to = "value")

ggplot(df, aes(x = Diameter)) +
  geom_histogram(aes(fill = ClassGroup), binwidth = 10, boundary = 0, color = "white") +
  geom_vline(data = stats,
             aes(xintercept = value, linetype = Statistic, color = Statistic),
             linewidth = 1) +
  facet_wrap(~ ClassGroup, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Class 1-3 (Sound)" = "#2c7fb8",
                               "Class 4" = "#fdae61",
                               "Class 5" = "#d73027"),
                    guide = "none") +
  scale_color_manual(values = c(Mean = "black", Median = "firebrick")) +
  scale_linetype_manual(values = c(Mean = "solid", Median = "dashed")) +
  labs(x = "Diameter (cm)", y = "Number of logs",
       title = "Log diameter distribution by decay class",
       color = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"))





#### Use Quantile Mapping to re-assign Class 5 diameter values based on Class 1-4 diameter distributions -------------------------------------


# Set the reference pool: class 1-4 logs from 2025, the same survey as the class 5 logs.
ref_diam <- df %>%
  filter(Year == 2025, Decay <= 4) %>%
  pull(Diameter)

# Identify which logs get corrected (2025 class 5 only)
fix_idx <- df$Year == 2025 & df$Decay == 5

# Convert each class 5 diameter to a percentile within the class 5 pool.
# Mid-ranks (rank - 0.5) keep percentiles strictly between 0 and 1.
p <- (rank(df$Diameter[fix_idx], ties.method = "average") - 0.5) / sum(fix_idx)

# Look up the class 1-4 diameter at those same percentiles
mapped <- quantile(ref_diam, probs = p, type = 8, names = FALSE)

# Build predicted_diameter: unchanged for everything except 2025 class 5
df$predicted_diameter <- df$Diameter
df$predicted_diameter[fix_idx] <- mapped

#Write csv and upload to gdrive

write.csv(df, "2025_Fuels_Class_5_Predicted.csv")

# Identify destination Google Drive folder
folder <- as_id("1xneBVWvZuoQEdT53WLp3k8Iw3JcCXWRm")

# Upload figure to Google Drive
drive_upload(
  media = "2025_Fuels_Class_5_Predicted.csv",
  path = folder,
  name = "2025_Fuels_Class_5_Predicted.csv"
)



#The class 5 distribution should now match class 1-4
df %>%
  filter(Year == 2025) %>%
  mutate(ClassGroup = case_when(Decay <= 3 ~ "1-3", Decay == 4 ~ "4", Decay == 5 ~ "5")) %>%
  group_by(ClassGroup) %>%
  summarise(n = n(),
            orig_median = median(Diameter),
            pred_median = median(predicted_diameter),
            orig_mean = mean(Diameter),
            pred_mean = mean(predicted_diameter))

# Plot class 5 before vs after against the class 1-4 reference

plot_df <- bind_rows(
  df %>% filter(Year == 2025, Decay <= 4) %>%
    transmute(Pool = "Class 1-4 (reference)", Diameter = Diameter),
  df %>% filter(fix_idx) %>%
    transmute(Pool = "Class 5 (original)", Diameter = Diameter),
  df %>% filter(fix_idx) %>%
    transmute(Pool = "Class 5 (predicted)", Diameter = predicted_diameter)
) %>%
  mutate(Pool = factor(Pool, levels = c("Class 1-4 (reference)",
                                        "Class 5 (original)",
                                        "Class 5 (predicted)")))

p <- ggplot(plot_df, aes(x = Diameter, fill = Pool)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10, boundary = 0, color = "white") +
  facet_wrap(~ Pool, ncol = 1) +
  labs(x = "Diameter (cm)", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")
p


ggsave("Diameter_distribution.png", plot = p, width = 6, height = 8, dpi = 300)


# Upload figure to Google Drive
drive_upload(
  media = "Diameter_distribution.png",
  path = folder,
  name = "Diameter_distribution.png"
)
