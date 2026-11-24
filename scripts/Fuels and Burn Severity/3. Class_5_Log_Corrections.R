#### Investigating higher fuel loads in the Additional Brown's Lines -------------------------------------

library(dplyr)
library(tidyverse)
library(ggplot2)
library(googledrive)
library(stringr)
library(patchwork)


#### READ IN DATASETS -------------------------------------

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


# 2022 transect-level data
folder <- as_id("1AATNCB0taGl6pY7CG60nvgs0pM00k5Dz")
files <- drive_ls(folder)

file <- files[files$name == "TK2.Fuels.Clean.2022.csv", ]

drive_download(
  file,
  path = "TK2.Fuels.Clean.2022.csv",
  overwrite = TRUE
)

fuel_2022 <- read.csv("TK2.Fuels.Clean.2022.csv")


# 2022 complete log dataset

file <- files[files$name == "cwd2022_alllogs_cleaned.csv", ]

drive_download(
  file,
  path = "cwd2022_alllogs_cleaned.csv",
  overwrite = TRUE
)

logs_2022 <- read.csv("cwd2022_alllogs_cleaned.csv")

#Let's pull out the declassified ones right away

logs_2022 <- logs_2022 %>%
  filter(Category == "In")

# 2021 data


file <- files[files$name == "TK2.Fuels.Clean.2021.csv", ]

drive_download(
  file,
  path = "TK2.Fuels.Clean.2021.csv",
  overwrite = TRUE
)

fuel_2021 <- read.csv(
  "TK2.Fuels.Clean.2021.csv",
  check.names = FALSE
)


#### 2025: TRANSECT-LEVEL DEPTH DATA -------------------------------------

fuel_2025_depth <- fuel_2025 %>%
  mutate(
    Year = 2025,
    
    Mean_Depth = rowMeans(
      cbind(
        as.numeric(X0.5m_Litter_Duff),
        as.numeric(X1.5m_Litter_Duff)
      ),
      na.rm = TRUE
    ),
    
    Mean_Depth = ifelse(is.nan(Mean_Depth), NA, Mean_Depth)
  ) %>%
  group_by(Year, Transect) %>%
  summarise(
    Mean_Depth = if (all(is.na(Mean_Depth))) {
      NA_real_
    } else {
      first(Mean_Depth[!is.na(Mean_Depth)])
    },
    .groups = "drop"
  )


#### 2025: INDIVIDUAL LOG DATA -------------------------------------

# Species.Code is the species column in Additional_Fuels_2025_Compiled.csv.
fuel_2025_logs <- fuel_2025 %>%
  transmute(
    Year = 2025,
    Transect,
    Species.Code = as.character(Species.Code),
    Diameter = as.numeric(Diameter..cm.),
    Decay = as.numeric(as.character(Decay.Class))
  ) %>%
  filter(!is.na(Diameter))


#### 2022: TRANSECT-LEVEL DEPTH DATA -------------------------------------

fuel_2022_depth <- fuel_2022 %>%
  mutate(
    Year = 2022,
    
    Depth_1m = X1m..Litter. + X1m..Duff.,
    Depth_3m = X3m..Litter. + X3m..Duff.,
    
    Mean_Depth = rowMeans(
      cbind(Depth_1m, Depth_3m),
      na.rm = TRUE
    ),
    
    Mean_Depth = ifelse(is.nan(Mean_Depth), NA, Mean_Depth)
  ) %>%
  group_by(Year, Plot, Transect) %>%
  summarise(
    Mean_Depth = if (all(is.na(Mean_Depth))) {
      NA_real_
    } else {
      mean(Mean_Depth, na.rm = TRUE)
    },
    .groups = "drop"
  )


#### 2022: INDIVIDUAL LOG DATA -------------------------------------
# USE SEPARATE COMPLETE LOG DATASET

fuel_2022_logs <- logs_2022 %>%
  separate(
    Plot.Transect,
    into = c("Plot", "Transect"),
    sep = "-",
    remove = FALSE
  ) %>%
  transmute(
    Year = 2022,
    Plot = as.character(Plot),
    Transect = as.character(Transect),
    Diameter = as.numeric(Diam),
    Decay = as.numeric(as.character(DecayClass))
  ) %>%
  filter(!is.na(Diameter)) 


#### 2021: TRANSECT-LEVEL DEPTH DATA -------------------------------------

fuel_2021_depth <- fuel_2021 %>%
  mutate(
    Year = 2021,
    
    Mean_Depth = rowMeans(
      cbind(
        as.numeric(`1m (Litter)`),
        as.numeric(`3m (Litter)`)
      ),
      na.rm = TRUE
    ),
    
    Mean_Depth = ifelse(is.nan(Mean_Depth), NA, Mean_Depth)
  ) %>%
  group_by(Year, Plot, Transect) %>%
  summarise(
    Mean_Depth = if (all(is.na(Mean_Depth))) {
      NA_real_
    } else {
      mean(Mean_Depth, na.rm = TRUE)
    },
    .groups = "drop"
  )


#### 2021: INDIVIDUAL LOG DATA -------------------------------------

fuel_2021_logs <- fuel_2021 %>%
  pivot_longer(
    cols = matches("^(Diam|Dec)(_[0-9]+)?$"),
    names_to = c(".value", "Log_Number"),
    names_pattern = "^(Diam|Dec)(?:_([0-9]+))?$"
  ) %>%
  transmute(
    Year = 2021,
    Plot = as.character(Plot),
    Transect = as.character(Transect),
    Diameter = as.numeric(Diam),
    Decay = as.numeric(as.character(Dec))
  ) %>%
  filter(!is.na(Diameter))


#### COMBINE YEARS -------------------------------------

# Transect-level depth comparison

fuel_depth_comparison <- bind_rows(
  
  fuel_2021_depth %>%
    mutate(
      Year = as.integer(Year),
      Plot = as.character(Plot),
      Transect = as.character(Transect)
    ) %>%
    select(Year, Plot, Transect, Mean_Depth),
  
  fuel_2022_depth %>%
    mutate(
      Year = as.integer(Year),
      Plot = as.character(Plot),
      Transect = as.character(Transect)
    ) %>%
    select(Year, Plot, Transect, Mean_Depth),
  
  fuel_2025_depth %>%
    mutate(
      Year = as.integer(Year),
      Plot = NA_character_,
      Transect = as.character(Transect)
    ) %>%
    select(Year, Plot, Transect, Mean_Depth)
)


# Individual log comparison
# (Species.Code only exists for 2025, so 2021 and 2022 logs get NA for Species.Code)

fuel_log_comparison <- bind_rows(
  
  fuel_2021_logs,
  
  fuel_2022_logs,
  
  fuel_2025_logs %>%
    mutate(
      Year = as.integer(Year),
      Plot = NA_character_,
      Transect = as.character(Transect)
    ) %>%
    select(Year, Plot, Transect, Species.Code, Diameter, Decay)
)

# Remove class 4-5 to check discrepancy

fuel_log_comparison_sound <- fuel_log_comparison %>%
  filter(Decay != 5 & Decay != 4)

#### CHECK DATASETS -------------------------------------

head(fuel_depth_comparison)
head(fuel_log_comparison)

summary(fuel_depth_comparison)
summary(fuel_log_comparison)


# Number of transects per year

fuel_depth_comparison %>%
  group_by(Year) %>%
  summarise(
    Transects = n(),
    Nonmissing_Depth = sum(!is.na(Mean_Depth)),
    .groups = "drop"
  )


# Number of logs per year

fuel_log_comparison %>%
  count(Year, Decay)


#### SUMMARY STATISTICS -------------------------------------

# Mean litter + duff depth by year

depth_summary <- fuel_depth_comparison %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(Mean_Depth, na.rm = TRUE),
    SD = sd(Mean_Depth, na.rm = TRUE),
    N = sum(!is.na(Mean_Depth)),
    SE = SD / sqrt(N),
    .groups = "drop"
  )


# Mean log diameter by year

diameter_summary <- fuel_log_comparison %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(Diameter, na.rm = TRUE),
    SD = sd(Diameter, na.rm = TRUE),
    N = n(),
    SE = SD / sqrt(N),
    .groups = "drop"
  )


depth_summary
diameter_summary


#### PLOT COLORS -------------------------------------

year_colors <- c(
  "2021" = "#8C6DAD",
  "2022" = "#D9A441",
  "2025" = "#547A56"
)


#### PLOT 1: MEAN LITTER + DUFF DEPTH -------------------------------------

p1 <- ggplot(
  depth_summary,
  aes(x = factor(Year), y = Mean, fill = factor(Year))
) +
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    width = 0.2,
    linewidth = 0.8
  ) +
  scale_fill_manual(values = year_colors) +
  labs(
    title = "Mean Litter + Duff Depth by Year",
    x = "Year",
    y = "Mean Depth (cm)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

p1


#### PLOT 2: MEAN LOG DIAMETER -------------------------------------

p2 <- ggplot(
  diameter_summary,
  aes(x = factor(Year), y = Mean, fill = factor(Year))
) +
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    width = 0.2,
    linewidth = 0.8
  ) +
  scale_fill_manual(values = year_colors) +
  labs(
    title = "Mean Log Diameter by Year",
    x = "Year",
    y = "Mean Diameter (cm)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

p2


# Combine first two plots

p1 + p2


#### LOGS PER METER (INCLUDING ZERO-LOG TRANSECTS) -------------------------------------

# Create complete list of sampled transects

all_transects <- bind_rows(
  
  fuel_2021_depth %>%
    select(Year, Plot, Transect) %>%
    mutate(
      Year = as.integer(Year),
      Plot = as.character(Plot),
      Transect = as.character(Transect)
    ),
  
  fuel_2022_depth %>%
    select(Year, Plot, Transect) %>%
    mutate(
      Year = as.integer(Year),
      Plot = as.character(Plot),
      Transect = as.character(Transect)
    ),
  
  fuel_2025_depth %>%
    select(Year, Transect) %>%
    mutate(
      Year = as.integer(Year),
      Plot = NA_character_,
      Transect = as.character(Transect)
    )
) %>%
  distinct(Year, Plot, Transect)


# Count logs per transect

log_counts <- fuel_log_comparison %>%
  group_by(Year, Plot, Transect) %>%
  summarise(
    Number_of_Logs = n(),
    .groups = "drop"
  )


# Join log counts to all sampled transects

fuel_logs_per_meter <- all_transects %>%
  left_join(
    log_counts,
    by = c("Year", "Plot", "Transect"),
    na_matches = "na"
  ) %>%
  mutate(
    Number_of_Logs = replace_na(Number_of_Logs, 0),
    
    Transect_Length = ifelse(
      Year %in% c(2021, 2022),
      25,
      15
    ),
    
    Logs_per_Meter = Number_of_Logs / Transect_Length
  )


# View results

fuel_logs_per_meter


#### SUMMARY: LOGS PER METER -------------------------------------

logs_meter_summary <- fuel_logs_per_meter %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(Logs_per_Meter),
    SD = sd(Logs_per_Meter),
    N = n(),
    SE = SD / sqrt(N),
    .groups = "drop"
  )

logs_meter_summary


#### PLOT 3: MEAN LOG DENSITY -------------------------------------

p3 <- ggplot(
  logs_meter_summary,
  aes(x = factor(Year), y = Mean, fill = factor(Year))
) +
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    width = 0.2,
    linewidth = 0.8
  ) +
  scale_fill_manual(values = year_colors) +
  labs(
    title = "Mean Log Density by Year",
    x = "Year",
    y = "Mean Number of Logs per Meter"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

p3


#### COMBINE ALL THREE PLOTS -------------------------------------

p1 + p2 + p3














#### COMPARE CARBON -------------------------------------

#Download all files
# Google Drive folder ID
folder <- as_id("130RccN323vwDP-h2qH7AMP_mVBP4qGc7")

# List all files in folder
files <- drive_ls(folder)

# Download all files
for (i in seq_len(nrow(files))) {
  drive_download(
    files[i, ],
    path = files$name[i],
    overwrite = TRUE
  )
}



#### INVESTIGATING CARBON BY FUEL TYPE AND YEAR -------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)

#### READ IN DATASETS (CARBON) -------------------------------------

c_plot_2026 <- read.csv("plotlevel.carbon.2026.csv")
c_plot_2022 <- read.csv("plotlevel.carbon.2022.csv")
c_add_2025.2026 <- read.csv("add.fuel.carbon.2026.corrected.csv")


#### PREPARE PLOT-LEVEL DATA -------------------------------------

# Add years
c_plot_2026$Year <- 2026
c_plot_2022$Year <- 2022

# Merge new logs with sound CWD in 2022
c_plot_2022$Sound.CWD.Carbon <-
  c_plot_2022$Sound.CWD.Carbon + c_plot_2022$New.CWD.Carbon

c_plot_2022 <- c_plot_2022 %>%
  select(-New.CWD.Carbon)


# Reorder columns in 2022 dataset
c_plot_2022 <- c_plot_2022[, c(
  "Plot",
  "Litter.Carbon",
  "Duff.Carbon",
  "Carbon1hour",
  "Carbon10hour",
  "Carbon100hour",
  "Sound.CWD.Carbon",
  "Rotten.CWD.Carbon",
  "Live.Carbon",
  "Dead.Carbon",
  "FWD.Carbon",
  "Standing.Carbon",
  "Year"
)]


# Reorder columns in 2026 dataset
c_plot_2026 <- c_plot_2026[, c(
  "Plot",
  "Litter.Carbon",
  "Duff.Carbon",
  "Carbon1hour",
  "Carbon10hour",
  "Carbon100hour",
  "Sound.CWD.Carbon",
  "Rotten.CWD.Carbon",
  "Live.Carbon",
  "Dead.Carbon",
  "FWD.Carbon",
  "Standing.Carbon",
  "Year"
)]


# Replace missing values in 2026 plot-level data with zero
c_plot_2026[is.na(c_plot_2026)] <- 0


# Combine plot-level datasets
c_plot <- bind_rows(
  c_plot_2026,
  c_plot_2022
) %>%
  mutate(
    littduff.carbon = Litter.Carbon + Duff.Carbon
  ) %>%
  select(-Litter.Carbon, -Duff.Carbon)


#### PREPARE ADDITIONAL FUELS DATA -------------------------------------

c_add_2025.2026 <- c_add_2025.2026 %>%
  mutate(
    Plot = transect,
    Year = year
  ) %>%
  select(
    -Weight1hour,
    -Weight10hour,
    -Weight100hour,
    -littduff.biomass,
    -CWD.Biomass.Sound,
    -CWD.Biomass.Rotten,
    -CWD.Biomass,
    -year,
    -transect
  )


#### STANDARDIZE ADDITIONAL FUELS DATA -------------------------------------

c_add_clean <- c_add_2025.2026 %>%
  rename(
    Sound.CWD.Carbon = CWD.Carbon.Sound,
    Rotten.CWD.Carbon = CWD.Carbon.Rotten
  ) %>%
  mutate(
    Year_Type = paste0(Year, "_Add")
  ) %>%
  select(
    Plot,
    Carbon1hour,
    Carbon10hour,
    Carbon100hour,
    Sound.CWD.Carbon,
    Rotten.CWD.Carbon,
    Year_Type,
    littduff.carbon
  )


#### STANDARDIZE PLOT-LEVEL DATA -------------------------------------

c_plot_clean <- c_plot %>%
  mutate(
    Year_Type = paste0(Year, "_Plot")
  ) %>%
  select(
    Plot,
    Carbon1hour,
    Carbon10hour,
    Carbon100hour,
    Sound.CWD.Carbon,
    Rotten.CWD.Carbon,
    Year_Type,
    littduff.carbon
  )


#### COMBINE ALL DATASETS -------------------------------------

carbon_combined <- bind_rows(
  c_plot_clean,
  c_add_clean
) %>%
  mutate(
    Year_Type = factor(
      Year_Type,
      levels = c(
        "2022_Plot",
        "2025_Add",
        "2026_Plot",
        "2026_Add"
      )
    )
  ) %>%
  arrange(Year_Type, Plot)


# Check combined dataset
head(carbon_combined)
glimpse(carbon_combined)

carbon_combined %>%
  count(Year_Type)


#### PREPARE DATA FOR STACKED BAR GRAPH -------------------------------------

carbon_long <- carbon_combined %>%
  pivot_longer(
    cols = c(
      Carbon1hour,
      Carbon10hour,
      Carbon100hour,
      Sound.CWD.Carbon,
      Rotten.CWD.Carbon,
      littduff.carbon
    ),
    names_to = "Fuel_Type",
    values_to = "Carbon"
  ) %>%
  mutate(
    Fuel_Type = recode(
      Fuel_Type,
      "Carbon1hour" = "1 Hour",
      "Carbon10hour" = "10 Hour",
      "Carbon100hour" = "100 Hour",
      "Sound.CWD.Carbon" = "Coarse Woody Debris (Class 1-3)",
      "Rotten.CWD.Carbon" = "Coarse Woody Debris (Class 4-5)",
      "littduff.carbon" = "Litter + Duff"
    ),
    Fuel_Type = factor(
      Fuel_Type,
      levels = c(
        "1 Hour",
        "10 Hour",
        "100 Hour",
        "Coarse Woody Debris (Class 1-3)",
        "Coarse Woody Debris (Class 4-5)",
        "Litter + Duff"
      )
    )
  )



#### CALCULATE MEAN CARBON BY YEAR TYPE AND FUEL TYPE -------------------------------------

carbon_summary <- carbon_long %>%
  group_by(Year_Type, Fuel_Type) %>%
  summarise(
    Mean_Carbon = if (all(is.na(Carbon))) {
      NA_real_
    } else {
      mean(Carbon, na.rm = TRUE)
    },
    SD = sd(Carbon, na.rm = TRUE),
    N = sum(!is.na(Carbon)),
    SE = SD / sqrt(N),
    .groups = "drop"
  )


# View summary
carbon_summary


#### STACKED BAR GRAPH -------------------------------------

p_carbon <- ggplot(
  carbon_summary,
  aes(
    x = Year_Type,
    y = Mean_Carbon,
    fill = Fuel_Type
  )
) +
  geom_col(
    width = 0.65,
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = c(
      "1 Hour" = "#facd7f",
      "10 Hour" = "#f5b038",
      "100 Hour" = "orange3",
      "Coarse Woody Debris (Class 1-3)" = "#4061AD",
      "Coarse Woody Debris (Class 4-5)" = "#052878",
      "Litter + Duff" = "#8f608f"
    ),
    breaks = c(
      "1 Hour",
      "10 Hour",
      "100 Hour",
      "Coarse Woody Debris (Class 1-3)",
      "Coarse Woody Debris (Class 4-5)",
      "Litter + Duff"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "2022_Plot" = "2022 Plot",
      "2025_Add" = "2025 Additional",
      "2026_Plot" = "2026 Plot",
      "2026_Add" = "2026 Additional"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Mean Carbon by Fuel Type and Sampling Year_Corrected",
    x = "Year / Dataset",
    y = expression("Mean Carbon (Mg/ha)"),
    fill = "Fuel Type"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )


# Display graph
p_carbon


ggsave(
  "Mean_Carbon_by_Fuel_Type_Corrected.png",
  plot = p_carbon,
  width = 10,
  height = 6,
  dpi = 300
)

# Identify destination Google Drive folder
folder <- as_id("1Wz4wbdnr1I7UyPM5elQTy9NjdFajzHmV")

# Upload figure to Google Drive
drive_upload(
  media = "Mean_Carbon_by_Fuel_Type_Corrected.png",
  path = folder,
  name = "Mean_Carbon_by_Fuel_Type_Corrected.png"
)




#### Correct class 5 diameter distributions -------------------------------------
df <- fuel_log_comparison %>%
  filter(Year == 2025)
df <- df %>%
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



write.csv(df, "Prefire_Logs_combined.csv")


#### Use Quantile Mapping to re-assign Class 5 diameter values based on Class 1-4 diameter distributions -------------------------------------

# Drop logs with no decay class (one row in this file), since they can't be assigned to a pool
df <- df %>% filter(!is.na(Decay))

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



# Sanity check: the class 5 distribution should now match class 1-4
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

ggplot(plot_df, aes(x = Diameter, fill = Pool)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10, boundary = 0, color = "white") +
  facet_wrap(~ Pool, ncol = 1) +
  labs(x = "Diameter (cm)", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")