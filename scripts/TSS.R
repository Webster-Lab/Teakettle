##==============================================================================
## Project: TEA
## Author: Carolina May
## This script downloads and plots TSS data (2025-2026) from Teakettle
## Last update: 2026-09-29
##===========================================================================
##############
## PACKAGES ##
##############

library(googledrive)
library(dplyr)
library(lubridate)
library(ggplot2)

#### Download data from Google Drive & read-in ####

drive_auth()


folder <- as_id("1-DBXgOD1P1RQW9xQXGKz4JIz_ARH-VQ9")
files <- drive_ls(folder)
file <- files[files$name == "TSS_Data_Raw", ]

drive_download(file, 
               path = "TSS_Data_Raw", 
               type = "csv",
               overwrite = TRUE)

TSS <- read.csv("TSS_Data_Raw.csv")

#remove tin numbers with no associated data yet
TSS <- TSS %>%
  filter(Date_collected != "")

#make weight numeric
TSS$TSS_weight <- as.numeric(TSS$TSS_weight)
TSS$C_Content <- as.numeric(TSS$C_Content)

#Add flag column anytime the value of TSS weight is less than -0.0030.  This is twice the sensitivity of the scale. 


TSS <- TSS %>%
  mutate(TSS_flag = if_else(TSS_weight < -0.00030, "Flagged", ""))

TSS <- TSS %>%
  mutate(
    TSS_weight_QC = case_when(
    TSS_flag %in% "Flagged" ~ NA_real_,
    TSS_weight <= 0 & TSS_weight >= -0.00030 ~ 0,
    TRUE ~ TSS_weight
  ))

#Calculate TSS and Carbon weight per L

TSS$TSS_mg_per_L <- (TSS$TSS_weight_QC/TSS$Sample_vol_mL) *1000

TSS$C_mg_per_L <- (TSS$C_Content/TSS$Sample_vol_mL) *1000



#Save TSS calculations to file on google drive

write_csv(TSS, "TSS_calculations.csv")   

drive_upload("TSS_calculations.csv",
             path = as_id("1-DBXgOD1P1RQW9xQXGKz4JIz_ARH-VQ9"),   
             name = "TSS_calculations.csv")


#Next let's plot these up
#First, all plot's together 
TSS <- TSS |>
  mutate(
    Date_collected     = as.Date(Date_collected, format = "%m/%d/%Y"),
    Year     = factor(year(Date_collected)),
    Date_cal = as.Date(format(Date_collected, "2000-%m-%d"))  # same dummy year for all rows
  )

p <- ggplot(TSS, aes(Date_cal, TSS_mg_per_L, color = Year)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Site, scales = "free_y", ncol = 5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(x = NULL, y = "TSS (mg/L)", color = "Year") +
  theme_bw() +
  theme(legend.position = "bottom")
p

#Now carbon

TSS <- TSS |>
  mutate(
    Date_collected     = as.Date(Date_collected, format = "%m/%d/%Y"),
    Year     = factor(year(Date_collected)),
    Date_cal = as.Date(format(Date_collected, "2000-%m-%d"))  # same dummy year for all rows
  )

p <- ggplot(TSS, aes(Date_cal, C_mg_per_L, color = Year)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Site, scales = "free_y", ncol = 5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(x = NULL, y = "TOC (mg/L)", color = "Year") +
  theme_bw() +
  theme(legend.position = "bottom")
p


#Plot together on one fig
p <- TSS |>
  filter(Year %in% c("2025", "2026")) |>
  ggplot(aes(Date_cal, TSS_mg_per_L, color = factor(Site), group = Site)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Year, ncol = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = "TSS (mg/L)", color = "Site") +
  theme_bw() +
  theme(legend.position = "bottom")
p

p <- TSS |>
  filter(Year %in% c("2025", "2026")) |>
  ggplot(aes(Date_cal, C_mg_per_L, color = factor(Site), group = Site)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Year, ncol = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = "TOC (mg/L)", color = "Site") +
  theme_bw() +
  theme(legend.position = "bottom")
p

#Okay lets do some summarizing because this is hard to wade through

TSS_by_site <- TSS %>%
  group_by(Year, Site) %>%
  summarise(Mean_C_mg_per_L = mean(C_mg_per_L, na.rm = TRUE), C_sd = sd(C_mg_per_L, na.rm = TRUE), Mean_TSS_mg_per_L = mean(TSS_mg_per_L, na.rm = TRUE), TSS_sd = sd(TSS_mg_per_L, na.rm = TRUE))

#Plot Mean TSS by site

TSS_long <- bind_rows(
  TSS_by_site |>
    transmute(Year, Site, Variable = "Carbon",
              Mean = Mean_C_mg_per_L, SD = C_sd),
  TSS_by_site |>
    transmute(Year, Site, Variable = "TSS",
              Mean = Mean_TSS_mg_per_L, SD = TSS_sd)
)
p <- TSS_long |>
  filter(Variable == "Carbon") |>
  ggplot(aes(x = factor(Site), y = Mean, fill = Site)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = pmax(Mean - SD, 0), ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.25, na.rm = TRUE) +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Site", y = "TOC (mg/L)", fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")
p

p <- TSS_long |>
  filter(Variable == "TSS") |>
  ggplot(aes(x = factor(Site), y = Mean, fill = Site)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = pmax(Mean - SD, 0), ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.25, na.rm = TRUE) +
  facet_wrap(~Year, ncol = 1) +
  labs(x = "Site", y = "TSS (mg/L)", fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")
p






#alright now see if there is a sesonal trend

TSS_by_Date <- TSS %>%
  mutate(yearMon = format(lubridate::ymd(Date_collected), "%Y-%m")) %>%
  group_by(yearMon, Year) %>%
  summarise(Mean_C_mg_per_L = mean(C_mg_per_L, na.rm = TRUE), C_sd = sd(C_mg_per_L, na.rm = TRUE), Mean_TSS_mg_per_L = mean(TSS_mg_per_L, na.rm = TRUE), TSS_sd = sd(TSS_mg_per_L, na.rm = TRUE))


TSS_long <- bind_rows(
  TSS_by_Date |>
    transmute(yearMon, Year, Variable = "Carbon",
              Mean = Mean_C_mg_per_L, SD = C_sd),
  TSS_by_Date |>
    transmute(yearMon, Year, Variable = "TSS",
              Mean = Mean_TSS_mg_per_L, SD = TSS_sd)
)

p <- ggplot(TSS_long, aes(x = yearMon, y = Mean, fill = Variable)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = pmax(Mean - SD, 0), ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.25, na.rm = TRUE) +
  facet_wrap(~Year, ncol = 1, scales = "free_x") +
  labs(x = NULL, y = "Concentration (mg/L)", fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")
p







#merge TSS and Q and see if they relate:

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

Q$Date <- as.Date(Q$Date, format = "%m/%d/%Y")

#create join column
Q$ID <- paste(Q$DataID, Q$Date)
TSS$ID <- paste(TSS$Site, TSS$Date_collected)



merged_TSS_Q <- left_join(TSS, Q, by = "ID")
merged_TSS_Q$Year <- factor(year(merged_TSS_Q$Date_collected))


p <- ggplot(merged_TSS_Q, aes(x = Q, y = TSS_mg_per_L,
                              color = Type, shape = Catchment)) +
  geom_point(size = 2, na.rm = TRUE) +
  labs(x = "Discharge (L/s)", y = "Total Suspended Solids (mg/L)", color = "Type", shape = "Catchment") +
  theme_bw() +
  facet_wrap(~Year)
p



p <- ggplot(merged_TSS_Q, aes(x = Q, y = TSS_mg_per_L,
                              fill = Type, shape = Catchment)) +
  geom_point(size = 3.5, alpha = 0.8, color = "grey20", stroke = 0.4,
             na.rm = TRUE) +
  facet_wrap(~Year, scales = "free") +
  scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
  scale_shape_manual(values = c(21, 22, 24, 23, 25, 8)) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(x = "Discharge (Q)", y = "TSS (mg/L)",
       fill = "Type", shape = "Catchment") +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4)),
         shape = guide_legend(override.aes = list(fill = "grey70", size = 4))) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.border = element_rect(color = "grey40"),
    strip.background = element_rect(fill = "grey92", color = "grey40"),
    strip.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key = element_blank(),
    panel.spacing = unit(1, "lines")
  )
p

ggsave("plots/TSS_vs_Q.png", p, width = 10, height = 5, dpi = 300)

p <- ggplot(merged_TSS_Q, aes(x = Q, y = C_mg_per_L,
                              fill = Type, shape = Catchment)) +
  geom_point(size = 3.5, alpha = 0.8, color = "grey20", stroke = 0.4,
             na.rm = TRUE) +
  facet_wrap(~Year, scales = "free") +
  scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
  scale_shape_manual(values = c(21, 22, 24, 23, 25, 8)) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(x = "Discharge (Q)", y = "TSS C (mg/L)",
       fill = "Type", shape = "Catchment") +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4)),
         shape = guide_legend(override.aes = list(fill = "grey70", size = 4))) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.border = element_rect(color = "grey40"),
    strip.background = element_rect(fill = "grey92", color = "grey40"),
    strip.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key = element_blank(),
    panel.spacing = unit(1, "lines")
  )
p

ggsave("plots/TOC_vs_Q.png", p, width = 10, height = 5, dpi = 300)

#What about POC vs Q in individual streams?

p <- ggplot(merged_TSS_Q, aes(x = Q, y = C_mg_per_L, color = Year)) +
  geom_point(size = 3.5, alpha = 0.8, stroke = 0.4, na.rm = TRUE) +
  facet_wrap(~Site, scales = "free")
p

p <- ggplot(merged_TSS_Q, aes(x = Q, y = TSS_mg_per_L, color = Year)) +
  geom_point(size = 3.5, alpha = 0.8, stroke = 0.4, na.rm = TRUE) +
  facet_wrap(~Site, scales = "free")
p

#Alrighty delete all the csvs we downloaded to clean up the repo before committing and pushing
# List all CSV files in the current working directory (repo root)
csv_files <- list.files(path = ".", pattern = "\\.csv$", full.names = TRUE)

# Remove them
file.remove(csv_files)

