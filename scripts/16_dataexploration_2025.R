#### Read me ####
# The following code is for exploratory data analysis of USFS discharge data for T003. 
# The file used - T003_Discharge.csv - is a dataset I created from the parent dataset
# provided by the Forest Service, titled "15min_discharge_gap_filled_wy2004-wy2015_working.xlsx",
# which contains stage and discharge data for all USFS monitoring streams. 

#### Libraries #### 
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("googledrive")
library(dplyr)
library(tidyr)
library(ggplot2)
library(googledrive) # Make sure Google Drive authentication is updated, otherwise the connection won't work.

#### Load data ####
drive_find(n_max = 10) 

# Download the desired file to the working directory
drive_download("USFS_streamsDischarge.csv", path = "USFS_streamsDischarge.csv", overwrite = TRUE)

#### Load data into R ####
usfs_q <- read.csv("USFS_streamsDischarge.csv")

# Create a dataset for T003 specifically
t003 <- usfs_q[,c(2,3,4,13,14)]

# Create a date-time column and change its format to POSIXct
t003$DateTime <- paste(t003$Day, t003$Time, sep = " ")
t003$DateTime <- as.POSIXct(t003$DateTime, format = "%Y-%m-%d %H:%M:%S")

#### Plot entire dataset ####
t003_plot <- ggplot(t003, aes(DateTime, T003_lps)) + geom_point(size = 0.125) +
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 800, by = 50)) +
  scale_x_datetime(name = "Year", date_breaks = "3 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_plot

#### Plot each year individually ####

# 2003
t003_03 <- t003[1:8832,]

t003_03_plot <- ggplot(t003_03, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 130, by = 10)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_03_plot

# 2004
t003_04 <- t003[8833:43968,]

t003_04_plot <- ggplot(t003_04, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 240, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_04_plot

# 2005
t003_05 <- t003[43969:79008,]

t003_05_plot <- ggplot(t003_05, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 700, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_05_plot

# 2006
t003_06 <- t003[79009:114048,]

t003_06_plot <- ggplot(t003_06, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 520, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_06_plot

# 2007
t003_07 <- t003[114049:149088,]

t003_07_plot <- ggplot(t003_07, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 80, by = 5)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_07_plot

# 2008
t003_08 <- t003[149089:184224,]

t003_08_plot <- ggplot(t003_08, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 140, by = 10)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_08_plot

# 2009
t003_09 <- t003[184225:219264,]

t003_09_plot <- ggplot(t003_09, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 800, by = 50)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_09_plot

# 2010
t003_10 <- t003[219265:254304,]

t003_10_plot <- ggplot(t003_10, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 400, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_10_plot

# 2011
t003_11 <- t003[254305:289344,]

t003_11_plot <- ggplot(t003_11, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 500, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_11_plot

# 2012
t003_12 <- t003[289345:324480,]

t003_12_plot <- ggplot(t003_12, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 400, by = 20)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_12_plot

# 2013
t003_13 <- t003[324481:359520,]

t003_13_plot <- ggplot(t003_13, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 100, by = 10)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_13_plot

# 2014
t003_14 <- t003[359521:394560,]

t003_14_plot <- ggplot(t003_14, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 80, by = 10)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_14_plot

# 2015
t003_15 <- t003[394561:420768,]

t003_15_plot <- ggplot(t003_15, aes(DateTime, T003_lps)) + geom_point(size = 0.125) + 
  scale_y_continuous(name = "Discharge (lps)", breaks = seq(0, 180, by = 10)) +
  scale_x_datetime(name = "Month", date_breaks = "1 month", date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 14, face = "bold")) +
  coord_cartesian(expand = TRUE)
t003_15_plot       

#### Save plots to Google Drive
plots <- list(t003_plot, t003_03_plot, t003_04_plot, t003_05_plot, t003_06_plot, t003_07_plot,
              t003_08_plot, t003_09_plot, t003_10_plot, t003_11_plot, t003_12_plot, t003_13_plot,
              t003_14_plot, t003_15_plot)

out_dir <- "plots"
dir.create(out_dir, showWarnings = FALSE)

# save to local folder
for (i in seq_along(plots)) {
  ggsave(
    filename = file.path(out_dir, paste0("plot_", i, ".png")),
    plot = plots[[i]],
    width = 8,
    height = 6,
    dpi = 300
  )
}

# get list of all files in your local folder
plot_files <- list.files("plots", full.names = TRUE)

# upload all of them
lapply(plot_files, function(f) {
  drive_upload(
    media = f,
    path = "https://drive.google.com/drive/u/0/folders/196iS6zSlwD81tlrSSNPkXF6Ie9r0Go_3",  # Google Drive folder
    name = basename(f)
  )
})

# clear the local folder we used to it can be used elsewhere
files <- list.files(path = "plot", full.names = TRUE)
file.remove(files)

## NOTE: This code to save the plots locally then to Google Drive didn't save them
# with the plot names, so I had to relabel the files in Google Drive. I tried to figure
# it out for awhile but couldn't quite get it...

#### Read me #### 
# This is as far as I took plotting USFS discharge data. What follows is the code 
# used to explore all of the available water data from the 2025 field season.

## Are the catchments true replicates? ##

#### Libraries ####
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("googledrive")
library(dplyr)
library(tidyr)
library(ggplot2)
library(googledrive) # Make sure Google Drive authentication is updated, otherwise the connection won't work.

#### Load data ####
# List recent files from Google Drive
drive_find(n_max = 10) 

# Download the desired file to the working directory
drive_download("2025_Q.csv", path = "2025_Q.csv", overwrite = TRUE)
drive_download("2025_TSS.csv", path = "2025_TSS.csv", overwrite = TRUE)
drive_download("2025_DOC_TDN.csv", path = "2025_DOC_TDN.csv", overwrite = TRUE)

# Load CSVs into R
q <- read.csv("2025_Q.csv")
tss <- read.csv("2025_TSS.csv")
doc_tdn <- read.csv("2025_DOC_TDN.csv")
head(data)

#### Probability distributions #### 

## Discharge ##
Q_hist <- ggplot(q[-c(121,122),], aes(x = Q)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, breaks = seq(0.005, 125, 0.5), fill = "lightblue", color = "black") +
  geom_density(color = "red", linewidth = 0.5) +
  labs(title = "Pre-fire Distribution of Discharge",
       x = "Discharge (L/s)",
       y = "Density") +
  theme_minimal()
Q_hist

shapiro.test((q$Q))
# p-value = 3.64e-14 ---> Data is not normally distributed.

# Q-Q plot
Q_qq <- ggplot(q, aes(sample = Q)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
Q_qq

## Specific conductance ## 
SpC_hist <- ggplot(q[-c(121,122),], aes(x = medianSpC)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, breaks = seq(5, 80, 0.25), fill = "lightblue", color = "black") +
  geom_density(color = "red", linewidth = 0.5) +
  labs(title = "Pre-fire Distribution of SpC",
       x = "Specific Conductance (uS/cm)",
       y = "Density") +
  theme_minimal()
SpC_hist

shapiro.test((q$medianSpC))
# p-value = 1.867e-10 ---> Data is not normally distributed.

# Q-Q plot
SpC_qq <- ggplot(q, aes(sample = medianSpC)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
SpC_qq

## Total suspended solids ##
TSS_hist <- ggplot(tss[-c(127:133),], aes(x = TSS_by_Vol)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, breaks = seq(-0.00004, 0.0002, 0.000004), fill = "lightblue", color = "black") +
  geom_density(color = "red", linewidth = 0.5) +
  labs(title = "Pre-fire Distribution of TSS",
       x = "Total Suspended Solids (mg/L)",
       y = "Density") +
  theme_minimal()
TSS_hist

shapiro.test((tss$TSS_by_Vol))
# p-value = 2.2e-16 ---> Data is not normally distributed.

# Q-Q plot 
TSS_qq <- ggplot(tss, aes(sample = TSS_105)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
TSS_qq

## Carbon content ##
C_hist <- ggplot(tss[-c(127:133),], aes(x = C_Content)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, breaks = seq(0, 0.075, 0.0005), fill = "lightblue", color = "black") +
  geom_density(color = "red", linewidth = 0.5) +
  labs(title = "Pre-fire Distribution of Carbon",
       x = "Carbon Content (mg/L)",
       y = "Density") +
  theme_minimal()
C_hist

shapiro.test((tss$C_Content))
# p-value = 2.2e-16 ---> Data is not normally distributed.

# Q-Q plot
C_qq <- ggplot(tss, aes(sample = C_Content)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
C_qq

## Dissolved organic carbon ##
DOC_hist <- ggplot(doc_tdn[-c(152:159),], aes(x = NPOC)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, breaks = seq(0, 5.75, 0.025), fill = "lightblue", color = "black") +
  geom_density(color = "red", linewidth = 0.5) +
  labs(title = "Pre-fire Distribution of DOC",
       x = "Dissolved Organic Carbon (mg/L)",
       y = "Density") +
  theme_minimal()
DOC_hist

shapiro.test((doc_tdn$NPOC))
# p-value = 4.103e-08 ---> Data is not normally distributed.

# Q-Q plot
DOC_qq <- ggplot(doc_tdn, aes(sample = NPOC)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
DOC_qq

## Total dissolved nitrogen ##
TDN_hist <- ggplot(doc_tdn[-c(152:159),], aes(x = TN)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, breaks = seq(0, 0.25, 0.00125), fill = "lightblue", color = "black") +
  geom_density(color = "red", linewidth = 0.5) +
  labs(title = "Pre-fire Distribution of TN",
       x = "Total Dissolved Nitrogen (mg/L)",
       y = "Density") +
  theme_minimal()
TDN_hist

shapiro.test((doc_tdn$TN))
# p-value = 1.549e-11 ---> Data is not normally distributed.

# Q-Q plot
TDN_qq <- ggplot(doc_tdn, aes(sample = TN)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "TDN Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
TDN_qq

#### Save plots to Google Drive ####
# clear the local folder we used to it can be used elsewhere
files <- list.files(path = "plot", full.names = TRUE)
file.remove(files)

plots <- list(Q_hist, Q_qq, SpC_hist, SpC_qq, TSS_hist, TSS_qq, DOC_hist, DOC_qq,
              TDN_hist, TDN_qq, C_hist, C_qq)

out_dir <- "plots"
dir.create(out_dir, showWarnings = FALSE)

# save to local folder
for (i in seq_along(plots)) {
  ggsave(
    filename = file.path(out_dir, paste0("plot_", i, ".png")),
    plot = plots[[i]],
    width = 8,
    height = 6,
    dpi = 300
  )
}

# get list of all files in your local folder
plot_files <- list.files("plots", full.names = TRUE)

# upload all of them
lapply(plot_files, function(f) {
  drive_upload(
    media = f,
    path = "https://drive.google.com/drive/u/0/folders/1uBIZ4ZZxo1TBz2vp_jYnw754-tfsCt4O",  # Google Drive folder
    name = basename(f)
  )
})


#### Preliminary plots ####

#### Discharge ####
## PRE-FIRE ##
# By catchment + outlets (weekly monitoring)
Q_by_catchment <- ggplot(q, aes(x = Catchment, y = Q, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-fire Discharge Distributions by Catchment",
       x = "Catchment",
       y = "Q (L/s)") +
  theme_minimal()
Q_by_catchment

# By campaign with weekly monitoring and October data removed
Q_by_campaign <- ggplot(q[-c(1:18,121,122),], aes(x = Campaign, y = Q, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire Discharge Distributions by Campaign",
       x = "Campaign",
       y = "Q (L/s)") +
  theme_minimal()
Q_by_campaign

# By site/catchment
# TECR
tecr_data <- q[grepl("TECR", q$DataID),]

Q_by_site_tecr <- ggplot(tecr_data, aes(x = DataID, y = Q)) +
  geom_boxplot() +
  labs(title = "Pre-fire Discharge Distributions by Site: TECR",
       x = "Site",
       y = "Q (L/s)") +
  theme_minimal()
Q_by_site_tecr # Highlight tributaries vs. mainstem sites

# TEAK 
teak_data <- q[grepl("TEAK", q$DataID),]

Q_by_site_teak <- ggplot(teak_data[-c(122),], aes(x = DataID, y = Q)) +
  geom_boxplot() +
  labs(title = "Pre-fire Discharge Distributions by Site: TEAK",
       x = "Site",
       y = "Q (L/s)") +
  theme_minimal()
Q_by_site_teak # Highlight tributaries vs. mainstem sites

## Pre- vs. post-fire (preliminary) ##
# Q for TEAK01 and OUTLET; no post-fire data (yet) for TECR01

# OUTLET
outlet_pre.post_fire_data <- q[grepl("OUT", q$DataID),]

Q_pre.post_fire_out <- ggplot(outlet_pre.post_fire_data, aes(x = Date, y = Q)) +
  geom_point() +
  labs(title = "Monthly Discharge at the Outlet",
       x = "Sampling Date",
       y = "Q (L/s)") +
  theme_minimal()
Q_pre.post_fire_out  

# TEAK
teak01_pre.post_fire_data <- q[grepl("TEAK01", q$DataID),]

Q_pre.post_fire_teak01 <- ggplot(teak01_pre.post_fire_data, aes(x = Date, y = Q)) +
  geom_point() +
  labs(title = "Discharge by Sampling Date at TEAK01",
       x = "Sampling Date",
       y = "Q (L/s)") +
  theme_minimal()
Q_pre.post_fire_teak01

# TECR
tecr01_pre_fire_data <- q[grepl("TECR01", q$DataID),]

Q_pre_fire_tecr01 <- ggplot(tecr01_pre_fire_data, aes(x = Date, y = Q)) +
  geom_point() +
  labs(title = "Discharge by Sampling Date at TECR01",
       x = "Sampling Date",
       y = "Q (L/s") +
  theme_minimal()
Q_pre_fire_tecr01

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(Q_by_campaign, Q_by_catchment, Q_by_site_teak, Q_by_site_tecr,
              Q_pre_fire_tecr01, Q_pre.post_fire_out, Q_pre.post_fire_teak01)

out_dir <- "plots"
dir.create(out_dir, showWarnings = FALSE)

# save to local folder
for (i in seq_along(plots)) {
  ggsave(
    filename = file.path(out_dir, paste0("plot_", i, ".png")),
    plot = plots[[i]],
    width = 8,
    height = 6,
    dpi = 300
  )
}

# get list of all files in your local folder
plot_files <- list.files("plots", full.names = TRUE)

# upload all of them
lapply(plot_files, function(f) {
  drive_upload(
    media = f,
    path = "https://drive.google.com/drive/u/0/folders/15z1n94zM79YSq88CfQvgIBYdF7kUh796",  # Google Drive folder
    name = basename(f)
  )
})

#### Specific conductance ####
## PRE-FIRE
# By catchment + outlets (i.e., weekly monitoring)
SpC_by_catchment <- ggplot(q[-c(121,122),], aes(x = Catchment, y = medianSpC, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-fire SpC Distributions by Catchment (excluding October)",
       x = "Catchment",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_by_catchment

# By campaign with weekly monitoring and October data removed
SpC_by_campaign <- ggplot(q[-c(1:18,121,122),], aes(x = Campaign, y = medianSpC, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire SpC Distributions by Campaign (weekly monitoring removed)",
       x = "Campaign",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_by_campaign

# By site/catchment
# TECR 
tecr_data <- q[grepl("TECR", q$DataID),]

SpC_by_site_tecr <- ggplot(tecr_data, aes(x = DataID, y = medianSpC)) +
  geom_boxplot() +
  labs(title = "Pre-fire SpC Distributions by Site: TECR",
       x = "Site",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_by_site_tecr # Highlight tributaries vs. mainstem sites

# TEAK 
teak_data <- q[grepl("TEAK", q$DataID),]

SpC_by_site_teak <- ggplot(teak_data[-c(122),], aes(x = DataID, y = medianSpC)) +
  geom_boxplot() +
  labs(title = "Pre-fire SpC Distributions by Site: TEAK",
       x = "Site",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_by_site_teak # Highlight tributaries vs. mainstem sites

## Pre- vs. post-fire (preliminary) ##
# SpC for TEAK01 and OUTLET; no post-fire data (yet) for TECR01

# OUTLET
outlet_pre.post_fire_data <- q[grepl("OUT", q$DataID),]

SpC_pre.post_fire_out <- ggplot(outlet_pre.post_fire_data, aes(x = Date, y = medianSpC)) +
  geom_point() +
  labs(title = "Monthly SpC at the Outlet",
       x = "Sampling Date",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_pre.post_fire_out  

# TEAK 
teak01_pre.post_fire_data <- q[grepl("TEAK01", q$DataID),]

SpC_pre.post_fire_teak01 <- ggplot(teak01_pre.post_fire_data, aes(x = Date, y = medianSpC)) +
  geom_point() +
  labs(title = "SpC by Sampling Date at TEAK01",
       x = "Sampling Date",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_pre.post_fire_teak01

# TECR
tecr01_pre_fire_data <- q[grepl("TECR01", q$DataID),]

SpC_pre_fire_tecr01 <- ggplot(tecr01_pre_fire_data, aes(x = Date, y = medianSpC)) +
  geom_point() +
  labs(title = "SpC by Sampling Date at TECR01",
       x = "Sampling Date",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_pre_fire_tecr01

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(SpC_by_campaign, SpC_by_catchment, SpC_by_site_teak, SpC_by_site_tecr,
              SpC_pre_fire_tecr01, SpC_pre.post_fire_out, SpC_pre.post_fire_teak01)

out_dir <- "plots"
dir.create(out_dir, showWarnings = FALSE)

# save to local folder
for (i in seq_along(plots)) {
  ggsave(
    filename = file.path(out_dir, paste0("plot_", i, ".png")),
    plot = plots[[i]],
    width = 8,
    height = 6,
    dpi = 300
  )
}

# get list of all files in your local folder
plot_files <- list.files("plots", full.names = TRUE)

# upload all of them
lapply(plot_files, function(f) {
  drive_upload(
    media = f,
    path = "https://drive.google.com/drive/u/0/folders/1df_cNDXNCGVj5k8298yybeT75Dgh9EgV",  # Google Drive folder
    name = basename(f)
  )
})

#### Dissolved organic carbon ####
## PRE-FIRE ##
# By catchment with weekly monitoring and October data removed
DOC_by_catchment <- ggplot(doc_tdn[-c(152:159),], aes(x = Catchment, y = NPOC, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-fire DOC Distributions by Catchment",
       x = "Catchment",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_by_catchment

# By campaign with weekly monitoring and October data removed
DOC_by_campaign <- ggplot(doc_tdn[-c(132:151, 152:159),], aes(x = Campaign, y = NPOC, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire DOC Distributions by Campaign (weekly monitoring data removed)",
       x = "Campaign",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_by_campaign

# By site/catchment
# TECR
tecr_data <- doc_tdn[grepl("TECR", doc_tdn$Site),]

DOC_by_site_tecr <- ggplot(tecr_data[-c(154,156,158)], aes(x = Site, y = NPOC)) +
  geom_boxplot() +
  labs(title = "Pre-fire DOC Distributions by Site: TECR",
       x = "Site",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_by_site_tecr

# TEAK
teak_data <- doc_tdn[grepl("TEAK", doc_tdn$Site),]

DOC_by_site_teak <- ggplot(teak_data[-c(152,153,155,159),], aes(x = Site, y = NPOC)) +
  geom_boxplot() +
  labs(title = "Pre-fire DOC Distributions by Site: TEAK",
       x = "Site",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_by_site_teak

## Pre- vs. post-fire ##
# DOC for OUTLET and TEAK01; no post-fire data (yet) for TECR01

# OUTLET
outletDOC_pre.post_fire_data <- doc_tdn[grepl("OUT", doc_tdn$Site),]

DOC_pre.post_fire_out <- ggplot(outletDOC_pre.post_fire_data, aes(x = Date, y = NPOC)) +
  geom_point(size = 2) +
  labs(title = "DOC Concentrations by Sampling Date: OUTLET",
       x = "Sampling Date",
       y = "DOC (mg/L") +
  theme_minimal()
DOC_pre.post_fire_out

# TEAK01
teak01DOC_pre.post_fire_data <- doc_tdn[grepl("TEAK01", doc_tdn$Site),]

DOC_pre.post_fire_teak01 <- ggplot(teak01DOC_pre.post_fire_data, aes(x = Date, y = NPOC)) +
  geom_point(size = 2) +
  labs(title = "DOC Concentrations by Sampling Date: TEAK01",
       x = "Sampling Date",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_pre.post_fire_teak01

# TECR01
tecr01DOC_pre_fire_data <- doc_tdn[grepl("TECR01", doc_tdn$Site),]

DOC_pre_fire_tecr01 <- ggplot(tecr01DOC_pre_fire_data, aes(x = Date, y = NPOC)) +
  geom_point(size = 2) +
  labs(title = "DOC Concentrations by Sampling Date: TECR01",
       x = "Sampling Date",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_pre_fire_tecr01

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(DOC_by_campaign, DOC_by_catchment, DOC_by_site_teak, DOC_by_site_tecr,
              DOC_pre_fire_tecr01, DOC_pre.post_fire_out, DOC_pre.post_fire_teak01)

out_dir <- "plots"
dir.create(out_dir, showWarnings = FALSE)

# save to local folder
for (i in seq_along(plots)) {
  ggsave(
    filename = file.path(out_dir, paste0("plot_", i, ".png")),
    plot = plots[[i]],
    width = 8,
    height = 6,
    dpi = 300
  )
}

# get list of all files in your local folder
plot_files <- list.files("plots", full.names = TRUE)

# upload all of them
lapply(plot_files, function(f) {
  drive_upload(
    media = f,
    path = "https://drive.google.com/drive/u/0/folders/16wltLJzgrmBO7L1ALRKmO_YwX8OAZ1t8",  # Google Drive folder
    name = basename(f)
  )
})

#### Total nitrogen ####
## PRE-FIRE ##
# By catchment with October data removed
TN_by_catchment <- ggplot(doc_tdn[-c(152:159),], aes(x = Catchment, y = TN, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-fire TN Distributions by Catchment (excluding October)",
       x = "Catchment",
       y = "TN (mg/L)") +
  theme_minimal()
TN_by_catchment

# By campaign with weekly monitoring and October data removed
TN_by_campaign <- ggplot(doc_tdn[-c(132:151, 152:159),], aes(x = Campaign, y = TN, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire TN Distributions by Campaign (weekly monitoring data removed)",
       x = "Campaign",
       y = "TN (mg/L)") +
  theme_minimal()
TN_by_campaign

# By site/catchment
# TECR
tecr_data <- doc_tdn[grepl("TECR", doc_tdn$Site),]

TN_by_site_tecr <- ggplot(tecr_data[-c(154,156,158)], aes(x = Site, y = TN)) +
  geom_boxplot() +
  labs(title = "Pre-fire TN Distributions by Site: TECR",
       x = "Site",
       y = "TN (mg/L)") +
  theme_minimal()
TN_by_site_tecr

# TEAK 
teak_data <- doc_tdn[grepl("TEAK", doc_tdn$Site),]

TN_by_site_teak <- ggplot(teak_data[-c(152,153,155,159),], aes(x = Site, y = TN)) +
  geom_boxplot() +
  labs(title = "Pre-fire TN Distributions by Site: TEAK",
       x = "Site",
       y = "TN (mg/L)") +
  theme_minimal()
TN_by_site_teak

## Pre- vs. post-fire ##
# TN for OUTLET and TEAK01; no data (yet) for TECR01

# OUTLET
outletTN_pre.post_fire_data <- doc_tdn[grepl("OUT", doc_tdn$Site),]

TN_pre.post_fire_out <- ggplot(outletTN_pre.post_fire_data, aes(x = Date, y = TN)) +
  geom_point(size = 2) +
  labs(title = "TN Concentrations by Sampling Date: OUTLET",
       x = "Sampling Date",
       y = "TN (mg/L") +
  theme_minimal()
TN_pre.post_fire_out

# TEAK01
teak01TN_pre.post_fire_data <- doc_tdn[grepl("TEAK01", doc_tdn$Site),]

TN_pre.post_fire_teak01 <- ggplot(teak01TN_pre.post_fire_data, aes(x = Date, y = TN)) +
  geom_point(size = 2) +
  labs(title = "TN Concentrations by Sampling Date: TEAK01",
       x = "Sampling Date",
       y = "TN (mg/L)") +
  theme_minimal()
TN_pre.post_fire_teak01

# TECR01
tecr01TN_pre_fire_data <- doc_tdn[grepl("TECR01", doc_tdn$Site),]

TN_pre_fire_tecr01 <- ggplot(tecr01TN_pre_fire_data, aes(x = Date, y = TN)) +
  geom_point(size = 2) +
  labs(title = "TN Concentrations by Sampling Date: TECR01",
       x = "Sampling Date",
       y = "TN (mg/L)") +
  theme_minimal()
TN_pre_fire_tecr01

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(TN_by_campaign, TN_by_catchment, TN_by_site_teak, TN_by_site_tecr,
              TN_pre_fire_tecr01, TN_pre.post_fire_out, TN_pre.post_fire_teak01)

out_dir <- "plots"
dir.create(out_dir, showWarnings = FALSE)

# save to local folder
for (i in seq_along(plots)) {
  ggsave(
    filename = file.path(out_dir, paste0("plot_", i, ".png")),
    plot = plots[[i]],
    width = 8,
    height = 6,
    dpi = 300
  )
}

# get list of all files in your local folder
plot_files <- list.files("plots", full.names = TRUE)

# upload all of them
lapply(plot_files, function(f) {
  drive_upload(
    media = f,
    path = "https://drive.google.com/drive/u/0/folders/1rxRG7IdnFwYalhs8Rdxhun_RLYWmb2nr",  # Google Drive folder
    name = basename(f)
  )
})

#### Total suspended solids ####
## PRE-FIRE ##
# By catchment with weekly monitoring data
TSS_by_catchment <- ggplot(tss[-c(127:133),], aes(x = Catchment, y = TSS_by_Vol, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-Fire TSS Distributions by Catchment (excluding October)",
       x = "Catchment",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_by_catchment

# By campaign with weekly monitoring data removed
TSS_by_campaign <- ggplot(tss[-c(25:28,57:62,91:98,127:133),], aes(x = Campaign, y = TSS_by_Vol, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire TSS Distributions by Campaign (weekly monitoring removed)",
       x = "Campaign",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_by_campaign

# By site/catchment
# TECR
tecr_data <- tss[grepl("TECR", tss$Site),]

TSS_by_site_tecr <- ggplot(tecr_data[-c(131:133),], aes(x = Site, y = TSS_by_Vol)) +
  geom_boxplot() +
  labs(title = "Pre-fire TSS Distributions by Site: TECR",
       x = "Site",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_by_site_tecr

# TEAK 
teak_data <- tss[grepl("TEAK", tss$Site),]

TSS_by_site_teak <- ggplot(teak_data[-c(128:130),], aes(x = Site, y = TSS_by_Vol)) +
  geom_boxplot() +
  labs(title = "Pre-Fire TSS Distributions by Site: TEAK",
       x = "Site",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_by_site_teak

## Pre- vs. post-fire ##
# TSS for OUTLET and TEAK01; no data (yet) for TECR01

# OUTLET
outletTSS_pre.post_fire_data <- tss[grepl("OUT", tss$Site),]

TSS_pre.post_fire_out <- ggplot(outletTSS_pre.post_fire_data, aes(x = Date, y = TSS_by_Vol)) +
  geom_point(size = 2) +
  labs(title = "TSS Concentrations by Sampling Date: OUTLET",
       x = "Sampling Date",
       y = "TS (mg/L)") +
  theme_minimal()
TSS_pre.post_fire_out

# TEAK01
teak01TSS_pre.post_fire_data <- tss[grepl("TEAK01", tss$Site),]

TSS_pre.post_fire_teak01 <- ggplot(teak01TSS_pre.post_fire_data, aes(x = Date, y = TSS_by_Vol)) +
  geom_point(size = 2) +
  labs(title = "TSS Concentrations by Sampling Date: TEAK01",
       x = "Sampling Date",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_pre.post_fire_teak01

# TECR01
tecr01TSS_pre_fire_data <- tss[grepl("TECR01", tss$Site),]

TSS_pre_fire_tecr01 <- ggplot(tecr01TSS_pre_fire_data, aes(x = Date, y = TSS_by_Vol)) +
  geom_point(size = 2) +
  labs(title = "TSS Concentrations by Sampling Date: TECR01",
       x = "Sampling Date",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_pre_fire_tecr01

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(TSS_by_campaign, TSS_by_catchment, TSS_by_site_teak, TSS_by_site_tecr,
              TSS_pre_fire_tecr01, TSS_pre.post_fire_out, TSS_pre.post_fire_teak01)

out_dir <- "plots"
dir.create(out_dir, showWarnings = FALSE)

# save to local folder
for (i in seq_along(plots)) {
  ggsave(
    filename = file.path(out_dir, paste0("plot_", i, ".png")),
    plot = plots[[i]],
    width = 8,
    height = 6,
    dpi = 300
  )
}

# get list of all files in your local folder
plot_files <- list.files("plots", full.names = TRUE)

# upload all of them
lapply(plot_files, function(f) {
  drive_upload(
    media = f,
    path = "https://drive.google.com/drive/u/0/folders/1EuHvKc7UZG2BJzZP9MP6CGEhg9W4YtLA",  # Google Drive folder
    name = basename(f)
  )
})

#### Carbon ####
## PRE-FIRE ##
# By catchment with weekly monitoring data
C_by_catchment <- ggplot(tss[-c(127:133),], aes(x = Catchment, y = C_Content, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-Fire C Distributions by Catchment (excluding October)",
       x = "Catchment",
       y = "Carbon (mg/L)") +
  theme_minimal()
C_by_catchment

# By campaign with weekly monitoring data removed
C_by_campaign <- ggplot(tss[-c(25:28,57:62,91:98, 127:133),], aes(x = Campaign, y = C_Content, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire C Distributions by Campaign (weekly monitoring removed)",
       x = "Campaign",
       y = "Carbon (mg/L)") +
  theme_minimal()
C_by_campaign

# By site/catchment
# TECR
tecr_data <- tss[grepl("TECR", tss$Site),]

C_by_site_tecr <- ggplot(tecr_data[-c(131:133),], aes(x = Site, y = C_Content)) +
  geom_boxplot() +
  labs(title = "Pre-fire C Distributions by Site: TECR",
       x = "Site",
       y = "C (mg/L)") +
  theme_minimal()
C_by_site_tecr

# TEAK
teak_data <- tss[grepl("TEAK", tss$Site),]

C_by_site_teak <- ggplot(teak_data[-c(128:130),], aes(x = Site, y = C_Content)) +
  geom_boxplot() +
  labs(title = "Pre-Fire C Distributions by Site: TEAK",
       x = "Site",
       y = "C (mg/L)") +
  theme_minimal()
C_by_site_teak

## Pre- vs. post-fire ##
# TSS for OUTLET and TEAK01; no data (yet) for TECR01
# OUTLET
outletC_pre.post_fire_data <- tss[grepl("OUT", tss$Site),]

C_pre.post_fire_out <- ggplot(outletC_pre.post_fire_data, aes(x = Date, y = C_Content)) +
  geom_point(size = 2) +
  labs(title = "C Concentrations by Sampling Date: OUTLET",
       x = "Sampling Date",
       y = "C (mg/L)") +
  theme_minimal()
C_pre.post_fire_out

# TEAK01
teak01C_pre.post_fire_data <- tss[grepl("TEAK01", tss$Site),]

C_pre.post_fire_teak01 <- ggplot(teak01C_pre.post_fire_data, aes(x = Date, y = C_Content)) +
  geom_point(size = 2) +
  labs(title = "C Concentrations by Sampling Date: TEAK01",
       x = "Sampling Date",
       y = "C (mg/L)") +
  theme_minimal()
C_pre.post_fire_teak01

# TECR01
tecr01C_pre.post_fire_data <- tss[grepl("TECR01", tss$Site),]

C_pre.post_fire_tecr01 <- ggplot(tecr01C_pre.post_fire_data, aes(x = Date, y = C_Content)) +
  geom_point(size = 2) +
  labs(title = "C Concentrations by Sampling Date: TECR01",
       x = "Sampling Date",
       y = "C (mg/L)") +
  theme_minimal()
C_pre.post_fire_tecr01

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(C_by_campaign, C_by_catchment, C_by_site_teak, C_by_site_tecr,
              C_pre.post_fire_tecr01, C_pre.post_fire_out, C_pre.post_fire_teak01)

out_dir <- "plots"
dir.create(out_dir, showWarnings = FALSE)

# save to local folder
for (i in seq_along(plots)) {
  ggsave(
    filename = file.path(out_dir, paste0("plot_", i, ".png")),
    plot = plots[[i]],
    width = 8,
    height = 6,
    dpi = 300
  )
}

# get list of all files in your local folder
plot_files <- list.files("plots", full.names = TRUE)

# upload all of them
lapply(plot_files, function(f) {
  drive_upload(
    media = f,
    path = "https://drive.google.com/drive/u/0/folders/1qT5vvJ49L0aeRWYjgiDr1MZlXTHHIa9p",  # Google Drive folder
    name = basename(f)
  )
})