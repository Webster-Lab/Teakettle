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
