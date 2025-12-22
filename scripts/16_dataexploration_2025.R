#####################################################
# USFS Discharge Data for T003 - WY 2004 to present #
#####################################################

install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(ggplot2)

# raw data
t003_Q <- read.csv("Teakettle Discharge Data.csv")

# refined data
t003_Qr <- t003_Q[,c(2,3,13)]
t003_Qr$Date_time <- paste(t003_Qr$Day, t003_Qr$Time)
t003_Qr$Date_time <- as.POSIXct(t003_Qr$Date_time, format = "%Y-%m-%d %H:%M:%S")

# create plots for each year, then format together into one figure
t003_03 <- t003_Qr[1:8832,]
t003_04 <- t003_Qr[8833:43968,]
t003_05 <- t003_Qr[43969:79008,]
t003_06 <- t003_Qr[79009:114048,]
t003_07 <- t003_Qr[114049:149088,]
t003_08 <- t003_Qr[149089:184224,]
t003_09 <- t003_Qr[184225:219264,]
t003_10 <- t003_Qr[219265:254304,]
t003_11 <- t003_Qr[254305:289344,]
t003_12 <- t003_Qr[289345:324480,]
t003_13 <- t003_Qr[324481:359520,]
t003_14 <- t003_Qr[359521:394560,]
t003_15 <- t003_Qr[394561:420768,]
       
# 2003 
t003_03_plot <- ggplot(t003_03, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,130,by=10)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_03_plot

# 2004 
t003_04_plot <- ggplot(t003_04, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,230,by=20)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_04_plot

# 2005 
t003_05_plot <- ggplot(t003_05, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,700,by=50)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_05_plot

# 2006 
t003_06_plot <- ggplot(t003_06, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,700,by=50)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_06_plot

# 2007 
t003_07_plot <- ggplot(t003_07, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,100,by=5)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_07_plot

# 2008 
t003_08_plot <- ggplot(t003_08, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,150,by=10)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_08_plot

# 2009 
t003_09_plot <- ggplot(t003_09, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,800,by=50)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_09_plot

# 2010
t003_10_plot <- ggplot(t003_10, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,800,by=50)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_10_plot

# 2011
t003_11_plot <- ggplot(t003_11, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,800,by=50)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_11_plot

# 2012
t003_12_plot <- ggplot(t003_12, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,800,by=50)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_12_plot

# 2013
t003_13_plot <- ggplot(t003_13, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,800,by=10)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_13_plot

# 2014
t003_14_plot <- ggplot(t003_14, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,800,by=10)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_14_plot

# 2015
t003_15_plot <- ggplot(t003_15, aes(Date_time, T003_lps)) + geom_line() + 
  scale_y_continuous(name="Discharge (lps)", breaks=seq(0,800,by=10)) +
  scale_x_datetime(name="Month", date_breaks="1 month", date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=11), 
        axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=16, face="bold")) +
  coord_cartesian(expand=TRUE)
t003_15_plot

# Combine Plots into One Graphic
par(mfrow = c(6,2))
t003_04_plot
t003_05_plot
t003_06_plot
t003_07_plot
t003_08_plot
t003_09_plot
t003_10_plot
t003_11_plot
t003_12_plot
t003_13_plot
t003_14_plot
t003_15_plot

# ggplot 
# t003_gplot_total <-  ggplot(t003_final[1:8832,], aes(time, q)) + geom_line() + 
  # scale_y_continuous(name="Discharge (lps)", breaks=seq(0,800,by=50)) +
  # scale_x_date(name="Month", date_breaks="1 month") + 
  # theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10), 
      # axis.title.x=element_text(size=16, face="bold"),
      # axis.text.y=element_text(size=10),
      # axis.title.y=element_text(size=16, face="bold")) +
  # coord_cartesian(expand=FALSE)
# t003_gplot_total



###########################################################
## Data Exploration: Are the catchments true replicates? ##
###########################################################

install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("googledrive")
library(dplyr)
library(tidyr)
library(ggplot2)
library(googledrive) # Make sure Google Drive authentication is updated, otherwise the connection won't work.

# List recent files from Google Drive
drive_find(n_max = 10) 

# Download the desired file to the working directory
drive_download("2025_Q_all.csv", path = "2025_Q_all.csv", overwrite = TRUE)
drive_download("2025_TSS.csv", path = "2025_TSS.csv", overwrite = TRUE)
drive_download("2025_DOC_TDN.csv", path = "2025_DOC_TDN.csv", overwrite = TRUE)

# Load CSVs into R
q <- read.csv("2025_Q.csv")
tss <- read.csv("2025_TSS.csv")
doc_tdn <- read.csv("2025_DOC_TDN.csv")
head(data)



############################################
#### PRE-FIRE PROBABILITY DISTRIBUTIONS #### 
############################################

###################
#### Discharge ####
## Histogram ##
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

## Q-Q Plot ##
Q_qq <- ggplot(q, aes(sample = Q)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
Q_qq

##############################
#### Specific Conductance ####
## Histogram ##
SpC_hist <- ggplot(q[-c(121,122),], aes(x = medianSPC.x)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, breaks = seq(5, 80, 0.25), fill = "lightblue", color = "black") +
  geom_density(color = "red", linewidth = 0.5) +
  labs(title = "Pre-fire Distribution of SpC",
       x = "Specific Conductance (uS/cm)",
       y = "Density") +
  theme_minimal()
SpC_hist

shapiro.test((q$medianSPC.x))
# p-value = 1.867e-10 ---> Data is not normally distributed.

## Q-Q Plot ##
SpC_qq <- ggplot(q, aes(sample = medianSPC.x)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
SpC_qq

################################
#### Total Suspended Solids ####
## Histogram ##
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

## Q-Q Plot ##
TSS_qq <- ggplot(tss, aes(sample = TSS_105)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
TSS_qq

########################
#### Carbon Content ####
## Histogram ##
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

## Q-Q Plot ##
C_qq <- ggplot(tss, aes(sample = C_Content)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
C_qq

##################################
#### Dissolved Organic Carbon ####
## Histogram ##
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

## Q-Q Plot ##
DOC_qq <- ggplot(doc_tdn, aes(sample = NPOC)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
DOC_qq

##################################
#### Total Dissolved Nitrogen ####
## Histogram ##
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

## Q-Q Plot ##
TDN_qq <- ggplot(doc_tdn, aes(sample = TN)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "TDN Q-Q Plot for Normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
TDN_qq



###################
###### PLOTS ######
###################

###############
## Discharge ##
###############

##################
#### PRE-FIRE ####
## By catchment + outlets (weekly monitoring)
Q_by_catchment <- ggplot(q, aes(x = Catchment, y = Q, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-fire Discharge Distributions by Catchment",
       x = "Catchment",
       y = "Q (L/s)") +
  theme_minimal()
Q_by_catchment

## By campaign with weekly monitoring and October data removed
Q_by_campaign <- ggplot(q[-c(1:18,121,122),], aes(x = Campaign, y = Q, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire Discharge Distributions by Campaign",
       x = "Campaign",
       y = "Q (L/s)") +
  theme_minimal()
Q_by_campaign

## By site/catchment
## TECR ##
tecr_data <- q[grepl("TECR", q$DataID),]

Q_by_site_tecr <- ggplot(tecr_data, aes(x = DataID, y = Q)) +
  geom_() +
  labs(title = "Pre-fire Discharge Distributions by Site: TECR",
       x = "Site",
       y = "Q (L/s)") +
  theme_minimal()
Q_by_site_tecr # Highlight tributaries vs. mainstem sites

## TEAK ##
teak_data <- q[grepl("TEAK", q$DataID),]

Q_by_site_teak <- ggplot(teak_data[-c(122),], aes(x = DataID, y = Q)) +
  geom_boxplot() +
  labs(title = "Pre-fire Discharge Distributions by Site: TEAK",
       x = "Site",
       y = "Q (L/s)") +
  theme_minimal()
Q_by_site_teak # Highlight tributaries vs. mainstem sites

#########################
#### PRE & POST-FIRE ####
## Q for TEAK01 and OUTLET; only pre-fire data for TECR01
## OUTLET ##
outlet_pre.post_fire_data <- q[grepl("OUT", q$DataID),]

Q_pre.post_fire_out <- ggplot(outlet_pre.post_fire_data, aes(x = Date, y = Q)) +
  geom_point() +
  labs(title = "Monthly Discharge at the Outlet",
       x = "Sampling Date",
       y = "Q (L/s)") +
  theme_minimal()
Q_pre.post_fire_out  

## TEAK ##
teak01_pre.post_fire_data <- q[grepl("TEAK01", q$DataID),]

Q_pre.post_fire_teak01 <- ggplot(teak01_pre.post_fire_data, aes(x = Date, y = Q)) +
  geom_point() +
  labs(title = "Discharge by Sampling Date at TEAK01",
       x = "Sampling Date",
       y = "Q (L/s)") +
  theme_minimal()
Q_pre.post_fire_teak01

## TECR ##
tecr01_pre_fire_data <- q[grepl("TECR01", q$DataID),]

Q_pre_fire_tecr01 <- ggplot(tecr01_pre_fire_data, aes(x = Date, y = Q)) +
  geom_point() +
  labs(title = "Discharge by Sampling Date at TECR01",
       x = "Sampling Date",
       y = "Q (L/s") +
  theme_minimal()
Q_pre_fire_tecr01


##########################
## Specific Conductance ##
##########################

##################
#### PRE-FIRE ####
## By catchment + outlets (weekly monitoring)
SpC_by_catchment <- ggplot(q[-c(121,122),], aes(x = Catchment, y = medianSPC.x, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-fire SpC Distributions by Catchment (excluding October)",
       x = "Catchment",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_by_catchment

## By campaign with weekly monitoring and October data removed
SpC_by_campaign <- ggplot(q[-c(1:18,121,122),], aes(x = Campaign, y = medianSPC.x, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire SpC Distributions by Campaign (weekly monitoring removed)",
       x = "Campaign",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_by_campaign

## By site/catchment
## TECR ##
tecr_data <- q[grepl("TECR", q$DataID),]

SpC_by_site_tecr <- ggplot(tecr_data, aes(x = DataID, y = medianSPC.x)) +
  geom_boxplot() +
  labs(title = "Pre-fire SpC Distributions by Site: TECR",
       x = "Site",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_by_site_tecr # Highlight tributaries vs. mainstem sites

## TEAK ##
teak_data <- q[grepl("TEAK", q$DataID),]

SpC_by_site_teak <- ggplot(teak_data[-c(122),], aes(x = DataID, y = medianSPC.x)) +
  geom_boxplot() +
  labs(title = "Pre-fire SpC Distributions by Site: TEAK",
       x = "Site",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_by_site_teak # Highlight tributaries vs. mainstem sites

#########################
#### PRE & POST-FIRE ####
## SpC for TEAK01 and OUTLET; only pre-fire data for TECR01
## OUTLET ##
outlet_pre.post_fire_data <- q[grepl("OUT", q$DataID),]

SpC_pre.post_fire_out <- ggplot(outlet_pre.post_fire_data, aes(x = Date, y = medianSPC.x)) +
  geom_point() +
  labs(title = "Monthly SpC at the Outlet",
       x = "Sampling Date",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_pre.post_fire_out  

## TEAK ##
teak01_pre.post_fire_data <- q[grepl("TEAK01", q$DataID),]

SpC_pre.post_fire_teak01 <- ggplot(teak01_pre.post_fire_data, aes(x = Date, y = medianSPC.x)) +
  geom_point() +
  labs(title = "SpC by Sampling Date at TEAK01",
       x = "Sampling Date",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_pre.post_fire_teak01

## TECR ##
tecr01_pre_fire_data <- q[grepl("TECR01", q$DataID),]

SpC_pre_fire_tecr01 <- ggplot(tecr01_pre_fire_data, aes(x = Date, y = medianSPC.x)) +
  geom_point() +
  labs(title = "SpC by Sampling Date at TECR01",
       x = "Sampling Date",
       y = "SpC (uS/cm)") +
  theme_minimal()
SpC_pre_fire_tecr01


##############################
## Dissolved Organic Carbon ##
##############################

##################
#### PRE-FIRE ####
## By catchment with weekly monitoring and October data removed
DOC_by_catchment <- ggplot(doc_tdn[-c(152:159),], aes(x = Catchment, y = NPOC, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-fire DOC Distributions by Catchment",
       x = "Catchment",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_by_catchment

## By campaign with weekly monitoring and October data removed
DOC_by_campaign <- ggplot(doc_tdn[-c(132:151, 152:159),], aes(x = Campaign, y = NPOC, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire DOC Distributions by Campaign (weekly monitoring data removed)",
       x = "Campaign",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_by_campaign

## By site/catchment
## TECR ##
tecr_data <- doc_tdn[grepl("TECR", doc_tdn$Site),]

DOC_by_site_tecr <- ggplot(tecr_data[-c(154,156,158)], aes(x = Site, y = NPOC)) +
  geom_boxplot() +
  labs(title = "Pre-fire DOC Distributions by Site: TECR",
       x = "Site",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_by_site_tecr

## TEAK ##
teak_data <- doc_tdn[grepl("TEAK", doc_tdn$Site),]

DOC_by_site_teak <- ggplot(teak_data[-c(152,153,155,159),], aes(x = Site, y = NPOC)) +
  geom_boxplot() +
  labs(title = "Pre-fire DOC Distributions by Site: TEAK",
       x = "Site",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_by_site_teak

###########################
#### PRE & POST-FIRE ####
## DOC by sampling date with only OUTLET and TEAK01 
## OUTLET ##
outletDOC_pre.post_fire_data <- doc_tdn[grepl("OUT", doc_tdn$Site),]

DOC_pre.post_fire_out <- ggplot(outletDOC_pre.post_fire_data, aes(x = Date, y = NPOC)) +
  geom_point(size = 2) +
  labs(title = "DOC Concentrations by Sampling Date: OUTLET",
       x = "Sampling Date",
       y = "DOC (mg/L") +
  theme_minimal()
DOC_pre.post_fire_out

## TEAK01 ##
teak01DOC_pre.post_fire_data <- doc_tdn[grepl("TEAK01", doc_tdn$Site),]

DOC_pre.post_fire_teak01 <- ggplot(teak01DOC_pre.post_fire_data, aes(x = Date, y = NPOC)) +
  geom_point(size = 2) +
  labs(title = "DOC Concentrations by Sampling Date: TEAK01",
       x = "Sampling Date",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_pre.post_fire_teak01

## TECR01 ##
tecr01DOC_pre_fire_data <- doc_tdn[grepl("TECR01", doc_tdn$Site),]

DOC_pre_fire_data_tecr01 <- ggplot(tecr01DOC_pre_fire_data, aes(x = Date, y = NPOC)) +
  geom_point(size = 2) +
  labs(title = "DOC Concentrations by Sampling Date: TECR01",
       x = "Sampling Date",
       y = "DOC (mg/L)") +
  theme_minimal()
DOC_pre_fire_data_tecr01


####################
## Total Nitrogen ##
####################

##################
#### PRE-FIRE ####
## By catchment with October data removed
TN_by_catchment <- ggplot(doc_tdn[-c(152:159),], aes(x = Catchment, y = TN, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-fire TN Distributions by Catchment (excluding October)",
       x = "Catchment",
       y = "TN (mg/L)") +
  theme_minimal()
TN_by_catchment

## By campaign with weekly monitoring and October data removed
TN_by_campaign <- ggplot(doc_tdn[-c(132:151, 152:159),], aes(x = Campaign, y = TN, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire TN Distributions by Campaign (weekly monitoring data removed)",
       x = "Campaign",
       y = "TN (mg/L)") +
  theme_minimal()
TN_by_campaign

## By site/catchment
## TECR ##
tecr_data <- doc_tdn[grepl("TECR", doc_tdn$Site),]

TN_by_site_tecr <- ggplot(tecr_data[-c(154,156,158)], aes(x = Site, y = TN)) +
  geom_boxplot() +
  labs(title = "Pre-fire TN Distributions by Site: TECR",
       x = "Site",
       y = "TN (mg/L)") +
  theme_minimal()
TN_by_site_tecr

## TEAK ##
teak_data <- doc_tdn[grepl("TEAK", doc_tdn$Site),]

TN_by_site_teak <- ggplot(teak_data[-c(152,153,155,159),], aes(x = Site, y = TN)) +
  geom_boxplot() +
  labs(title = "Pre-fire TN Distributions by Site: TEAK",
       x = "Site",
       y = "TN (mg/L)") +
  theme_minimal()
TN_by_site_teak

###################
#### POST-FIRE ####
## TN by sampling date with only OUTLET and TEAK01 
## OUTLET ##
outletTN_pre.post_fire_data <- doc_tdn[grepl("OUT", doc_tdn$Site),]

TN_pre.post_fire_out <- ggplot(outletTN_pre.post_fire_data, aes(x = Date, y = TN)) +
  geom_point(size = 2) +
  labs(title = "TN Concentrations by Sampling Date: OUTLET",
       x = "Sampling Date",
       y = "TN (mg/L") +
  theme_minimal()
TN_pre.post_fire_out

## TEAK01 ##
teak01TN_pre.post_fire_data <- doc_tdn[grepl("TEAK01", doc_tdn$Site),]

TN_pre.post_fire_teak01 <- ggplot(teak01TN_pre.post_fire_data, aes(x = Date, y = TN)) +
  geom_point(size = 2) +
  labs(title = "TN Concentrations by Sampling Date: TEAK01",
       x = "Sampling Date",
       y = "TN (mg/L)") +
  theme_minimal()
TN_pre.post_fire_teak01

## TECR01 ##
tecr01TN_pre_fire_data <- doc_tdn[grepl("TECR01", doc_tdn$Site),]

TN_pre_fire_data_tecr01 <- ggplot(tecr01TN_pre_fire_data, aes(x = Date, y = TN)) +
  geom_point(size = 2) +
  labs(title = "TN Concentrations by Sampling Date: TECR01",
       x = "Sampling Date",
       y = "TN (mg/L)") +
  theme_minimal()
TN_pre_fire_data_tecr01


############################
## Total Suspended Solids ##
############################

##################
#### PRE-FIRE ####
## By catchment with weekly monitoring data
TSS_by_catchment <- ggplot(tss[-c(127:133),], aes(x = Catchment, y = TSS_by_Vol, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-Fire TSS Distributions by Catchment (excluding October)",
       x = "Catchment",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_by_catchment

## By campaign with weekly monitoring data removed
TSS_by_campaign <- ggplot(tss[-c(25:28,57:62,91:98,127:133),], aes(x = Campaign, y = TSS_by_Vol, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire TSS Distributions by Campaign (weekly monitoring removed)",
       x = "Campaign",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_by_campaign

## By site/catchment
## TECR ##
tecr_data <- tss[grepl("TECR", tss$Site),]

TSS_by_site_tecr <- ggplot(tecr_data[-c(131:133),], aes(x = Site, y = TSS_by_Vol)) +
  geom_boxplot() +
  labs(title = "Pre-fire TSS Distributions by Site: TECR",
       x = "Site",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_by_site_tecr

## TEAK ##
teak_data <- tss[grepl("TEAK", tss$Site),]

TSS_by_site_teak <- ggplot(teak_data[-c(128:130),], aes(x = Site, y = TSS_by_Vol)) +
  geom_boxplot() +
  labs(title = "Pre-Fire TSS Distributions by Site: TEAK",
       x = "Site",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_by_site_teak

#########################
#### PRE & POST-FIRE ####
## TSS by sampling date with only OUTLET and TEAK01 
## OUTLET ##
outletTSS_pre.post_fire_data <- tss[grepl("OUT", tss$Site),]

TSS_pre.post_fire_out <- ggplot(outletTSS_pre.post_fire_data, aes(x = Date, y = TSS_by_Vol)) +
  geom_point(size = 2) +
  labs(title = "TSS Concentrations by Sampling Date: OUTLET",
       x = "Sampling Date",
       y = "TS (mg/L)") +
  theme_minimal()
TSS_pre.post_fire_out

## TEAK01 ##
teak01TSS_pre.post_fire_data <- tss[grepl("TEAK01", tss$Site),]

TSS_pre.post_fire_teak01 <- ggplot(teak01TSS_pre.post_fire_data, aes(x = Date, y = TSS_by_Vol)) +
  geom_point(size = 2) +
  labs(title = "TSS Concentrations by Sampling Date: TEAK01",
       x = "Sampling Date",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_pre.post_fire_teak01

## TECR01 ##
tecr01TSS_pre_fire_data <- tss[grepl("TECR01", tss$Site),]

TSS_pre_fire_data_tecr01 <- ggplot(tecr01TSS_pre_fire_data, aes(x = Date, y = TSS_by_Vol)) +
  geom_point(size = 2) +
  labs(title = "TSS Concentrations by Sampling Date: TECR01",
       x = "Sampling Date",
       y = "TSS (mg/L)") +
  theme_minimal()
TSS_pre_fire_data_tecr01


############
## Carbon ##
############

##################
#### PRE-FIRE ####
## By catchment with weekly monitoring data
C_by_catchment <- ggplot(tss[-c(127:133),], aes(x = Catchment, y = C_Content, fill = Catchment)) +
  geom_boxplot() +
  labs(title = "Pre-Fire C Distributions by Catchment (excluding October)",
       x = "Catchment",
       y = "Carbon (mg/L)") +
  theme_minimal()
C_by_catchment

## By campaign with weekly monitoring data removed
C_by_campaign <- ggplot(tss[-c(25:28,57:62,91:98, 127:133),], aes(x = Campaign, y = C_Content, fill = Campaign)) +
  geom_boxplot() +
  labs(title = "Pre-fire C Distributions by Campaign (weekly monitoring removed)",
       x = "Campaign",
       y = "Carbon (mg/L)") +
  theme_minimal()
C_by_campaign

## By site/catchment
## TECR ##
tecr_data <- tss[grepl("TECR", tss$Site),]

C_by_site_tecr <- ggplot(tecr_data[-c(131:133),], aes(x = Site, y = C_Content)) +
  geom_boxplot() +
  labs(title = "Pre-fire C Distributions by Site: TECR",
       x = "Site",
       y = "C (mg/L)") +
  theme_minimal()
C_by_site_tecr

## TEAK ##
teak_data <- tss[grepl("TEAK", tss$Site),]

C_by_site_teak <- ggplot(teak_data[-c(128:130),], aes(x = Site, y = C_Content)) +
  geom_boxplot() +
  labs(title = "Pre-Fire C Distributions by Site: TEAK",
       x = "Site",
       y = "C (mg/L)") +
  theme_minimal()
C_by_site_teak

#########################
#### PRE & POST-FIRE ####
## TSS by sampling date with only OUTLET and TEAK01 
## OUTLET ##
outletC_pre.post_fire_data <- tss[grepl("OUT", tss$Site),]

C_pre.post_fire_out <- ggplot(outletC_pre.post_fire_data, aes(x = Date, y = C_Content)) +
  geom_point(size = 2) +
  labs(title = "C Concentrations by Sampling Date: OUTLET",
       x = "Sampling Date",
       y = "C (mg/L)") +
  theme_minimal()
C_pre.post_fire_out

## TEAK01 ##
teak01C_pre.post_fire_data <- tss[grepl("TEAK01", tss$Site),]

C_pre.post_fire_teak01 <- ggplot(teak01C_pre.post_fire_data, aes(x = Date, y = C_Content)) +
  geom_point(size = 2) +
  labs(title = "C Concentrations by Sampling Date: TEAK01",
       x = "Sampling Date",
       y = "C (mg/L)") +
  theme_minimal()
C_pre.post_fire_teak01

## TECR01 ##
tecr01C_pre.post_fire_data <- tss[grepl("TECR01", tss$Site),]

C_pre.post_fire_tecr01 <- ggplot(tecr01C_pre.post_fire_data, aes(x = Date, y = C_Content)) +
  geom_point(size = 2) +
  labs(title = "C Concentrations by Sampling Date: TECR01",
       x = "Sampling Date",
       y = "C (mg/L)") +
  theme_minimal()
C_pre.post_fire_tecr01

