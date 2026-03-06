#### Read me #### 
# This code calculates leverage for preliminary data analyses from the 2025 field season. 
# Author: AJW
# Last update: 2026-01-21

###########################################
#### Set up data ####
###########################################
#### libraries ####
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("googledrive")
library(dplyr)
library(tidyr)
library(ggplot2)
library(googledrive) # Make sure Google Drive authentication is updated, otherwise the connection won't work.
library(googlesheets4)
library(lubridate)
library(tidyverse)
library(dataRetrieval) # Download USGS discharge data
library(ggforce)
options(scipen=999)
library(viridis)
library(gridExtra)
library(moments)
library(cowplot)

#### set up working directory ####

dir.create(file.path("data/", "googledrive_data"))

#### load chem and q data ####
# NEED to improve this code to specify what Google Drive subfolders to look within for Google Drive downloads
# parent_folder <- drive_get("Teakettle Project")
# files_in_folder <- drive_ls(parent_folder)
# parent_subfolder <- drive_get("2025 Data")
# files_in_subfolder <- drive_ls(parent_subfolder)

# Download the desired file to the working directory
drive_download("2025_Q.csv", path = "data/googledrive_data/2025_Q.csv", overwrite = TRUE)
drive_download("2025_TSS.csv", path = "data/googledrive_data/2025_TSS.csv", overwrite = TRUE)
drive_download("2025_DOC_TDN.csv", path = "data/googledrive_data/2025_DOC_TDN.csv", overwrite = TRUE)

# Load CSVs into R
q <- read.csv("data/googledrive_data/2025_Q.csv")
tss <- read.csv("data/googledrive_data/2025_TSS.csv")
doc_tdn <- read.csv("data/googledrive_data/2025_DOC_TDN.csv")

# Load YSI data from Webster Sample Log
log = read.csv("data/TEA Webster Lab Samples Log Sheet downloaded_20260204.csv")

#### wrangle chem and q data ####

# remove weekly monitoring samples
q_r = q %>% filter(Campaign != "WM")
tss_r = tss %>% filter(Campaign != "WM")
doc_tdn_r = doc_tdn %>% filter(Campaign != "WM")

# reduce to just one bottle
log_r = log %>% filter(bottle_type == "60_mL") 
log_r = distinct(log_r, Site, Date, Time, .keep_all= TRUE)

# harmonize headers
colnames(q_r)[which(colnames(q_r) == "DataID")] <- "Site"

# harmonize site names
q_r$Site = gsub("OUTLET", "OUT", q_r$Site)
tss_r$Site = gsub("OUTLET", "OUT", tss_r$Site)
log_r$Site = gsub("OUTLET", "OUT", log_r$Site)
#
q_r$Site = gsub("TEAK03B", "TEAK03", q_r$Site)
tss_r$Site = gsub("TEAK03B", "TEAK03", tss_r$Site)
doc_tdn_r$Site = gsub("TEAK03B", "TEAK03", doc_tdn_r$Site)
log_r$Site = gsub("TEAK03B", "TEAK03", log_r$Site)

# remove colums I don't want to join by:
q_r$Type=NULL
tss_r$Type=NULL
log_r$Campaign=NULL
log_r$Type=NULL

# There are duplicate NPOC and TN data for July TEAK12 and July TEAK13 because Elliot accidentally ran the archive sample. I'll average the very similar values.
doc_tdn_r <- doc_tdn_r %>%
  group_by(Catchment, Campaign, Site, Type, Date) %>%
  summarise(across(c(NPOC, TN, TN_adj), mean, na.rm = TRUE))

# for TSS, the TSS g/mL was calcualted, but the carbon content g/mL was not. This code adds that:
tss_r$C_Content_by_Vol = tss_r$C_Content/tss_r$Sample.Volume..mL.

# both TSS_by_Vol and C_Content_by_Vol are in g/mL, which is silly small since chems are usually in mg/L. Converting to mg/L so the chems are all the same:
tss_r$TSS_by_Vol = 1000000 * tss_r$TSS_by_Vol
tss_r$C_Content_by_Vol = 1000000 * tss_r$C_Content_by_Vol

# simplify sample log
log_r = log_r[,c(1:22)]

# JOIN
all = left_join(doc_tdn_r, tss_r, by=c("Catchment", "Campaign", "Site","Date"))
all = left_join(all, q_r, by=c("Catchment", "Campaign", "Site","Date"))
all = left_join(all, log_r, by=c("Site","Date"))

# format dates
all$Date = as.Date(all$Date)

# convert SpC in us/cm to TDS in mg/L
# see https://www.geotechenv.com/Manuals/YSI_Manuals/YSI_Pro30_Manual.pdf
all$TDS_mgL = all$SpC_uScm2 * 0.65

# remove unneeded columns
# What is TN_adj? The values are lower than TN, so I'm guessing it is subtracting blank values or something like that. I will ignore it until I find out
all_r <- subset(all, select = c(Catchment,Campaign,Site,Type,Date,
                              NPOC, TN,
                              TSS_by_Vol,C_Content_by_Vol,
                              TDS_mgL,
                              Q))

# add units to col names
names(all_r) = c("Catchment","Campaign","Site","Type","Date",
               "NPOC_mgL","TN_mgL",
               "TSS_by_Vol_mgL","C_Content_by_Vol_mgL",
               "TDS_mgL",
               "Q_Lsec")

# cleanup environment
rm(doc_tdn,doc_tdn_r,q,q_r,tss,tss_r,log,log_r)

#### load and join subcatchment areas ####

#load output of 02_subcatareas.R sript
areas = read.csv("data/script_output/02_subcatareas/subcatareas_all.csv")

# harmonize site names
areas$Area_ID = gsub("OUTLET", "OUT", areas$Area_ID)

# # convert areas from km2 to m2 since m2 is usually what we use to calculate leverage in QuEST datasets
# areas$Size_m2 = areas$Size_km2*1000000

syn = left_join(all_r, areas, by=join_by(Site == Area_ID))

###########################################
#### Calculate specific discharge (Qs) ####
###########################################

# make sure that Q and area are numeric values
structure(syn)

# calculate Qs 
syn <- syn %>%
  group_by(Catchment, Campaign, Site, Date, Size_km2) %>%
  mutate(Qs_Lseckm2 = (Q_Lsec/Size_km2)) %>%
  ungroup()

# count non-NA values in Q column using dplyr
nonna_counts_dplyr <- syn %>%
  summarise_all(~ sum(!is.na(.)))

#################################################
#### Calculate Leverage in Paired Watersheds ####
################################################
# In this dataset, it is possible to calculate leverage in reference to the outlet of the larger experimental area ("OUT"), or separately in reference to each paired watershed's outlet ("TEAK01" and "TECR01"). Here, it is calculated separately in reference to each paired watershed's outlet.

# remove OUT data since we aren't using it here
syn_pairs = syn %>% filter(Catchment != "OUTLET")

### make data long 
syn_pairs_long=
  reshape2::melt(syn_pairs, id.vars=c("Catchment", "Campaign", "Site", "Type","Date", "Size_km2",
                                "Q_Lsec","Qs_Lseckm2"), 
                    measure.vars = c("NPOC_mgL","TN_mgL","TSS_by_Vol_mgL","C_Content_by_Vol_mgL","TDS_mgL"), 
                    na.rm=TRUE)

### calculate leverage with Q
syn_pairs_lev = 
  syn_pairs_long %>%
  group_by(Catchment, Campaign, variable) %>%
  mutate(leverage_wQ = ((value - value[which.max(Size_km2)]) * Size_km2/Size_km2[which.max(Size_km2)]) * Qs_Lseckm2/Qs_Lseckm2[which.max(Size_km2)])  

### calculate leverage without Q
# use if you don’t have specific Q values
# 100 * ((Cs-Co)*(As/Ao))
syn_pairs_lev = 
  syn_pairs_lev %>% 
  group_by(Catchment, Campaign, variable) %>%
  mutate(leverage_noQ = ((value - value[which.max(Size_km2)]) * Size_km2/Size_km2[which.max(Size_km2)]) *100) 

### remove outlets from dataset since the leverage there will always be zero
#syn_pairs_lev = syn_pairs_lev[syn_pairs_lev$Site != "TEAK01",]
#syn_pairs_lev = syn_pairs_lev[syn_pairs_lev$Site != "TECR01",]

### explore distributions
hist(syn_pairs_lev$leverage_wQ, breaks=50)
hist(syn_pairs_lev$leverage_noQ, breaks=50)

# summarize by ws and date mean and sd
syn_pairs_lev_summ = syn_pairs_lev %>%
  ### remove outlets from dataset since the leverage there will always be zero
  filter(Site != "TEAK01") %>%
  filter(Site != "TECR01") %>%
  group_by(Catchment, Campaign, variable) %>%
  summarize(count = n(),
            Size_km2_mean = mean(Size_km2, na.rm = TRUE),
            leverage_wQ_mean = mean(leverage_wQ, na.rm = TRUE), 
            leverage_wQ_sd = sd(leverage_wQ, na.rm = TRUE),
            leverage_noQ_mean = mean(leverage_noQ, na.rm = TRUE), 
            leverage_noQ_sd = sd(leverage_noQ, na.rm = TRUE))
#
syn_pairs_lev_summ$leverage_wQ_upperSD = syn_pairs_lev_summ$leverage_wQ_mean+syn_pairs_lev_summ$leverage_wQ_sd
syn_pairs_lev_summ$leverage_wQ_lowerSD = syn_pairs_lev_summ$leverage_wQ_mean-syn_pairs_lev_summ$leverage_wQ_sd
#
syn_pairs_lev_summ$leverage_noQ_upperSD = syn_pairs_lev_summ$leverage_noQ_mean+syn_pairs_lev_summ$leverage_noQ_sd
syn_pairs_lev_summ$leverage_noQ_lowerSD = syn_pairs_lev_summ$leverage_noQ_mean-syn_pairs_lev_summ$leverage_noQ_sd
#
syn_pairs_lev_summ$Catchment = as.factor(syn_pairs_lev_summ$Catchment)
syn_pairs_lev_summ$Campaign = as.factor(syn_pairs_lev_summ$Campaign)


### filter to only include sites sampled in Oct post-fire for pre/post fire comparison
postfire_sites = c("TEAK06","TEAK11",
                   "TECR06","TECR09")

syn_pairs_lev_firecomp = syn_pairs_lev %>%
  filter(Site %in% postfire_sites)

#################################################
#### Calculate Leverage in Combined Watershed ####
################################################
# In this dataset, it is possible to calculate leverage in reference to the outlet of the larger experimental area ("OUT"), or separately in reference to each paired watershed's outlet ("TEAK01" and "TECR01"). Here, it is calculated in reference to the shared "OUT" watershed outlet.

### make data long 
syn_long=
  reshape2::melt(syn, id.vars=c("Catchment", "Campaign", "Site", "Type","Date", "Size_km2",
                                      "Q_Lsec","Qs_Lseckm2"), 
                 measure.vars = c("NPOC_mgL","TN_mgL","TSS_by_Vol_mgL","C_Content_by_Vol_mgL","TDS_mgL"), 
                 na.rm=TRUE)

### calculate leverage with Q
syn_lev = 
  syn_long %>%
  group_by(Campaign, variable) %>%
  mutate(leverage_wQ = ((value - value[which.max(Size_km2)]) * Size_km2/Size_km2[which.max(Size_km2)]) * Qs_Lseckm2/Qs_Lseckm2[which.max(Size_km2)])  

### calculate leverage without Q
# use if you don’t have specific Q values
# 100 * ((Cs-Co)*(As/Ao))
syn_lev = 
  syn_lev %>% 
  group_by(Campaign, variable) %>%
  mutate(leverage_noQ = ((value - value[which.max(Size_km2)]) * Size_km2/Size_km2[which.max(Size_km2)]) *100) 


### explore distributions
hist(syn_lev$leverage_wQ, breaks=50)
hist(syn_lev$leverage_noQ, breaks=50)

# summarize by ws and date mean and sd
syn_lev_summ = syn_lev %>%
  ### remove outlets from dataset since the leverage there will always be zero
  filter(Site != "OUT") %>%
  group_by(Campaign, variable) %>%
  summarize(count = n(),
            Size_km2_mean = mean(Size_km2, na.rm = TRUE),
            leverage_wQ_mean = mean(leverage_wQ, na.rm = TRUE), 
            leverage_wQ_sd = sd(leverage_wQ, na.rm = TRUE),
            leverage_noQ_mean = mean(leverage_noQ, na.rm = TRUE), 
            leverage_noQ_sd = sd(leverage_noQ, na.rm = TRUE))
#
syn_lev_summ$leverage_wQ_upperSD = syn_lev_summ$leverage_wQ_mean+syn_lev_summ$leverage_wQ_sd
syn_lev_summ$leverage_wQ_lowerSD = syn_lev_summ$leverage_wQ_mean-syn_lev_summ$leverage_wQ_sd
#
syn_lev_summ$leverage_noQ_upperSD = syn_lev_summ$leverage_noQ_mean+syn_lev_summ$leverage_noQ_sd
syn_lev_summ$leverage_noQ_lowerSD = syn_lev_summ$leverage_noQ_mean-syn_lev_summ$leverage_noQ_sd
#
#syn_lev_summ$Catchment = as.factor(syn_lev_summ$Catchment)
syn_lev_summ$Campaign = as.factor(syn_lev_summ$Campaign)


### filter to only include sites sampled in Oct post-fire for pre/post fire comparison
postfire_sites = c("TEAK01", "TEAK06","TEAK11",
                   "TECR01", "TECR06","TECR09")

syn_lev_firecomp = syn_lev %>%
  filter(Site %in% postfire_sites)

###################################
#### Save final leverage files ####
###################################

## leverage in reference to catchment pairs
# write file to computer
dir.create(file.path("data/script_output", "20_leverage"))
write.csv(syn_pairs_lev, "data/script_output/20_leverage/Leverage_bypairs.csv")
# define the target folder ID in Google Drive
# this is the current leverage folder
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/Leverage_bypairs.csv", path = as_id(drive_folder_id), overwrite = TRUE)

## leverage in reference to shared outlet
dir.create(file.path("data/script_output", "20_leverage"))
write.csv(syn_lev, "data/script_output/20_leverage/Leverage_byoutlet.csv")
# define the target folder ID in Google Drive
# this is the current leverage folder
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/Leverage_byoutlet.csv", path = as_id(drive_folder_id), overwrite = TRUE)

###################################
#### Load leverage files ####
###################################

# Download the desired file to the working directory
drive_download("Leverage_bypairs.csv", path = "data/googledrive_data/Leverage_bypairs.csv", overwrite = TRUE)
# Load CSVs into R
syn_pairs_lev <- read.csv("data/googledrive_data/Leverage_bypairs.csv")
# fill in NAs about network position (main/trib)
syn_pairs_lev$Type[syn_pairs_lev$Site=="TEAK01"] = "Main"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TEAK06"] = "Trib"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TEAK08"] = "Trib"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TEAK10"] = "Trib"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TEAK12"] = "Trib"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TEAK13"] = "Main"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TEAK11"] = "Main"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TECR01"] = "Main"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TECR06"] = "Trib"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TECR09"] = "Main"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TECR05"] = "Trib"
syn_pairs_lev$Type[syn_pairs_lev$Site=="TECR08"] = "Trib"
# format dates
syn_pairs_lev$Date = as.Date(syn_pairs_lev$Date)
# highlight sites that were sampled in october
syn_pairs_lev = syn_pairs_lev %>%
  mutate(group = ifelse(Site %in% c("OUT","TEAK01", "TEAK06", "TEAK11","TECR01", "TECR06", "TECR09"), 
                        "prepost", "other")) 


# Download the desired file to the working directory
drive_download("Leverage_byoutlet.csv", path = "data/googledrive_data/Leverage_byoutlet.csv", overwrite = TRUE)
# Load CSVs into R
syn_lev <- read.csv("data/googledrive_data/Leverage_byoutlet.csv")
# fill in NAs about network position (main/trib)
syn_lev$Type[syn_lev$Site=="TEAK01"] = "Main"
syn_lev$Type[syn_lev$Site=="TEAK06"] = "Trib"
syn_lev$Type[syn_lev$Site=="TEAK08"] = "Trib"
syn_lev$Type[syn_lev$Site=="TEAK10"] = "Trib"
syn_lev$Type[syn_lev$Site=="TEAK12"] = "Trib"
syn_lev$Type[syn_lev$Site=="TEAK13"] = "Main"
syn_lev$Type[syn_lev$Site=="TEAK11"] = "Main"
syn_lev$Type[syn_lev$Site=="TECR01"] = "Main"
syn_lev$Type[syn_lev$Site=="TECR06"] = "Trib"
syn_lev$Type[syn_lev$Site=="TECR09"] = "Main"
syn_lev$Type[syn_lev$Site=="TECR05"] = "Trib"
syn_lev$Type[syn_lev$Site=="TECR08"] = "Trib"
# format dates
syn_lev$Date = as.Date(syn_lev$Date)
# highlight sites that were sampled in october
syn_lev = syn_lev %>%
  mutate(group = ifelse(Site %in% c("OUT","TEAK01", "TEAK06", "TEAK11","TECR01", "TECR06", "TECR09"), 
                        "prepost", "other")) 

#######################################
#### Plotting in Paired Watersheds ####
#######################################

# Positive leverage values indicate that the upstream subcatchment has a higher concentration than the outlet. This indicates that there must be solute uptake or dilution in the stream network. Less positive leverage values due to a smaller concentration difference indicate less uptake or dilution in the stream network, but still net uptake/dilution

#Negative leverage values indicate that the upstream subcatchment has a lower concentration than the outlet. This indicates that there must be solute production or enrichment in the stream network.

# highlight_points <- syn_pairs_lev[syn_pairs_lev$Site %in% c("TEAK01", "TEAK06", "TEAK11",
#                                                             "TECR01", "TECR06", "TECR09"), ]
syn_pairs_lev = syn_pairs_lev %>%
  mutate(group = ifelse(Site %in% c("TEAK01", "TEAK06", "TEAK11","TECR01", "TECR06", "TECR09"), 
                        "highlight", "normal")) 

# leverage over time for DOC ##
p1 = syn_pairs_lev %>%
  ### remove outlets from dataset since the leverage there will always be zero
  filter(Site != "TEAK01") %>%
  filter(Site != "TECR01") %>%
  dplyr::filter(variable %in% c("NPOC_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, fill=Catchment, color=group), alpha=0.6, size=2)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "grey")) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")+
  labs(title = "DOC leverage")

# leverage over time for C_Content_by_Vol_mgL ##
p2 = syn_pairs_lev %>%
  ### remove outlets from dataset since the leverage there will always be zero
  filter(Site != "TEAK01") %>%
  filter(Site != "TECR01") %>%
  dplyr::filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, fill=Catchment, color=group), alpha=0.6, size=2)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "grey")) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")+
  labs(title = "POC leverage")

# leverage over time for TN ##
p3 = syn_pairs_lev %>%
  ### remove outlets from dataset since the leverage there will always be zero
  filter(Site != "TEAK01") %>%
  filter(Site != "TECR01") %>%
  dplyr::filter(variable %in% c("TN_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, fill=Catchment, color=group), alpha=0.6, size=2)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "grey")) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")+
  labs(title = "TN leverage")

# # leverage over time for TSS_by_Vol_mgL ##
# syn_pairs_lev %>%
#   dplyr::filter(variable %in% c("TSS_by_Vol_mgL")) %>%
#   ggplot() +
#   xlab("") +
#   ylab("Subcatchment Leverage (%)") +
#   facet_grid(~factor(Catchment))+
#   geom_hline(yintercept=0, linetype = 'dashed') +
#   # geom_errorbar(aes(x = Date, y = leverage_noQ_mean, ymin=leverage_noQ_lowerSD, ymax=leverage_noQ_upperSD), linewidth=0.5) +
#   geom_point(aes(x = Date, y = leverage_noQ, fill=Catchment, color=Catchment), alpha=0.5)+
#   theme_bw()+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.text.x = element_text(angle = 45,hjust = 1),
#         legend.title = element_blank())

p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/leverage_all_overtime.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/leverage_all_overtime.png", path = as_id(drive_folder_id), overwrite = TRUE)



#### mean leverage per campaign ###

# mean leverage over time for DOC ##
p1 = syn_pairs_lev_summ %>%
  mutate(Campaign = fct_relevel(Campaign, c("May","June","July","August","October"))) %>% 
  dplyr::filter(variable %in% c("NPOC_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Mean Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_errorbar(aes(x = Campaign, y = leverage_noQ_mean, ymin=leverage_noQ_lowerSD, ymax=leverage_noQ_upperSD), linewidth=0.5) +
  geom_point(aes(x = Campaign, y = leverage_noQ_mean, fill=Catchment, color=Catchment, size=3))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())

# mean leverage over time for TN ##
p3 = syn_pairs_lev_summ %>%
  mutate(Campaign = fct_relevel(Campaign, c("May","June","July","August","October"))) %>% 
  dplyr::filter(variable %in% c("TN_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Mean Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_errorbar(aes(x = Campaign, y = leverage_noQ_mean, ymin=leverage_noQ_lowerSD, ymax=leverage_noQ_upperSD), linewidth=0.5) +
  geom_point(aes(x = Campaign, y = leverage_noQ_mean, fill=Catchment, color=Catchment, size=3))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())

# mean leverage over time for POC ##
p2 = syn_pairs_lev_summ %>%
  mutate(Campaign = fct_relevel(Campaign, c("May","June","July","August","October"))) %>% 
  dplyr::filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Mean Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_errorbar(aes(x = Campaign, y = leverage_noQ_mean, ymin=leverage_noQ_lowerSD, ymax=leverage_noQ_upperSD), linewidth=0.5) +
  geom_point(aes(x = Campaign, y = leverage_noQ_mean, fill=Catchment, color=Catchment, size=3))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())

p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/leverage_mean_overtime.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/leverage_mean_overtime.png", path = as_id(drive_folder_id), overwrite = TRUE)





### pre/post fire comparison sites only ###

# leverage over time for DOC pre/post fire comparison sites only ## 
p1=
  syn_pairs_lev_firecomp %>%
  dplyr::filter(variable %in% c("NPOC_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, fill=Catchment, color=Catchment, size=2), alpha=0.75)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  labs(title = "DOC leverage pre/post fire")

# leverage over time for POC pre/post fire comparison sites only ## 
p2 =
  syn_pairs_lev_firecomp %>%
  dplyr::filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, fill=Catchment, color=Catchment, size=2), alpha=0.75)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  labs(title = "POC leverage pre/post fire")

# leverage over time for TN pre/post fire comparison sites only ## 
p3 =
  syn_pairs_lev_firecomp %>%
  dplyr::filter(variable %in% c("TN_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, fill=Catchment, color=Catchment, size=2), alpha=0.75)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  labs(title = "TN leverage pre/post fire")

p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/leverage_prepostfiresites.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/leverage_prepostfiresites.png", path = as_id(drive_folder_id))





### variance collapse ###

## May ##
p1 = syn_pairs_lev %>%
  filter(variable %in% c("NPOC_mgL")) %>%
  filter(Campaign %in% c("May")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "May DOC")+
  theme_bw()+
  ylim(c(0,6)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p2 = syn_pairs_lev %>%
  filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  filter(Campaign %in% c("May")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "May POC")+
  theme_bw()+
  ylim(c(0,30)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p3 = syn_pairs_lev %>%
  filter(variable %in% c("TN_mgL")) %>%
  filter(Campaign %in% c("May")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "May TN")+
  theme_bw()+
  ylim(c(0,.2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/variancecollapse_May.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/variancecollapse_May.png", path = as_id(drive_folder_id), overwrite = TRUE)

## June ##
p1 = syn_pairs_lev %>%
  filter(variable %in% c("NPOC_mgL")) %>%
  filter(Campaign %in% c("June")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "June DOC")+
  theme_bw()+
  ylim(c(0,6)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p2 = syn_pairs_lev %>%
  filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  filter(Campaign %in% c("June")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "June POC")+
  theme_bw()+
  ylim(c(0,30)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p3 = syn_pairs_lev %>%
  filter(variable %in% c("TN_mgL")) %>%
  filter(Campaign %in% c("June")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "June TN")+
  theme_bw()+
  ylim(c(0,0.2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/variancecollapse_June.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/variancecollapse_June.png", path = as_id(drive_folder_id), overwrite = TRUE)

## July ##
p1 = syn_pairs_lev %>%
  filter(variable %in% c("NPOC_mgL")) %>%
  filter(Campaign %in% c("July")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "July DOC")+
  theme_bw()+
  ylim(c(0,6)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p2 = syn_pairs_lev %>%
  filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  filter(Campaign %in% c("July")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "July POC")+
  theme_bw()+
  ylim(c(0,30)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p3 = syn_pairs_lev %>%
  filter(variable %in% c("TN_mgL")) %>%
  filter(Campaign %in% c("July")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "July TN")+
  theme_bw()+
  ylim(c(0,0.2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/variancecollapse_July.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/variancecollapse_July.png", path = as_id(drive_folder_id), overwrite = TRUE)


## August ##
p1 = syn_pairs_lev %>%
  filter(variable %in% c("NPOC_mgL")) %>%
  filter(Campaign %in% c("August")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "August DOC")+
  theme_bw()+
  ylim(c(0,6)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p2 = syn_pairs_lev %>%
  filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  filter(Campaign %in% c("August")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "August POC")+
  theme_bw()+
  ylim(c(0,30)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p3 = syn_pairs_lev %>%
  filter(variable %in% c("TN_mgL")) %>%
  filter(Campaign %in% c("August")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "August TN")+
  theme_bw()+
  ylim(c(0,0.2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/variancecollapse_August.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/variancecollapse_August.png", path = as_id(drive_folder_id), overwrite = TRUE)


## October ##
p1 = syn_pairs_lev %>%
  filter(variable %in% c("NPOC_mgL")) %>%
  filter(Campaign %in% c("October")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "October DOC")+
  theme_bw()+
  ylim(c(0,6)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p2 = syn_pairs_lev %>%
  filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  filter(Campaign %in% c("October")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "October POC")+
  theme_bw()+
  ylim(c(0,30)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p3 = syn_pairs_lev %>%
  filter(variable %in% c("TN_mgL")) %>%
  filter(Campaign %in% c("October")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "October TN")+
  theme_bw()+
  ylim(c(0,0.2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/variancecollapse_October.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/variancecollapse_October.png", path = as_id(drive_folder_id), overwrite = TRUE)


#######################################
#### Plotting in Combined Watershed ####
#######################################

#### concentrations ####

syn_lev %>%
  dplyr::filter(variable %in% c("NPOC_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Concentration (mg/L)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = value, color=group, fill=Catchment), alpha=0.6, size=4, shape=21)+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green", "OUTLET"="red")) +
  #scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  #legend.position = "none")+
  labs(title = "DOC")

syn_lev_firecomp %>%
  dplyr::filter(variable %in% c("NPOC_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Concentration (mg/L)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = value, color=group, fill=Catchment), alpha=0.6, size=4, shape=21)+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green", "OUTLET"="red")) +
  #scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  #legend.position = "none")+
  labs(title = "DOC")

syn_lev %>%
  dplyr::filter(variable %in% c("TDS_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Concentration (mg/L)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = value, color=group, fill=Catchment), alpha=0.6, size=4, shape=21)+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green", "OUTLET"="red")) +
  #scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  #legend.position = "none")+
  labs(title = "TDS")


syn_lev %>%
  dplyr::filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Concentration (mg/L)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = value, color=group, fill=Catchment), alpha=0.6, size=4, shape=21,
             position = position_jitter(width = 4, height = 0.1))+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green", "OUTLET"="red")) +
  ylim(-1,50)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  #legend.position = "none")+
  labs(title = "POC")


syn_lev %>%
  dplyr::filter(variable %in% c("TN_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Concentration (mg/L)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = value, color=group, fill=Catchment), alpha=0.6, size=4, shape=21)+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green", "OUTLET"="red")) +
  #scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  #legend.position = "none")+
  labs(title = "TDN")

syn_lev %>%
  dplyr::filter(variable %in% c("TSS_by_Vol_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Concentration (mg/L)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = value, color=group, fill=Type), alpha=0.6, size=4, shape=21,
             position = position_jitter(width = 4, height = 0.1))+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  #scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green", "OUTLET"="red")) +
  scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  #ylim(-1,50)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  #legend.position = "none")+
  labs(title = "TSS")


syn_lev_summ %>%
  dplyr::filter(variable %in% c("TSS_by_Vol_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Concentration (mg/L)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = value, color=group, fill=Type), alpha=0.6, size=4, shape=21,
             position = position_jitter(width = 4, height = 0.1))+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  #scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green", "OUTLET"="red")) +
  scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  #ylim(-1,50)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  #legend.position = "none")+
  labs(title = "TSS")


#### leverage ####
# Positive leverage values indicate that the upstream subcatchment has a higher concentration than the outlet. This indicates that there must be solute uptake or dilution in the stream network. Less positive leverage values due to a smaller concentration difference indicate less uptake or dilution in the stream network, but still net uptake/dilution

#Negative leverage values indicate that the upstream subcatchment has a lower concentration than the outlet. This indicates that there must be solute production or enrichment in the stream network.


# leverage over time for DOC ##
p1 = syn_lev %>%
  ### remove outlets from dataset since the leverage there will always be zero
  filter(Site != "OUT") %>%
  dplyr::filter(variable %in% c("NPOC_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  #facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, color=group, fill=Catchment), alpha=0.6, size=4, shape=21)+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green")) +
  #scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
        #legend.position = "none")+
  labs(title = "DOC leverage")

# leverage over time for C_Content_by_Vol_mgL ##
p2 = syn_lev %>%
  ### remove outlets from dataset since the leverage there will always be zero
  filter(Site != "OUT") %>%
  dplyr::filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, color=group, fill=Catchment), alpha=0.6, size=4, shape=21)+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green")) +
  #scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
        #legend.position = "none")+
  labs(title = "POC leverage")

# leverage over time for TN ##
p3 = syn_lev %>%
  ### remove outlets from dataset since the leverage there will always be zero
  filter(Site != "OUT") %>%
  dplyr::filter(variable %in% c("TN_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, color=group, fill=Catchment), alpha=0.6, size=4, shape=21)+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green")) +
  #scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
        #legend.position = "none")+
  labs(title = "TN leverage")

# leverage over time for TDS ##
p4 = syn_lev %>%
  ### remove outlets from dataset since the leverage there will always be zero
  filter(Site != "OUT") %>%
  dplyr::filter(variable %in% c("TDS_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, color=group, fill=Catchment), alpha=0.6, size=3, shape=21)+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green")) +
  #scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  #legend.position = "none")+
  labs(title = "TDS leverage")

syn_lev %>%
  ### remove outlets from dataset since the leverage there will always be zero
  filter(Site != "OUT") %>%
  dplyr::filter(variable %in% c("TSS_by_Vol_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, color=group, fill=Catchment), alpha=0.6, size=3, shape=21)+
             #position = position_jitter(width = 3, height = 0.1))+
  scale_color_manual(values = c("prepost" = "red", "other" = "grey")) + 
  scale_fill_manual(values = c("TECR" = "blue", "TEAK" = "green")) +
  #scale_fill_manual(values = c("Main" = "lightblue", "Trib" = "blue")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  #legend.position = "none")+
  labs(title = "TSS leverage")

p = plot_grid(p1, p4, p2, p3, nrow = 2, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/leverage_comboWS_all_overtime.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/leverage_comboWS_all_overtime.png", path = as_id(drive_folder_id), overwrite = TRUE)



#### mean leverage per campaign ###

# mean leverage over time for DOC ##
p1 = syn_lev_summ %>%
  mutate(Campaign = fct_relevel(Campaign, c("May","June","July","August","October"))) %>% 
  dplyr::filter(variable %in% c("NPOC_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Mean Subcatchment Leverage (%)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_errorbar(aes(x = Campaign, y = leverage_noQ_mean, ymin=leverage_noQ_lowerSD, ymax=leverage_noQ_upperSD), linewidth=0.5) +
  geom_point(aes(x = Campaign, y = leverage_noQ_mean, size=3))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")+
  labs(title = "DOC")

# mean leverage over time for TN ##
p3 = syn_lev_summ %>%
  mutate(Campaign = fct_relevel(Campaign, c("May","June","July","August","October"))) %>% 
  dplyr::filter(variable %in% c("TN_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Mean Subcatchment Leverage (%)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_errorbar(aes(x = Campaign, y = leverage_noQ_mean, ymin=leverage_noQ_lowerSD, ymax=leverage_noQ_upperSD), linewidth=0.5) +
  geom_point(aes(x = Campaign, y = leverage_noQ_mean, size=3))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")+
  labs(title = "Total Nitrogen")

# mean leverage over time for POC ##
p2 = syn_lev_summ %>%
  mutate(Campaign = fct_relevel(Campaign, c("May","June","July","August","October"))) %>% 
  dplyr::filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Mean Subcatchment Leverage (%)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_errorbar(aes(x = Campaign, y = leverage_noQ_mean, ymin=leverage_noQ_lowerSD, ymax=leverage_noQ_upperSD), linewidth=0.5) +
  geom_point(aes(x = Campaign, y = leverage_noQ_mean, size=3))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")+
  labs(title = "POC")

# mean leverage over time for TDS ##
p4 = syn_lev_summ %>%
  mutate(Campaign = fct_relevel(Campaign, c("May","June","July","August","October"))) %>% 
  dplyr::filter(variable %in% c("TDS_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Mean Subcatchment Leverage (%)") +
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_errorbar(aes(x = Campaign, y = leverage_noQ_mean, ymin=leverage_noQ_lowerSD, ymax=leverage_noQ_upperSD), linewidth=0.5) +
  geom_point(aes(x = Campaign, y = leverage_noQ_mean, size=3))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")+
  labs(title = "TDS")

p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/leverage_comboWS_mean_overtime.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/leverage_comboWS_mean_overtime.png", path = as_id(drive_folder_id), overwrite = TRUE)





### pre/post fire comparison sites only ###

# leverage over time for DOC pre/post fire comparison sites only ## 
p1=
  syn_lev_firecomp %>%
  dplyr::filter(variable %in% c("NPOC_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, fill=Catchment, color=Catchment, size=2), alpha=0.75)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  labs(title = "DOC leverage pre/post fire")

# leverage over time for POC pre/post fire comparison sites only ## 
p2 =
  syn_lev_firecomp %>%
  dplyr::filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, fill=Catchment, color=Catchment, size=2), alpha=0.75)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  labs(title = "POC leverage pre/post fire")

# leverage over time for TN pre/post fire comparison sites only ## 
p3 =
  syn_lev_firecomp %>%
  dplyr::filter(variable %in% c("TN_mgL")) %>%
  ggplot() +
  xlab("") +
  ylab("Subcatchment Leverage (%)") +
  facet_grid(~factor(Catchment))+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_point(aes(x = Date, y = leverage_noQ, fill=Catchment, color=Catchment, size=2), alpha=0.75)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+
  labs(title = "TN leverage pre/post fire")

p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/leverage_prepostfiresites.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/leverage_prepostfiresites.png", path = as_id(drive_folder_id))





#### variance collapse ####

## May ##
p1 = syn_lev %>%
  filter(variable %in% c("NPOC_mgL")) %>%
  filter(Campaign %in% c("May")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "May DOC")+
  theme_bw()+
  ylim(c(0,6)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p2 = syn_lev %>%
  filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  filter(Campaign %in% c("May")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "May POC")+
  theme_bw()+
  ylim(c(0,30)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p3 = syn_lev %>%
  filter(variable %in% c("TN_mgL")) %>%
  filter(Campaign %in% c("May")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "May TN")+
  theme_bw()+
  ylim(c(0,.2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/variancecollapse_May.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/variancecollapse_May.png", path = as_id(drive_folder_id), overwrite = TRUE)

## June ##
p1 = syn_lev %>%
  filter(variable %in% c("NPOC_mgL")) %>%
  filter(Campaign %in% c("June")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "June DOC")+
  theme_bw()+
  ylim(c(0,6)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p2 = syn_lev %>%
  filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  filter(Campaign %in% c("June")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "June POC")+
  theme_bw()+
  ylim(c(0,30)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p3 = syn_lev %>%
  filter(variable %in% c("TN_mgL")) %>%
  filter(Campaign %in% c("June")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "June TN")+
  theme_bw()+
  ylim(c(0,0.2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/variancecollapse_June.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/variancecollapse_June.png", path = as_id(drive_folder_id), overwrite = TRUE)

## July ##
p1 = syn_lev %>%
  filter(variable %in% c("NPOC_mgL")) %>%
  filter(Campaign %in% c("July")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "July DOC")+
  theme_bw()+
  ylim(c(0,6)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p2 = syn_lev %>%
  filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  filter(Campaign %in% c("July")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "July POC")+
  theme_bw()+
  ylim(c(0,30)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p3 = syn_lev %>%
  filter(variable %in% c("TN_mgL")) %>%
  filter(Campaign %in% c("July")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "July TN")+
  theme_bw()+
  ylim(c(0,0.2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/variancecollapse_July.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/variancecollapse_July.png", path = as_id(drive_folder_id), overwrite = TRUE)


## August ##
p1 = syn_lev %>%
  filter(variable %in% c("NPOC_mgL")) %>%
  filter(Campaign %in% c("August")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "August DOC")+
  theme_bw()+
  ylim(c(0,6)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p2 = syn_lev %>%
  filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  filter(Campaign %in% c("August")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "August POC")+
  theme_bw()+
  ylim(c(0,30)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p3 = syn_lev %>%
  filter(variable %in% c("TN_mgL")) %>%
  filter(Campaign %in% c("August")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "August TN")+
  theme_bw()+
  ylim(c(0,0.2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/variancecollapse_August.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/variancecollapse_August.png", path = as_id(drive_folder_id), overwrite = TRUE)


## October ##
p1 = syn_lev %>%
  filter(variable %in% c("NPOC_mgL")) %>%
  filter(Campaign %in% c("October")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "October DOC")+
  theme_bw()+
  ylim(c(0,6)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p2 = syn_lev %>%
  filter(variable %in% c("C_Content_by_Vol_mgL")) %>%
  filter(Campaign %in% c("October")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "October POC")+
  theme_bw()+
  ylim(c(0,30)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p3 = syn_lev %>%
  filter(variable %in% c("TN_mgL")) %>%
  filter(Campaign %in% c("October")) %>%
  ggplot()+
  geom_point(aes(x = Size_km2, y = value, shape=Catchment, color=group), alpha=0.6, size=3)+
  scale_color_manual(values = c("highlight" = "blue", "normal" = "black")) + 
  geom_hline(yintercept=0, linetype = 'dashed') +
  labs(x = "Subcatchment Area (km2)", y = "Concentration (mg/L)", title = "October TN")+
  theme_bw()+
  ylim(c(0,0.2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())+ guides(color = "none")
p = plot_grid(p1, p2, p3, nrow = 3, align = "h")

# Save the plot as a PNG file locally
ggsave("data/script_output/20_leverage/variancecollapse_October.png", plot = p)

## Save to Drive
# Define the local folder path and the target folder ID in Google Drive
local_folder <- "data/script_output/20_leverage/"
drive_folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1Vnk--rdl1BYdg_Tgo0y_STwQNwhcUulG")
# upload the file to the specified Google Drive folder
drive_upload(media = "data/script_output/20_leverage/variancecollapse_October.png", path = as_id(drive_folder_id), overwrite = TRUE)

