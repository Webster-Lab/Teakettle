#### Read me ####
# The following code is for exploratory data analysis of NEON data. It utilizes
# the "merged" datasets from the "Merged datasets" folder in Google Drive. 
# The only data not included here is the salt-based discharge dataset. See notes 
# in "08_saltdischargeNEON.R" for an explanation.

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
drive_download("all_dissolvedgases_data.csv", path = "all_dissolvedgases_data.csv", overwrite = TRUE)
drive_download("all_elevation_data.csv", path = "all_elevation_data.csv", overwrite = TRUE)
drive_download("all_fielddischarge_data.csv", path = "all_fielddischarge_data.csv", overwrite = TRUE)
drive_download("all_gaugeheight_data.csv", path = "all_gaugeheight_data.csv", overwrite = TRUE)
drive_download("all_isotope_data.csv", path = "all_isotope_data.csv", overwrite = TRUE)
drive_download("all_nitrate_data.csv", path = "all_nitrate_data.csv", overwrite = TRUE)
drive_download("all_ratingcurve_data.csv", path = "all_ratingcurve_data.csv", overwrite = TRUE)
drive_download("all_temp_data.csv", path = "all_temp_data.csv", overwrite = TRUE)
drive_download("all_waterchem_data.csv", path = "all_waterchem_data.csv", overwrite = TRUE)
drive_download("all_waterquality_data.csv", path = "all_waterquality_data.csv", overwrite = TRUE)

#### Load data into R ####
# omitted rating curve because it's... different
gases <- read.csv("all_dissolvedgases_data.csv")
elevation <- read.csv("all_elevation_data.csv")
fieldQ <- read.csv("all_fielddischarge_data.csv")
gaugeheight <- read.csv("all_gaugeheight_data.csv")
isotopes <- read.csv("all_isotope_data.csv")
nitrate <- read.csv("all_nitrate_data.csv")
temp <- read.csv("all_temp_data.csv")
chem <- read.csv("all_waterchem_data.csv")
wq <- read.csv("all_waterquality_data.csv")

#### Dissolved gases ####
# convert x-axis variable to POSIXct format
gases$collectDate <- as.POSIXct(gases$collectDate)

# methane
ch4 <- ggplot(gases, aes(x = collectDate, y = concentrationCH4)) +
  geom_point() +
  labs(title = "Methane - NEON - Pre-fire",
       x = "Time",
       y = "Methane") +
  theme_minimal()
ch4

ch4_vio <- ggplot(gases, aes(x = collectDate, y = concentrationCH4)) +
  geom_violin() +
  labs(title = "Methane - NEON - Pre-fire",
       x = "Time",
       y = "Methane") +
  theme_minimal()
ch4_vio

# carbon dioxide
co2 <- ggplot(gases, aes(x = collectDate, y = concentrationCO2)) +
  geom_point() +
  labs(title = "Carbon Dioxide - NEON - Pre-fire",
       x = "Time",
       y = "Carbon Dioxide") +
  theme_minimal()
co2

co2_vio <- ggplot(gases, aes(x = collectDate, y = concentrationCO2)) +
  geom_violin() +
  labs(title = "Carbon Dioxide - NEON - Pre-fire",
       x = "Time",
       y = "Carbon Dioxide") +
  theme_minimal()
co2_vio

# nitrous oxide
n2o <- ggplot(gases, aes(x = collectDate, y = concentrationN2O)) +
  geom_point() +
  labs(title = "Nitrous Oxide - NEON - Pre-fire",
       x = "Time",
       y = "Nitrous Oxide") +
  theme_minimal()
n2o

n2o_vio <- ggplot(gases, aes(x = collectDate, y = concentrationN2O)) +
  geom_violin() +
  labs(title = "Carbon Dioxide - NEON - Pre-fire",
       x = "Time",
       y = "Nitrous Oxide") +
  theme_minimal()
n2o_vio

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(ch4, ch4_vio, co2, co2_vio, n2o, n2o_vio)

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
    path = "https://drive.google.com/drive/u/0/folders/1F4butqJbV59XWKhwMlrDYoW2rZmhZXLR",  # Google Drive folder
    name = basename(f)
  )
})

#### Elevation ####
# convert x-axis variable to POSIXct format
elevation$startDateTime <- as.POSIXct(elevation$startDateTime)
elevation$endDateTime <- as.POSIXct(elevation$endDateTime)

elev <- ggplot(elevation, aes(x = endDateTime, y = surfacewaterElevMean)) +
  geom_point() +
  labs(title = "Surface Water Elevation - NEON - Pre-fire",
       x = "Time",
       y = "Elevation") +
  theme_minimal()
elev

elev_vio <- ggplot(elevation, aes(x = endDateTime, y = surfacewaterElevMean)) +
  geom_violin() +
  labs(title = "Surface Water Elevation - NEON - Pre-fire",
       x = "Time",
       y = "Elevation") +
  theme_minimal()
elev_vio

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(elev, elev_vio)

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
    path = "https://drive.google.com/drive/u/0/folders/1zlxEsaNg3Ywopf6zSTk7SXtxGZOFxKcx",  # Google Drive folder
    name = basename(f)
  )
})

#### Field discharge ####
# convert x-axis variable to POSIXct format
fieldQ$collectDate <- as.POSIXct(fieldQ$collectDate)

# field discharge
field_dis <- ggplot(fieldQ, aes(x = collectDate, y = finalDischarge)) +
  geom_point() +
  labs(title = "Discharge (field measurement) - NEON - Pre-fire",
       x = "Time",
       y = "Discharge") +
  theme_minimal()
field_dis

field_dis_vio <- ggplot(fieldQ, aes(x = collectDate, y = finalDischarge)) +
  geom_violin() +
  labs(title = "Discharge (field measurement) - NEON - Pre-fire",
       x = "Time",
       y = "Discharge") +
  theme_minimal()
field_dis_vio

# stage
field_stage <- ggplot(fieldQ, aes(x = collectDate, y = streamStage)) +
  geom_point() +
  labs(title = "Stream stage - NEON - Pre-fire",
       x = "Time",
       y = "Stage") +
  theme_minimal()
field_stage

field_stage_vio <- ggplot(fieldQ, aes(x = collectDate, y = streamStage)) +
  geom_violin() +
  labs(title = "Stream stage - NEON - Pre-fire",
       x = "Time",
       y = "Stage") +
  theme_minimal()
field_stage_vio

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(field_dis, field_dis_vio, field_stage, field_stage_vio)

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
    path = "https://drive.google.com/drive/u/0/folders/1zcdPthcrbGpALtjAXWvCCLor2Ic5fmS5",  # Google Drive folder
    name = basename(f)
  )
})

#### Gauge height ####
# convert x-axis variable to POSIXct format
gaugeheight$startDate <- as.POSIXct(gaugeheight$startDate)
gaugeheight$endDate <- as.POSIXct(gaugeheight$endDate)

# gauge height
gauge <- ggplot(gaugeheight, aes(x = startDate, y = initialStageHeight)) +
  geom_point() +
  labs(title = "Gauge height - NEON - Pre-fire",
      x = "Time",
      y = "Stage") +
  theme_minimal()
gauge

gauge_vio <- ggplot(gaugeheight, aes(x = startDate, y = initialStageHeight)) +
  geom_violin() +
  labs(title = "Gauge height - NEON - Pre-fire",
       x = "Time",
       y = "Stage") +
  theme_minimal()
gauge_vio

# elevation
gauge_elev <- ggplot(gaugeheight, aes(x = startDate, y = elevation)) +
  geom_point() +
  labs(title = "Elevation - NEON - Pre-fire",
       x = "Time",
       y = "Elevation") +
  theme_minimal()
gauge_elev

gauge_elev_vio <- ggplot(gaugeheight, aes(x = startDate, y = elevation)) +
  geom_violin() +
  labs(title = "Elevation - NEON - Pre-fire",
       x = "Time",
       y = "Elevation") +
  theme_minimal()
gauge_elev_vio

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(gauge, gauge_vio, gauge_elev, gauge_elev_vio)
  
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
    path = "https://drive.google.com/drive/u/0/folders/1Dfv-52BlWPPV1uCEBL3vvPhABTSoVT-C",  # Google Drive folder
    name = basename(f)
  )
})

#### Isotopes ####
# convert x-axis variable to POSIXct format
isotopes$collectDate <- as.POSIXct(isotopes$collectDate)

# d18 - oxygen
d18 <- ggplot(isotopes, aes(x = collectDate, y = d18OWater)) +
  geom_point() +
  labs(title = "d18O - NEON - Pre-fire",
       x = "Time",
       y = "d18O") +
  theme_minimal()
d18

d18_vio <- ggplot(isotopes, aes(x = collectDate, y = d18OWater)) +
  geom_violin() +
  labs(title = "d18O - NEON - Pre-fire",
       x = "Time",
       y = "d18O") +
  theme_minimal()
d18_vio

# d2 - hydrogen
d2 <- ggplot(isotopes, aes(x = collectDate, y = d2HWater)) +
  geom_point() +
  labs(title = "d2H - NEON - Pre-fire",
       x = "Time",
       y = "d2H") +
  theme_minimal()
d2

d2_vio <- ggplot(isotopes, aes(x = collectDate, y = d2HWater)) +
  geom_violin() +
  labs(title = "d2H - NEON - Pre-fire",
       x = "Time",
       y = "d2H") +
  theme_minimal()
d2_vio

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(d18, d18_vio, d2, d2_vio)

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
    path = "https://drive.google.com/drive/u/0/folders/12bGGYQ1_B_Qdn_EfEQRnXuzGn7R7LF9s",  # Google Drive folder
    name = basename(f)
  )
})

#### Nitrates ####
# convert x-axis variable to POSIXct format
nitrate$startDateTime <- as.POSIXct(nitrate$startDateTime)
nitrate$endDateTime <- as.POSIXct(nitrate$endDateTime)

# total nitrogen
n <- ggplot(nitrate, aes(x = endDateTime, y = surfWaterNitrateMean)) +
  geom_point() +
  labs(title = "Nitrates - NEON - Pre-fire",
       x = "Time",
       y = "Nitrates") +
  theme_minimal()
n

n_vio <- ggplot(nitrate, aes(x = endDateTime, y = surfWaterNitrateMean)) +
  geom_violin() +
  labs(title = "Nitrates - NEON - Pre-fire",
       x = "Time",
       y = "Nitrates") +
  theme_minimal()
n_vio

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(n, n_vio)

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
    path = "https://drive.google.com/drive/u/0/folders/1lwp55DiSi7Y3VyacO50yVGWuJD6pXo2c",  # Google Drive folder
    name = basename(f)
  )
})

#### Water temperature ####
# convert x-axis variable to POSIXct format
temp$startDateTime <- as.POSIXct(temp$startDateTime)

# temp
watertemp <- ggplot(temp, aes(x = startDateTime, y = surfacewaterTempMean)) +
  geom_point() +
  labs(title = "Surface Water Temperature - NEON - Pre-fire",
       x = "Time",
       y = "Temperature") +
  theme_minimal()
watertemp

watertemp_vio <- ggplot(temp, aes(x = startDateTime, y = surfacewaterTempMean)) +
  geom_violin() +
  labs(title = "Surface Water Temperature - NEON - Pre-fire",
       x = "Time",
       y = "Temperature") +
  theme_minimal()
watertemp_vio

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(watertemp, watertemp_vio)

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
    path = "https://drive.google.com/drive/u/0/folders/1Z3HUTd1agLaQpqrkW1x6T-PuMhOossMr",  # Google Drive folder
    name = basename(f)
  )
})

#### Water chemistry ####
# convert x-axis variable to POSIXct format
chem$collectDate <- as.POSIXct(chem$collectDate)

all_chem <- ggplot(chem, aes(x = collectDate, y = analyteConcentration, color = analyte)) +
  geom_point() +
  labs(title = "Water Chemistry - NEON - Pre-fire",
       x = "Time",
       y = "Temperature") +
  theme_minimal()
all_chem

# We probably don't need all of these variables...

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(all_chem)

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
    path = "https://drive.google.com/drive/u/0/folders/1O75VnCjDjYevyega1FtWjXGgTvcAR3YI",  # Google Drive folder
    name = basename(f)
  )
})

#### Water quality ####
# convert x-axis variable to POSIXct format
wq$startDateTime <- as.POSIXct(wq$startDateTime)
wq$endDateTime <- as.POSIXct(wq$endDateTime)

# Specific conductance
spc <- ggplot(wq, aes(x = startDateTime, y = specificConductance)) +
  geom_point() +
  labs(title = "SpC - NEON - Pre-fire",
       x = "Time",
       y = "Specific Conductance") +
  theme_minimal()
spc

spc_vio <- ggplot(wq, aes(x = startDateTime, y = specificConductance)) +
  geom_violin() +
  labs(title = "SpC - NEON - Pre-fire",
       x = "Time",
       y = "Specific Conductance") +
  theme_minimal()
spc_vio

# Dissolved oxygen
do <- ggplot(wq, aes(x = startDateTime, y = dissolvedOxygen)) +
  geom_point() +
  labs(title = "D.O. - NEON - Pre-fire",
       x = "Time",
       y = "Dissolved Oxygen") +
  theme_minimal()
do

do_vio <- ggplot(wq, aes(x = startDateTime, y = dissolvedOxygen)) +
  geom_violin() +
  labs(title = "D.O. - NEON - Pre-fire",
       x = "Time",
       y = "Dissolved Oxygen") +
  theme_minimal()
do_vio

# pH
ph <- ggplot(wq, aes(x = startDateTime, y = pH)) +
  geom_point() +
  labs(title = "pH - NEON - Pre-fire",
       x = "Time",
       y = "pH") +
  theme_minimal()
ph

ph_vio <- ggplot(wq, aes(x = startDateTime, y = pH)) +
  geom_violin() +
  labs(title = "pH - NEON - Pre-fire",
       x = "Time",
       y = "pH") +
  theme_minimal()
ph_vio

# Chlorophyll
chloro <- ggplot(wq, aes(x = startDateTime, y = chlorophyll)) +
  geom_point() +
  labs(title = "Chlorophyll - NEON - Pre-fire",
       x = "Time",
       y = "Chlorophyll") +
  theme_minimal()
chloro

chloro_vio <- ggplot(wq, aes(x = startDateTime, y = chlorophyll)) +
  geom_violin() +
  labs(title = "Chlorophyll - NEON - Pre-fire",
       x = "Time",
       y = "Chlorophyll") +
  theme_minimal()
chloro_vio

# Turbidity
turb <- ggplot(wq, aes(x = startDateTime, y = turbidity)) +
  geom_point() +
  labs(title = "Turbidity - NEON - Pre-fire",
       x = "Time",
       y = "Turbidity") +
  theme_minimal()
turb

turb_vio <- ggplot(wq, aes(x = startDateTime, y = turbidity)) +
  geom_violin() +
  labs(title = "Turbidity - NEON - Pre-fire",
       x = "Time",
       y = "Turbidity") +
  theme_minimal()
turb_vio

# fDOM
fdom <- ggplot(wq, aes(x = startDateTime, y = fDOM)) +
  geom_point() +
  labs(title = "fDOM - NEON - Pre-fire",
       x = "Time",
       y = "fDOM") +
  theme_minimal()
fdom

fdom_vio <- ggplot(wq, aes(x = startDateTime, y = fDOM)) +
  geom_violin() +
  labs(title = "fDOM - NEON - Pre-fire",
       x = "Time",
       y = "fDOM") +
  theme_minimal()
fdom_vio

#### Save plots to Google Drive ####
# clear the local folder we used so it can be used elsewhere
files <- list.files(path = "plots", full.names = TRUE)
file.remove(files)

plots <- list(spc, spc_vio, do, do_vio, ph, ph_vio, chloro, chloro_vio,
              turb, turb_vio, fdom, fdom_vio)

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
    path = "https://drive.google.com/drive/u/0/folders/115olzrtHOBqg9NTxFCHadblt6GIrk2gu",  # Google Drive folder
    name = basename(f)
  )
})