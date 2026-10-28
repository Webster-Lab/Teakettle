#Additional Brown's Lines (Webster Lab) Calculations
#Author: Carolina May
#Last Updated: 2026-10-20
#Copied from Hurteau Lab NAS 

### Packages & Working Directory ####


library(dplyr)
library(tidyverse)
library(ggplot2)
library(googledrive)

#Use Ctrl + Shift + O to see script overview by section

### Data Download, Cleaning, & Merging ####

#Download from Google Drive
drive_auth()

#designate where to store outputs and create upload function
google_drive_outputs <- drive_get(as_id("1kAl5ASxI4XlM5YA3sXrcRfdk0joRFkYC"))

# Helper: upload (or update) a single file
upload_output <- function(path, folder = google_drive_outputs) {
  drive_put(media = path,
            path = folder,
            name = basename(path))
}


folder <- as_id("12iF1t9FRETTb08eRjNvXjJkYHZGOTMeu")
files <- drive_ls(folder)
file <- files[files$name == "Additional_Fuels_2026_Compiled.csv", ]

drive_download(file, 
               path = "Additional_Fuels_2026_Compiled.csv", 
               overwrite = TRUE)

file <- files[files$name == "Additional_Fuels_2025_Compiled.csv", ]

drive_download(file, 
               path = "Additional_Fuels_2025_Compiled.csv", 
               overwrite = TRUE)

#Read in cleaned and compiled Fuels Data 
fuel_2025 <- read.csv("Additional_Fuels_2025_Compiled.csv")
fuel_2026 <- read.csv("Additional_Fuels_2026_Compiled.csv")

#lets separate out fine fuels and logs, and clean up column names first

fine_fuels25 <- fuel_2025 %>%
  filter(!is.na(Date)) %>%                      # first row of each transect
  transmute(
    year = 2025,
    date = mdy(Date),
    transect = as.integer(Transect),
    bearing = as.numeric(Bearing),
    hr1 = as.numeric(`X1hr`), hr10 = as.numeric(`X10hr`),
    hr100 = as.numeric(`X100hr`),
    littduff_0.5 = as.numeric(`X0.5m_Litter_Duff`),
    littduff_1.5 = as.numeric(`X1.5m_Litter_Duff`),
    h30 = as.numeric(Height.30cm), h60 = as.numeric(Height.60cm),
    h90 = as.numeric(Height.90cm)
  )

logs25 <- fuel_2025 %>%
  filter(!is.na(`Diameter..cm.`)) %>%
  transmute(
    year = 2025,
    transect = as.integer(Transect),
    diam = as.numeric(`Diameter..cm.`),
    spec = toupper(str_trim(`Species.Code`)),
    decay = as.integer(`Decay.Class`)
  )

fine_fuels26 <- fuel_2026 %>%
  transmute(
    year = 2026,
    date = mdy(Date),
    transect = as.integer(Transect),
    bearing = as.numeric(Bearing),
    hr1 = as.numeric(`X1hr`), hr10 = as.numeric(`X10hr`),
    hr100 = as.numeric(`X100hr`),
    littduff_0.5 = as.numeric(`X0.5m_Litter_Duff`),
    littduff_1.5 = as.numeric(`X1.5m_Litter_Duff`),
    h30 = as.numeric(Height.30cm), h60 = as.numeric(Height.60cm),
    h90 = as.numeric(Height.90cm),
    cone1 = as.numeric(`Cone.I`), cone2 = as.numeric(`Cone.II`),
    cone3 = as.numeric(`Cone.III`), cone4 = as.numeric(`Cone.IV`),
    cone5 = as.numeric(`Cone.V`), cone6 = as.numeric(`Cone.VI`),
    notes = Notes
  )

logs26 <- fuel_2026 %>%
  select(Transect, Bearing,
         matches("^(Diam|Spec|Decay)[1-4]$")) %>%
  pivot_longer(matches("^(Diam|Spec|Decay)[1-4]$"),
               names_to = c(".value", "slot"),
               names_pattern = "(Diam|Spec|Decay)([1-4])") %>%
  filter(!is.na(Diam)) %>%
  transmute(
    year = 2026,
    transect = as.integer(Transect),
    diam = as.numeric(Diam),
    spec = recode(toupper(Spec), UNKN = "UNK"),
    decay = as.integer(Decay)
  )

#combine these dataframes 
fine_fuels <- bind_rows(fine_fuels25, fine_fuels26)

fine_fuels <-fine_fuels %>%
  filter(!is.na(bearing))

#combine logs and recode UNK and ABIES SP. to UNKN (no coefficients for abies sp.)
logs  <- bind_rows(logs25, logs26) %>%
  mutate(spec = recode(spec, UNK = "UNKN", ABIES = "UNKN", PINUS = "PISP"))




### Get overstory proportions & associated values set up 

#pull out overstory proptions and clean up columns
overstory <- fuel_2025 %>%
  select(Primary.Overstory, Secondary.Overstory, Primary.Overstory.., Secondary.Overstory.., Transect)%>%
  transmute(primary_percent = as.numeric(Primary.Overstory..), secondary_percent = as.numeric(Secondary.Overstory..),
            primary_spec = as.character(Primary.Overstory), secondary_spec = as.character(Secondary.Overstory), transect = as.integer(Transect))%>%
  filter(!is.na(primary_percent))
#pivot longer
overstory_long <- overstory %>%
  pivot_longer(
    cols = c(primary_percent, secondary_percent, primary_spec, secondary_spec),
    names_to = c("rank", ".value"),
    names_sep = "_"
  ) %>%
  mutate(rank = factor(rank, levels = c("primary", "secondary"))) %>%
  arrange(transect, rank)

data <- overstory_long %>%
  rename(Species = spec)

#Create Coefficient columns
#QMD is Quadratic Mean Diameter
#SG is Specific Gravity
#SEC is Secant

data$QMD1 <- 0
data$QMD10 <- 0
data$QMD100 <- 0

data$SG1 <- 0
data$SG10 <- 0
data$SG100 <- 0

data$SEC1 <- 0
data$SEC10 <- 0
data$SEC100 <- 0




#Add Coefficient Values that correspond to each Species 
#Coefficients come from van Wagtendonk et al. 1996
#QMD 1 Hour Constants 
data[which(data$Species == 'CADE'),'QMD1'] <- 0.09

data[which(data$Species == 'ABCO'),'QMD1'] <- 0.08

data[which(data$Species == 'PIJE'),'QMD1'] <- 0.15

data[which(data$Species == 'PILA'),'QMD1'] <- 0.12

data[which(data$Species == 'ABMA'),'QMD1'] <- 0.1

data[which(data$Species == 'QUKE'),'QMD1'] <- 0.018

data[which(data$Species == 'UNKN'),'QMD1'] <- 0.108

data[which(data$Species == 'PISP'),'QMD1'] <- 0.135

#QMD 10 Hour Constants
data[which(data$Species == 'CADE'),'QMD10'] <- 1.23

data[which(data$Species == 'ABCO'),'QMD10'] <- 1.32

data[which(data$Species == 'PIJE'),'QMD10'] <- 1.25

data[which(data$Species == 'PILA'),'QMD10'] <- 1.46

data[which(data$Species == 'ABMA'),'QMD10'] <- 1.32

data[which(data$Species == 'QUKE'),'QMD10'] <- 0.271

data[which(data$Species == 'UNKN'),'QMD10'] <- 1.316

data[which(data$Species == 'PISP'),'QMD10'] <- 1.355

#QMD 100 Hour Constants 
data[which(data$Species == 'CADE'),'QMD100'] <- 20.79

data[which(data$Species == 'ABCO'),'QMD100'] <- 11.56

data[which(data$Species == 'PIJE'),'QMD100'] <- 17.31

data[which(data$Species == 'PILA'),'QMD100'] <- 13.61

data[which(data$Species == 'ABMA'),'QMD100'] <- 16.24

data[which(data$Species == 'QUKE'),'QMD100'] <- 3.172

data[which(data$Species == 'UNKN'),'QMD100'] <- 15.902

data[which(data$Species == 'PISP'),'QMD100'] <- 15.46

#Specific Gravity 1 Hour Constants
data[which(data$Species == 'CADE'),'SG1'] <- 0.59

data[which(data$Species == 'ABCO'),'SG1'] <- 0.53

data[which(data$Species == 'PIJE'),'SG1'] <- 0.53

data[which(data$Species == 'PILA'),'SG1'] <- 0.59

data[which(data$Species == 'ABMA'),'SG1'] <- 0.57

data[which(data$Species == 'QUKE'),'SG1'] <- 0.56

data[which(data$Species == 'UNKN'),'SG1'] <- 0.562

data[which(data$Species == 'PISP'),'SG1'] <- 0.56

#Specific Gravity 10 Hour Constants
data[which(data$Species == 'CADE'),'SG10'] <- 0.54

data[which(data$Species == 'ABCO'),'SG10'] <- 0.54

data[which(data$Species == 'PIJE'),'SG10'] <- 0.55

data[which(data$Species == 'PILA'),'SG10'] <- 0.59

data[which(data$Species == 'ABMA'),'SG10'] <- 0.56

data[which(data$Species == 'QUKE'),'SG10'] <- 0.56

data[which(data$Species == 'UNKN'),'SG10'] <- 0.556

data[which(data$Species == 'PISP'),'SG10'] <- 0.57

#Specific Gravity 100 Hour Constants
data[which(data$Species == 'CADE'),'SG100'] <- 0.55

data[which(data$Species == 'ABCO'),'SG100'] <- 0.57

data[which(data$Species == 'PIJE'),'SG100'] <- 0.55

data[which(data$Species == 'PILA'),'SG100'] <- 0.52

data[which(data$Species == 'ABMA'),'SG100'] <- 0.47

data[which(data$Species == 'QUKE'),'SG100'] <- 0.56

data[which(data$Species == 'UNKN'),'SG100'] <- 0.532

data[which(data$Species == 'PISP'),'SG100'] <- 0.535

#Secant 1 Hour Constants for Composition 1
data[which(data$Species == 'CADE'),'SEC1'] <- 1.02

data[which(data$Species == 'ABCO'),'SEC1'] <- 1.03

data[which(data$Species == 'PIJE'),'SEC1'] <- 1.03

data[which(data$Species == 'PILA'),'SEC1'] <- 1.04

data[which(data$Species == 'ABMA'),'SEC1'] <- 1.03

data[which(data$Species == 'QUKE'),'SEC1'] <- 1.03

data[which(data$Species == 'UNKN'),'SEC1'] <- 1.03

data[which(data$Species == 'PISP'),'SEC1'] <- 1.035

#Secant 10 Hour Constants
data[which(data$Species == 'CADE'),'SEC10'] <- 1.02

data[which(data$Species == 'ABCO'),'SEC10'] <- 1.02

data[which(data$Species == 'PIJE'),'SEC10'] <- 1.03

data[which(data$Species == 'PILA'),'SEC10'] <- 1.04

data[which(data$Species == 'ABMA'),'SEC10'] <- 1.02

data[which(data$Species == 'QUKE'),'SEC10'] <- 1.02

data[which(data$Species == 'UNKN'),'SEC10'] <- 1.026

data[which(data$Species == 'PISP'),'SEC10'] <- 1.035


#Secant 100 Hour Constants
data[which(data$Species == 'CADE'),'SEC100'] <- 1.03

data[which(data$Species == 'ABCO'),'SEC100'] <- 1.02

data[which(data$Species == 'PIJE'),'SEC100'] <- 1.04

data[which(data$Species == 'PILA'),'SEC100'] <- 1.03

data[which(data$Species == 'ABMA'),'SEC100'] <- 1.01

data[which(data$Species == 'QUKE'),'SEC100'] <- 1.03

data[which(data$Species == 'UNKN'),'SEC100'] <- 1.026

data[which(data$Species == 'PISP'),'SEC100'] <- 1.035


#Weight column by overstory proprtion (percent/100)
data <- data %>% mutate_at(vars(starts_with("QMD")), ~.*(percent/100))
data <- data %>% mutate_at(vars(starts_with("SEC")), ~.*(percent/100))
data <- data %>% mutate_at(vars(starts_with("SG")), ~.*(percent/100))


#group by Transect and sum each weighted column
fwd.coeff <- aggregate(cbind(QMD1, QMD10, QMD100, SG1, SG10, SG100, SEC1, SEC10, SEC100) ~ transect, data, sum)



### Fine Fuels Calculations ####

#Calculate average litter + duff depth, biomass, and carbon per transect

fine_fuels$littduff.ave <- (fine_fuels$littduff_1.5 + fine_fuels$littduff_0.5)/2 

fine_fuels$littduff.biomass <- ((fine_fuels$littduff.ave*1.5286)*10) 
fine_fuels$littduff.carbon <- fine_fuels$littduff.biomass*0.37


#Add Measurement Distances for each Fuel Class (2 meters for 1 hour, 2 meters for 10 hour and 4 meters for 100 hour)
fine_fuels$Dist1hour <- 2
fine_fuels$Dist10hour <- 2
fine_fuels$Dist100hour <- 4

#merge fine fuels with overstory carbon coefficients by transect
fine_fuels<- merge(fine_fuels, fwd.coeff, by= "transect")


#Calculate weight using coefficients and distance
fine_fuels$Weight1hour = ((1.257359332*fine_fuels$hr1*fine_fuels$QMD1*fine_fuels$SG1*fine_fuels$SEC1)/fine_fuels$Dist1hour)

fine_fuels$Weight10hour = ((1.257359332*fine_fuels$hr10*fine_fuels$QMD10*fine_fuels$SG10*fine_fuels$SEC10)/fine_fuels$Dist10hour)

fine_fuels$Weight100hour = ((1.257359332*fine_fuels$hr100*fine_fuels$QMD100*fine_fuels$SG100*fine_fuels$SEC100)/fine_fuels$Dist100hour)

#Half of dry wood biomass is carbon (0.5 is carbon fraction of dry wood)
fine_fuels$Carbon1hour = (fine_fuels$Weight1hour*0.5)
fine_fuels$Carbon10hour = (fine_fuels$Weight10hour*0.5)
fine_fuels$Carbon100hour = (fine_fuels$Weight100hour*0.5)






### CWD Calculations ####

#Get coefficients for each log species and decay class

logs[which(logs$spec == 'ABCO' & logs$decay == '1'),'coefficient'] <- 0.369

logs[which(logs$spec == 'ABCO' & logs$decay == '2'),'coefficient'] <- 0.305

logs[which(logs$spec == 'ABCO' & logs$decay == '3'),'coefficient'] <- 0.212

logs[which(logs$spec == 'ABCO' & logs$decay == '4'),'coefficient'] <- 0.178

logs[which(logs$spec == 'ABCO' & logs$decay == '5'),'coefficient'] <- 0.178

#Coefficients for UNKN

logs[which(logs$spec == 'UNKN' & logs$decay == '1'),'coefficient'] <- 0.382

logs[which(logs$spec == 'UNKN' & logs$decay == '2'),'coefficient'] <- 0.318

logs[which(logs$spec == 'UNKN' & logs$decay == '3'),'coefficient'] <- 0.276


logs[which(logs$spec == 'UNKN' & logs$decay == '4'),'coefficient'] <- 0.159

logs[which(logs$spec == 'UNKN' & logs$decay == '5'),'coefficient'] <- 0.159

#Coefficients for ABMA

logs[which(logs$spec == 'ABMA' & logs$decay == '1'),'coefficient'] <- 0.369

logs[which(logs$spec == 'ABMA' & logs$decay == '2'),'coefficient'] <- 0.305


logs[which(logs$spec == 'ABMA' & logs$decay == '3'),'coefficient'] <- 0.212

logs[which(logs$spec == 'ABMA' & logs$decay == '4'),'coefficient'] <- 0.178

logs[which(logs$spec == 'ABMA' & logs$decay == '5'),'coefficient'] <- 0.178

#Coefficients for ABSP
logs[which(logs$spec == 'ABSP' & logs$decay == '1'),'coefficient'] <- 0.369

logs[which(logs$spec == 'ABSP' & logs$decay == '2'),'coefficient'] <- 0.305


logs[which(logs$spec == 'ABSP' & logs$decay == '3'),'coefficient'] <- 0.212

logs[which(logs$spec == 'ABSP' & logs$decay == '4'),'coefficient'] <- 0.178

logs[which(logs$spec == 'ABSP' & logs$decay == '5'),'coefficient'] <- 0.178


#Coefficients for CADE

logs[which(logs$spec == 'CADE' & logs$decay == '1'),'coefficient'] <- 0.425

logs[which(logs$spec == 'CADE' & logs$decay == '2'),'coefficient'] <- 0.356

logs[which(logs$spec == 'CADE' & logs$decay == '3'),'coefficient'] <- 0.331

logs[which(logs$spec == 'CADE' & logs$decay == '4'),'coefficient'] <- 0.156

logs[which(logs$spec == 'CADE' & logs$decay == '5'),'coefficient'] <- 0.156

#Coefficients for PIJE

logs[which(logs$spec == 'PIJE' & logs$decay == '1'),'coefficient'] <- 0.365

logs[which(logs$spec == 'PIJE' & logs$decay == '2'),'coefficient'] <- 0.343


logs[which(logs$spec == 'PIJE' & logs$decay == '3'),'coefficient'] <- 0.340

logs[which(logs$spec == 'PIJE' & logs$decay == '4'),'coefficient'] <- 0.176

logs[which(logs$spec == 'PIJE' & logs$decay == '5'),'coefficient'] <- 0.176

#Coefficients for PILA

logs[which(logs$spec == 'PILA' & logs$decay == '1'),'coefficient'] <- 0.369

logs[which(logs$spec == 'PILA' & logs$decay == '2'),'coefficient'] <- 0.269

logs[which(logs$spec == 'PILA' & logs$decay == '3'),'coefficient'] <- 0.221

logs[which(logs$spec == 'PILA' & logs$decay == '4'),'coefficient'] <- 0.125

logs[which(logs$spec == 'PILA' & logs$decay == '5'),'coefficient'] <- 0.125

#Coefficients for PISP

logs[which(logs$spec == 'PISP' & logs$decay == '1'),'coefficient'] <- 0.369

logs[which(logs$spec == 'PISP' & logs$decay == '2'),'coefficient'] <- 0.269

logs[which(logs$spec == 'PISP' & logs$decay == '3'),'coefficient'] <- 0.221

logs[which(logs$spec == 'PISP' & logs$decay == '4'),'coefficient'] <- 0.125

logs[which(logs$spec == 'PISP' & logs$decay == '5'),'coefficient'] <- 0.125

#Coefficients for QUKE

logs[which(logs$spec == 'QUKE' & logs$decay == '1'),'coefficient'] <- 0.444

logs[which(logs$spec == 'QUKE' & logs$decay == '2'),'coefficient'] <- 0.353

logs[which(logs$spec == 'QUKE' & logs$decay == '3'),'coefficient'] <- 0.289

logs[which(logs$spec == 'QUKE' & logs$decay == '4'),'coefficient'] <- 0.159

logs[which(logs$spec == 'QUKE' & logs$decay == '5'),'coefficient'] <- 0.159


logs$biomass <- NA

#calculate biomass and carbon

logs$diam <- as.numeric(logs$diam)

#Note: Since these transects are shorter, divide by 15 m rather than 25 m (as done for the plot fuels)
logs$biomass <- ((1.257359332*((logs$diam)^2)*logs$coefficient)/15)

logs$carbon <- logs$biomass/2



#Save output of Carbon Calculations for each log
write.table(logs, "output/add_logcalculations2025.2026.csv", sep=",", row.names=FALSE)

#Group Logs by New Logs, Sound Logs (Class 1-3), and Rotten Logs (Class 4-5)
logs$Group <- NA
logs[which( logs$decay < 4), 'Group'] <- "Sound"
logs[which(logs$decay > 3), 'Group'] <- "Rotten"

#Calculate Transect level Biomass by Summing all logs per transect per year
cwd.ave <- aggregate(biomass ~ transect + Group + year, logs, sum)

#Add CWD Data to Fuels Dataframe -- first pivot wider for easier merge

cwd.wide <- cwd.ave %>%
  pivot_wider(names_from = Group, values_from = biomass,
              names_prefix = "CWD.Biomass.", values_fill = 0)

total_fuels <- left_join(fine_fuels, cwd.wide, by = c("transect", "year"))

#where the join creates NAs, make these 0s.  There were no CWD on some transects
total_fuels$CWD.Biomass.Sound[is.na(total_fuels$CWD.Biomass.Sound)] <- 0
total_fuels$CWD.Biomass.Rotten[is.na(total_fuels$CWD.Biomass.Rotten)] <- 0

total_fuels$CWD.Carbon.Sound <- total_fuels$CWD.Biomass.Sound/2
total_fuels$CWD.Carbon.Rotten <- total_fuels$CWD.Biomass.Rotten/2



#select carbon/biomass columns

carbon <- total_fuels %>%
  select(transect, year,
         littduff.biomass, littduff.carbon,
         Weight1hour, Weight10hour, Weight100hour,
         Carbon1hour, Carbon10hour, Carbon100hour,
         CWD.Biomass.Sound, CWD.Carbon.Sound,
         CWD.Biomass.Rotten, CWD.Carbon.Rotten)
carbon$CWD.Biomass <- carbon$CWD.Biomass.Rotten + carbon$CWD.Biomass.Sound
carbon$CWD.Carbon<- carbon$CWD.Carbon.Rotten + carbon$CWD.Carbon.Sound

write.table(carbon, "output/add.fuel.carbon.2026.csv", sep=",", row.names=FALSE)
upload_output("output/add.fuel.carbon.2026.csv")


### Plot Data for Comparison ####

#first summarize

biomass_summary <- carbon %>%   # swap in total_fuels if that's your combined data frame
  # Treat "no logs found" as zero so those transects count in the average
  mutate(across(starts_with("CWD.Biomass"), ~ replace_na(.x, 0))) %>%
  pivot_longer(c(littduff.biomass, Weight1hour, Weight10hour, Weight100hour,
                 CWD.Biomass.Sound, CWD.Biomass.Rotten),
               names_to = "fuel_type",
               values_to = "biomass") %>%
  mutate(fuel_type = factor(fuel_type,
                            levels = c("littduff.biomass", "Weight1hour", "Weight10hour",
                                       "Weight100hour", "CWD.Biomass.Sound", "CWD.Biomass.Rotten"),
                            labels = c("Litter/Duff", "1-hr", "10-hr", "100-hr",
                                       "CWD Sound", "CWD Rotten")),
         year = factor(year)) %>%
  group_by(year, fuel_type) %>%
  summarise(mean_b = mean(biomass, na.rm = TRUE),
            se_b   = sd(biomass, na.rm = TRUE) / sqrt(sum(!is.na(biomass))),
            n      = sum(!is.na(biomass)),
            .groups = "drop")

p <- ggplot(biomass_summary, aes(x = fuel_type, y = mean_b, fill = year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_b - se_b, ymax = mean_b + se_b),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Fuel type",
       y = "Mean biomass (Mg/ha)",
       fill = "Year",
       title = "Mean fuel biomass by fuel type and year",
       caption = "Error bars show ± 1 SE") +
  theme_minimal(base_size = 13)

ggsave("output/additional_fuels_summary.png", plot = p,
       width = 8, height = 5, dpi = 300, bg = "white")

upload_output("output/additional_fuels_summary.png")

### Pair Transects and Calculate Difference ####

#Lets get the total biomass and carbon per transect for each year
totals <- carbon %>%
  mutate(total_biomass = littduff.biomass + Weight1hour + Weight10hour +
           Weight100hour + CWD.Biomass,
         total_carbon  = littduff.carbon + Carbon1hour + Carbon10hour +
           Carbon100hour + CWD.Carbon) %>%
  select(transect, year, total_biomass, total_carbon)

# 2. Pivot wide and calculate change (2026 - 2025) 
totals_wide <- totals %>%
  pivot_wider(names_from = year,
              values_from = c(total_biomass, total_carbon)) %>%
  mutate(biomass_change = total_biomass_2026 - total_biomass_2025,
         carbon_change  = total_carbon_2026  - total_carbon_2025,
         carbon_pct_change = 100 * carbon_change / total_carbon_2025)

# Keep only transects measured in both years (Transect 6 was removed in 2025 and not resurveyed)
totals_paired <- totals_wide %>%
  filter(!is.na(biomass_change))

write.table(totals_paired, "output/add.paired.fuel.diff.csv", sep=",", row.names=FALSE)
upload_output("output/add.paired.fuel.diff.csv")




#### Clean Up Repo ####
# 1. Free-floating CSVs in the working directory (not in subfolders)
loose_csvs <- list.files(pattern = "\\.csv$", full.names = TRUE)

# 2. Everything inside the output folder (including subfolders)
output_files <- list.files("output", full.names = TRUE, recursive = TRUE, all.files = TRUE)


file.remove(loose_csvs)
file.remove(output_files)

