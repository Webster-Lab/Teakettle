### KREW Burn Severity
library(sf)
library(tidyverse)
library(ggplot2)
library(googledrive)
library(dplyr)
library(viridis)
library(terra)
library(ggrepel)



#### Load data ####

#First authorize google drive
drive_auth(scopes = "https://www.googleapis.com/auth/drive")



file <- drive_get("krew_watersheds.gpkg")

drive_download(file, path = "krew_watersheds.gpkg", overwrite = TRUE)

my_data <- st_read("krew_watersheds.gpkg")

#filter out just the polygons in the Teakettle area that burned (other polygons up by Dinkey Creek)
my_data <- my_data %>%
  filter(watershed == "B201"| watershed == "B203" | watershed == "B204"| watershed == "B200"| watershed == "T003")

plot(my_data)


#Alright now we want to bring in the RdNBR Composite Burn Index Raster
#I downloaded this data for the Garnet fire from this link: https://burnseverity.cr.usgs.gov/ravg/data-access
#Relativized differenced Normalized Burn Ratio (RdNBR) – a normalized version of the dNBR that removes the biasing effect of the pre-fire conditions (Miller et al 2009). The RdNBR is calculated as:
#RdNBR = dNBR / SquareRoot(ABS(NBR pre-fire / 1000))
#Composite Burn Index: A numerical, synoptic rating calculated from a field-based estimate of fire effects on individual strata within a plot or site in a burned area (Composite Burn Index | Burn Severity Portal). Estimates the overall impact to a site based on post-fire conditions averaged across the burnable portion of the site

r <- drive_get("ca3686111904320250824_20241009_20251012_rdnbr_cbi.tif")

drive_download(r, path = "temp.tif", overwrite = TRUE)

r <- rast("temp.tif")  # raster


# Mask values (set to NA) These are due to clouds/smoke/fire etc.
r[r %in% c(-9, -9999)] <- NA

# Create classification matrix
# Columns: from, to, class value
#Based of value classifications from here: https://burnseverity.cr.usgs.gov/ravg/background-products-applications
#Slightly negative values are unburned areas with a slight increase in greenness post-fire
m <- matrix(c(
  -1,     0.1,   1,   # unchanged
  0.1,   1.25,  2,   # low severity
  1.25,  2.25,  3,   # moderate severity
  2.25,  3.0,   4    # high severity
), ncol = 3, byrow = TRUE)

# Apply classification
r_class <- classify(r, m)
  

#Now lets crop the raster to the watershed layer
#But first we have to make it compatible with terra
watersheds <- vect(my_data)
#and then project it to match the raster
watersheds <- project(watersheds, crs(r_class))

#alright now we crop the raster
r_crop <- crop(r_class, watersheds)



# Convert raster to data frame for ggplot
r_df <- as.data.frame(r_crop, xy = TRUE)
colnames(r_df)[3] <- "CBI_class"

# Map numeric classes to descriptive labels
r_df <- r_df %>%
  mutate(CBI_class = factor(CBI_class,
                            levels = 1:4,
                            labels = c("Unchanged", "Low", "Moderate", "High")))

# Define colors
cbi_colors <- c("Unchanged" = "#CDF2A7",
                "Low" = "#F0E590",
                "Moderate" = "#E89A41",
                "High" = "#C23123")



subwatersheds_sf <- st_transform(my_data, crs(r_crop))
subwatersheds_vect <- vect(subwatersheds_sf)

#order by area (smallest watersheds plot on top! So they are visible)
subwatersheds_sf <- subwatersheds_sf %>%
  mutate(area = st_area(.)) %>%
  arrange(desc(area))


#Make a nice plot.  I had labels for the subwatersheds but it looked a mess, even with the ggrepel.  Hard when they are nested
ggplot() +
  # Raster layer
  geom_raster(data = r_df, aes(x = x, y = y, fill = CBI_class)) +
  scale_fill_manual(values = cbi_colors) +
  
  # Watersheds polygons on top
  geom_sf(data = subwatersheds_sf,
          color = "black", alpha = 0, linewidth = 1) +
  
  geom_text_repel(
   data = st_coordinates(st_centroid(subwatersheds_sf)) %>%
      as.data.frame() %>%
      cbind(watershed = subwatersheds_sf$watershed),
    aes(X, Y, label = watershed),
    size = 4
  ) +
  
  theme_minimal() +
  labs(title = "RdNBR Composite Burn Index with Subwatersheds",
       fill = "Composite Burn Index")


# Save the raster CBI map
ggsave(
  filename = "output/KREW_Composite_Burn_Index.png",
  plot = last_plot(),      # or assign your ggplot object to a variable
  width = 10,              # width in inches
  height = 8,              # height in inches
  dpi = 300
)

### Alright, what percent of each subwatershed burned at high severity?

# Make sure subwatersheds_sf is in the same CRS as raster
subwatersheds_sf <- st_transform(subwatersheds_sf, crs(r_crop))

# Convert to terra vector for extraction
subwatersheds_vect <- vect(subwatersheds_sf)

# Assign a unique ID for each polygon
subwatersheds_sf <- subwatersheds_sf %>%
  mutate(ID = row_number())

subwatersheds_vect <- vect(subwatersheds_sf)  # make sure the vector has the same IDs

# Extract raster values with coverage fraction (weights).  Allows us to weight cells that are only partially in the subwatershed lower
extracted <- terra::extract(r_crop, subwatersheds_vect, weights = TRUE)

# Rename the raster column to a simple name
colnames(extracted)[2] <- "CBI_class"  # second column is the raster value

# Compute weighted high-severity proportion
percent_high_df <- extracted %>%
  mutate(high_weighted = ifelse(CBI_class == 4, weight, 0)) %>%
  group_by(ID) %>%
  summarise(
    perc_high = sum(high_weighted, na.rm = TRUE) / sum(weight, na.rm = TRUE) * 100
  )

# Join back to the sf object by ID
subwatersheds_sf <- subwatersheds_sf %>%
  left_join(percent_high_df, by = "ID")

# Inspect
subwatersheds_sf %>%
  st_drop_geometry() %>%
  select(watershed, perc_high)

subwatersheds_sf <- subwatersheds_sf %>%
  rename(area_m2 = area, percent_high_severity = perc_high)

KREW_subwatersheds_table <- st_drop_geometry(subwatersheds_sf)


# Export to CSV
write.csv(KREW_subwatersheds_table, file = "output/KREW_subwatersheds_summary.csv", row.names = FALSE)

#Make a little histogram of this data
ggplot(subwatersheds_sf, aes(x = percent_high_severity)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  geom_vline(
    xintercept = median(subwatersheds_sf$percent_high_severity, na.rm = TRUE),
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    x = "Percent of watershed burned at high severity",
    y = "Watershed count"
  ) +
  theme_minimal()

ggsave(
  filename = "output/KREW_Burn_severity_histogram.png",
  plot = last_plot(),      # or assign your ggplot object to a variable
  width = 10,              # width in inches
  height = 8,              # height in inches
  dpi = 300
)


##Ok now color each watershed by percent burned at high severity:

ggplot() +
  geom_sf(data = subwatersheds_sf,
          aes(fill = percent_high_severity),
          color = "black",
          linewidth = 0.4, alpha = 1) +
  
  scale_fill_gradientn(
    colours = c("yellow", "orange", "red"),
    name = "% High Severity"
  ) +
  
  theme_minimal() +
  labs(title = "Percent of Subwatershed Burned at High Severity (CBI)")


# Save the raster CBI map
ggsave(
  filename = "output/KREW_Percent_Subwatershed_burned_HS_CBI.png",
  plot = last_plot(),      # or assign your ggplot object to a variable
  width = 10,              # width in inches
  height = 8,              # height in inches
  dpi = 300
)
