#Can download shp file here (not official UN data): https://thematicmapping.org/downloads/world_borders.php
#Can download the official UN shpfile here (but it only goes to region, not the subregions that we use): https://data.unhabitat.org/datasets/GUO-UN-Habitat::m49-regions/about
#Can download the UN country to region table here: https://unstats.un.org/unsd/methodology/m49/overview/
# https://data.unhabitat.org/search?collection=Dataset&q=M49%20regions

#Import libraries
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggplot2)

#Basically we take the countries shpfile, merge it with official UN data to get the regions,
#then merge our counts csv to get the final figure

#This code is the wrangling to achieve that
# Read the countries shapefile 
shp_data <- st_read("D:/IPBES_review/data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
# Rename the column in shp_data to match the UN table
shp_data <- shp_data %>%
  rename(ISO.alpha3.Code = ISO3)
# Read the UN table file into a data frame
csv_data <- read.csv("D:/IPBES_review/data/UNSD — Methodology.csv", sep = ";")
#Merge together
merged_data <- merge(shp_data, csv_data, by = "ISO.alpha3.Code")

# Read in the region counts from review
region_counts <- read.csv("D:/IPBES_review/nexus-4.3-review/regions_counts.csv")
# Rename region column name to match UN table
colnames(region_counts)[colnames(region_counts) == "Region"] <- "Sub.region.Name"
#Merge together
merged_data <- merge(merged_data, region_counts, by = "Sub.region.Name")

# Define the color scale from red (highest) to yellow (lowest)
color_scale <- scale_fill_gradient(low = "yellow", high = "red")

# Plot the shapefile without displaying country polygon borders
my_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = n), color = "NA") +  
  color_scale +
  labs(title = "Number of studies in each region", fill = "Count") +
  theme_minimal()


# Export the ggplot as a PNG image
ggsave(filename = "D:/IPBES_review/nexus-4.3-review/studies_region_counts.png", plot = my_plot, width = 6, height = 4, dpi = 300)

