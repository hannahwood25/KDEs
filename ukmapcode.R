#### Map of colonies


setwd("/Users/hannahwood/Desktop/map")

library(track2KBA) # load package
library(tidyverse)
# Load library for multiple plots
library(patchwork)
library(terra)
library(readr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggmap")
library(ggmap)
library(rnaturalearth)


FAME_CombinedBLKI_wTripIDs <- read_csv("/Users/hannahwood/Desktop/OMGPhD/PhD data/FAME_CombinedBLKI_wTripIDs.csv")

kitt <- FAME_CombinedBLKI_wTripIDs


kitt_colony = kitt %>%
  group_by(Site) %>%
  filter(TripID == "COLONY") %>%
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude))

write.csv(kitt_colony, "colony_locations.csv")

colony_data <- read.csv("colony_locations.csv")


########

#### chat gpt attempt

library(sf)  # for reading shapefiles
library(dplyr) 

# Read the UK coastline shapefile
uk_coastline <- st_read("/Users/hannahwood/Desktop/map/shapefiles/GBR_adm/GBR_adm0.shp")

ggplot() +
  geom_sf(data = uk_coastline, color = "blue", size = 0.5) +
  geom_point(data = colony_data, aes(x = Longitude, y = Latitude), size = 3, color = "red") +
  labs(title = "GPS Locations and UK Coastline")


### Adding location names
ggplot() +
  geom_sf(data = uk_coastline, color = "blue", size = 0.5) +
  geom_point(data = colony_data, aes(x = Longitude, y = Latitude), size = 3, color = "red") +
  geom_text(data = colony_data, aes(x = Longitude, y = Latitude, label = Location_Name), size = 3, vjust = 2) +
  labs(title = "GPS Locations and UK Coastline") 

### colour code locations

ggplot() +
  geom_sf(data = uk_coastline, color = "blue", size = 0.5) +
  geom_point(data = colony_data, aes(x = Longitude, y = Latitude, color = Individ), size = 3) +
  geom_text(data = colony_data, aes(x = Longitude, y = Latitude, label = Location_Name), size = 3, vjust = 2) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "GPS Locations and UK Coastline") 

plot(coast2, border="gray66", col="white", add=T, lwd=2)# UK

### colour code locations

ggplot() +
  geom_sf(data = uk_coastline, color = "grey", size = 0.5) +
  geom_point(data = colony_data, aes(x = Longitude, y = Latitude, color = Individ), size = 3) +
  geom_text(data = colony_data, aes(x = Longitude, y = Latitude, label = Location_Name), size = 3, vjust = 2) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "GPS Locations and UK Coastline") 

####

ggplot() +
  geom_sf(data = uk_coastline, color = "grey", size = 0.5) +
  geom_point(data = colony_data, aes(x = Longitude, y = Latitude, color = Individ), size = 3) +
  geom_text(
    data = colony_data,
    aes(x = Longitude + 0.3, y = Latitude, label = Location_Name),
    size = 3,  # Adjust the font size as needed
    vjust = 0.5,  # Adjust vertical position as needed
    hjust = 0  # Move the text to the right of each location
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "GPS Locations and UK Coastline")

#####

ggplot() +
  geom_sf(data = uk_coastline, color = "grey", size = 0.5) +
  geom_point(data = colony_data, aes(x = Longitude, y = Latitude, color = Individ), size = 3) +
  geom_text(
    data = colony_data,
    aes(x = Longitude + 0.3, y = Latitude, label = Location_Name),
    size = 3,  # Adjust the font size as needed
    vjust = 0.5,  # Adjust vertical position as needed
    hjust = 0  # Move the text to the right of each location
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "GPS Locations and UK Coastline") +
  theme_bw() +  # Maintain the plot border
  theme(panel.grid = element_blank())  # Remove grid lines


#### stopping the location names from overlapping

install.packages("ggrepel")
library(ggrepel)

ggplot() +
  geom_sf(data = uk_coastline, color = "grey", size = 0.1) +
  geom_point(data = colony_data, aes(x = Longitude, y = Latitude, color = Individ), size = 3) +
  geom_text_repel(
    data = colony_data,
    aes(x = Longitude + 0.3, y = Latitude, label = Location_Name),
    size = 3,
    nudge_x = 0.1,
    nudge_y = 0.1,
    direction = "both",
    segment.size = 0.2,
    segment.color = "grey50",
    min.segment.length = 0.1,
    force = 3
  ) +
  scale_color_gradient(low = "blue", high = "red", name = "No. of Individuals") +
  labs(title = NULL) +  # Remove the main title
  theme_bw() +
  theme(panel.grid = element_blank())


#### removing the coast line

ggplot() +
  geom_sf(data = uk_coastline, fill = "grey", color = "grey", size = 0) +
  geom_point(data = colony_data, aes(x = Longitude, y = Latitude, color = Individ), size = 3) +
  geom_text_repel(
    data = colony_data,
    aes(x = Longitude + 0.3, y = Latitude, label = Location_Name),
    size = 4,
    nudge_x = 0.1,
    nudge_y = 0.1,
    direction = "both",
    segment.size = 0.2,
    segment.color = "grey50",
    min.segment.length = 0.1,
    force = 3
  ) +
  scale_color_gradient(low = "blue", high = "red", name = "No. of Individuals") +
  labs(title = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank())


#### Happy with at the moment

ggplot() +
  geom_sf(data = uk_coastline, fill = "grey", color = "grey", size = 0) +
  geom_point(data = colony_data, aes(x = Longitude, y = Latitude, color = Individ), size = 3) +
  geom_text_repel(
    data = colony_data,
    aes(x = Longitude + 0.3, y = Latitude, label = Location_Name),
    size = 3,
    nudge_x = 0.1,
    nudge_y = 0.1,
    direction = "both",
    segment.size = 0.2,
    segment.color = "grey50",
    min.segment.length = 0.1,
    force = 3
 ) +
  scale_color_gradient(low = "blue", high = "red", name = "No. of Individuals") +
  labs(title = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank())

