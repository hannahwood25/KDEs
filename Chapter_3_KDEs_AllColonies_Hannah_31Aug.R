#### Kittiwake KDAs for all colonies

# process
# step 1: choose which tracks to analyse (species, island, length etc)
# step 2: make KDEs for all individuals
# step 3: export saved KDEs 
# step 4a: train HMMs on whole colony and then apply to individuals (justification - scale paramaters of KDEs based on long tracks from whole colony)
# step 4b: create KDEs for each behavior for each individual
# step 4c: export shape files of behavioral KDEs
# step 5: compare percentage overlap of individuals

###################################################################################

#### Packages 

install.packages("track2KBA")
library(track2KBA) # load package
library(tidyverse)
library(readr)

####################################################################################

### Kittiwake data  #####

#setwd("/Users/hannahwood/Desktop/Chapter_3/Chapter3_R")

## read in csv from data folder
FAME_CombinedBLKI_wTripIDs <- read_csv("FAME_CombinedBLKI_wTripIDs.csv")

###rename data to something shorter
kitt <- FAME_CombinedBLKI_wTripIDs


######### Stats for Supplementary Material Table 1

# Hpw many individuals vs. individuals per year
# kitt %>%
#   group_by(Site) %>%
#   summarise(n_IDyr = n_distinct(IDYr),
#             n_ID = n_distinct(ID))
## no individuals are tracked over multiple yearskitt_summary = kitt %>%
 
## how many individuals and trips and years are recorded at each colony
kitt_summary = kitt %>%
  group_by(Site) %>%
  summarise(n_ind = n_distinct(ID),
            n_trips = n_distinct(TripID),
            n_year = n_distinct(Year))



### locate the colonies (get lat and long)
kitt_colony = kitt %>%
  group_by(Site) %>%
  filter(TripID == "COLONY") %>%
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude),
            n_year = n_distinct(Year))

### locate the colonies (get lat and long)
kitt_colony = kitt %>%
  dplyr::group_by(Site) %>%
  dplyr::filter(TripID == "COLONY") %>%
  dplyr::summarise(Latitude = mean(Latitude),
                   Longitude = mean(Longitude))


###########################################################################

###### The data we want to analyse 

### To match with Alice, only use colnies with 8 or more tracked individuals
### etract these colnies from kitt_summary
colonies_more_than_8 = subset(kitt_summary, n_ind >= 8)
## save as csv
write.csv(colonies_more_than_8, "colony_morethan8_summ.csv")

#############################################################################

#### Plot a map of the colonies with their locations

# Plotting the Bempton colony
# ggplot(kittbem, aes(x = Longitude, y = Latitude)) + 
#   geom_point(alpha = 0.2) + 
#   geom_point(data = subset(kittbem, TripID == "COLONY"), color = "red") + 
#   geom_point(data = subset(kitt_colony, Site == "BEM"), aes(x=lon, y=lat), color = "green", size = 3)


###############################################################################

### separate "TrackTime" into fieldDate and fieldTime

colonies_more_than_8 = subset(colonies_more_than_8, Site != "FAI")

for (colony in colonies_more_than_8$Site) {
  colony = gsub(":", "_", colony)
  
  print(sprintf("Processing Colony %s", colony))
  colony_track_data = subset(kitt, Site == colony)
  
  # temp = colony_track_data %>% group_by(ID, TripID) %>%
  #   filter(TripID == "COLONY") %>%
  #   summarise(max_dist_to_colony = max(Dist2colony))
  # 
  # Remove any data labelled as COLONY in FAME
  #colony_data_ontrip = subset(colony_track_data, TripID != "COLONY")
  # Make our own tripID
  colony_track_data$tripID = paste0(colony_track_data$IDYr, "_", colony_track_data$TripID)
  
  colony_data_ontrip_summ = colony_track_data %>% 
    group_by(tripID) %>%
    filter(TripID != "COLONY") %>%
    summarise(
      duration_m = (max(Abstime) - min(Abstime))/60,
      first_loc_past_500 = min(which(Dist2colony > 0.5))
    )
  
  write.csv(colony_data_ontrip_summ, paste0(colony, "_colony_data_dur_and_first_loc500.csv"))
  
  # Find any trips with duration < 14 mins
  #short_trips = filter(colony_data_ontrip_summ, duration_m < 14)
  
  #colony_data_ontrip = filter(colony_track_data, !(tripID %in% short_trips$tripID))
  
  
  # Set up the dataframe for using the track2kba functions, specifying each column
  colony_dataGroup <- formatFields(
    dataGroup = colony_track_data, 
    fieldID   = "id", 
    fieldDateTime = "TrackTime", ### kitt date and time need to be separated
    fieldLon  = "Longitude", 
    fieldLat  = "Latitude"
  )
  # Have a look a tthe structur
  str(colony_dataGroup)
  
  #colony_dataGroup$ID = paste0(colony_dataGroup$ID, "_", colony_dataGroup$TripID)
  
  # here we know that the first points in the data set are from the colony center
  colony_location <- kitt_colony %>% 
    filter(Site == colony) %>%
    dplyr::select(Latitude, Longitude)
  
  # Split the tracks up into trips using some defined thresholds
  # ** Read docs! **
  # ?tripSplit
  colony_trips <- tripSplit(
    dataGroup  = colony_dataGroup,
    colony     = colony_location,
    innerBuff  = 0.5,      # kilometers
    returnBuff = 1,
    duration   = 0.25,      # hours
    rmNonTrip  = FALSE
  )
  
  
  ### total no. of trips
  n_distinct(colony_trips$tripID)

# Plot these trips
  mapTrips(trips = colony_trips, colony = colony_location)
  
  # Remove trips that did not return to with in 5km of the colony
  complete_colony_trips <- subset(colony_trips, colony_trips$Returns == "Yes" )
  
  ### total no. of trips
  n_distinct(complete_colony_trips$tripID)
  
  mapTrips(trips = complete_colony_trips, colony = colony_location)
  
  colony_sumTrips <- tripSummary(trips = complete_colony_trips, colony = colony_location)
  #View(colony_sumTrips)
  
 # print(long_colony_trips_summ)
  
  write.csv(colony_sumTrips, paste0(colony, "_colony_trips_summ.csv"))
  
  # Subset to only retain the long trips IDs we found
  ### REMOVED TO MATCH PROCESS FROM TREVAIL ET AL
  # long_colony_trips = subset(complete_colony_trips, tripID %in% long_colony_trips_summ$tripID)
  #long_colony_trips = complete_colony_trips
  
  saveRDS(complete_colony_trips, paste0(colony, "_complete_trips.rds"))
  
  complete_colony_trips_df = as.data.frame(complete_colony_trips)
  write.csv(complete_colony_trips_df, paste0(colony, "_complete_trips.csv"))
  
  # Plot these trips
  #mapTrips(trips = long_colony_trips, colony = colony_location)
  
  ### replotting tracks into "azim" projection
  complete_colony_tracks <- projectTracks( dataGroup = complete_colony_trips, projType = 'azim', custom=TRUE )
  
  ## finding a scale in km for kernel densities
  colony_hVals <- findScale(
    tracks   = complete_colony_tracks,
    scaleARS = FALSE,
    res = 1,
    sumTrips = colony_sumTrips)
  
  ### look up what this means
  saveRDS(colony_hVals, paste0(colony, "_hVals.rds"))
  colony_hVals
  
  
  ### try running below but substituting mag for scaleARS
  colony_KDE <- estSpaceUse(
    tracks = complete_colony_tracks, 
    scale = colony_hVals$mag, 
    levelUD = 50, 
    res = 1,
    polyOut = TRUE
  )
  
  mapKDE(KDE = colony_KDE$UDPolygons, colony = colony_location)
  
  saveRDS(colony_KDE, paste0(colony, "_Kittiwake_KDE_innerBuff0p5_returnBuff2_dur1_1km.rds"))
}

###############################################################################

# Create an interactive map of locations and colony + radius
library(sf)
library(rgdal)
library(mapview)

temp = colony_track_data %>%
  group_by(id) %>%
  sample_n(100) %>%
  ungroup() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

colony_location_df = colony_location %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

colony_buffer <- st_buffer(colony_location_df, dist = 3000)

mapview(colony_location_df, col.regions="red") + 
  mapview(temp, zcol = "TripID") + 
  mapview(colony_buffer, col.regions = NA, color = "red", lwd = 2)

#############################################################################

####### Stats from trip which are complete and used to do the KDEs

### read in colony trip summaries

bar <- read_csv("BAR_colony_trips_summ.csv")
bem <- read_csv("BEM_colony_trips_summ.csv")
coq <- read_csv("COQ_colony_trips_summ.csv")
csy <- read_csv("CSY_colony_trips_summ.csv")
fai <- read_csv("FAI_colony_trips_summ.csv")
fil <- read_csv("FIL_colony_trips_summ.csv")
fow <- read_csv("FOW_colony_trips_summ.csv")
iom <- read_csv("IOM_colony_trips_summ.csv")
lam <- read_csv("LAM_colony_trips_summ.csv")
cop <- read_csv("ORK_COP_colony_trips_summ.csv")
mks <- read_csv("ORK_MKS_colony_trips_summ.csv")
puf <- read_csv("PUF_colony_trips_summ.csv")
rat <- read_csv("RAT_colony_trips_summ.csv")
sab <- read_csv("SAB_colony_trips_summ.csv")
sci <- read_csv("SCI_STM_colony_trips_summ.csv")
win <- read_csv("WIN_colony_trips_summ.csv")


### bind into one file
all_trips_data <- rbind(bar, bem, coq,
                           csy,
                           fai,
                           fil,
                           fow,
                           iom ,
                           lam ,
                           cop ,
                           mks ,
                           puf ,
                           rat ,
                           sab ,
                           sci ,
                           win)
 
## save megafile

write.csv(all_trips_data, "all_trips_data_summ.csv")

all_trips_data = read.csv("all_trips_data_summ.csv")

### overall trip statistics

### total trip distance
mean(all_trips_data$total_dist)
## 299.10
sd(all_trips_data$total_dist)
## 228.84

### max distance from colony
mean(all_trips_data$max_dist)
### 59.32
sd(all_trips_data$max_dist)
## 46.74

### mean trip duration
mean(all_trips_data$duration) ##what is duration in?
## 31.87
sd(all_trips_data$duration)
## 19.64

### mean no of locations in a trip
mean(all_trips_data$n_locs)
## 728.35
sd(all_trips_data$n_locs)
## 520.57


### total no. of trips
n_distinct(all_trips_data$tripID)
## 544

### total number of individuals
n_distinct(all_trips_data$ID)
## 544

### looks like there is only one trip for each individual which doesn't seem right?

####### average number of trips per individual?
## am I doing this wrong?

trips_per_individual = all_trips_data %>%
  group_by(ID) %>%
  summarise(n_trips = n_distinct(tripID))

mean(trips_per_individual$n_trips)
sd(trips_per_individual$n_trips)


########## Stats for each colony.
##### Trip metrics per Site. #######

## Add a column for the colony
# Extract 6th, 7th, and 8th letters from IDtripID and create a new column "ExtractedLetters"
all_trips_data$Site <- substr(all_trips_data$ID, 6, 8)

## trip length (per colony?)

## Number of individuals and trips: makes a summary of colonies
all_trips_site_summary = all_trips_data %>%
  group_by(Site) %>%
  summarise(n_ind = n_distinct(ID),
            n_trips = n_distinct(tripID),
            mean_tdist = mean(total_dist),
            sd_tdist = sd(total_dist),
            mean_maxcoldist = mean(max_dist),
            sd_maxcoldist = sd(max_dist),
            mean_duration = mean(duration), ##what is duration in?
            sd_duration = sd(duration))


## save summary file
write.csv(all_trips_site_summary, "all_trips_site_summ.csv")

