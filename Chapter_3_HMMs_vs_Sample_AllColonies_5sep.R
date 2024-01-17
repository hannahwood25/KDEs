#### Kittiwake KDAs

# process
# step 1: choose which tracks to analyse (species, island, length etc)
# step 2: make KDEs for all individuals
# step 3: export saved KDEs 
# step 4a: train HMMs on whole colony and then apply to individuals (justification - scale paramaters of KDEs based on long tracks from whole colony)
# step 4b: create KDEs for each behavior for each individual
# step 4c: export shape files of behavioral KDEs
# step 5: compare percentage overlap of individuals


## read through paper, KDE help files, and Alice's code
## send Kittiwake_PUF_KDE_innerBuff5_returnBuff20_dur1 to Rob

# ~/Desktop/R 5 sep/Chapter_3_HMMs_vs_Sample_AllColonies_5sep.R

setwd("/Users/hannahwood/Desktop/R 5 sep")

library(track2KBA) # load package
library(tidyverse)
library(moveHMM)
# Load library for multiple plots
library(patchwork)
library(terra)
library(readr)

FAME_CombinedBLKI_wTripIDs <- read_csv("/Users/hannahwood/Desktop/OMGPhD/PhD data/FAME_CombinedBLKI_wTripIDs.csv")
View(FAME_CombinedBLKI_wTripIDs)

kitt <- FAME_CombinedBLKI_wTripIDs


kitt_colony = kitt %>%
  group_by(Site) %>%
  filter(TripID == "COLONY") %>%
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude))
#write.csv(colonies_more_than_10, "colony_data_summ.csv")

colonies_more_than_8 = read.csv("colony_morethan8_summ.csv")

### locate the colonies (get lat and long)
kitt_colony = kitt %>%
  dplyr::group_by(Site) %>%
  dplyr::filter(TripID == "COLONY") %>%
  dplyr::summarise(Latitude = mean(Latitude),
                   Longitude = mean(Longitude))

### Values from Alice's Oikos 2018 paper

stepMean0 <- c(0.08, 0.27, 1) # step length means of 3 states
stepSD0 <- c(0.05, 0.31, 0.35) # step length variance of 3 states
stepPar0 <- c(stepMean0,stepSD0)

angleMean0 <- c(0.00, 0.00, 0.00) # turning angle means of 3 states
kappa0 <- c(14, 0.4, 6.8) # angle concentration
anglePar0 <- c(angleMean0 ,kappa0)

### parameters roughly describe sitting, foraging, flying.


colonies_more_than_8 = subset(colonies_more_than_8, Site != "FAI")
colonies_more_than_8 = subset(colonies_more_than_8, Site != "RAT")


for (colony in colonies_more_than_8$Site) {
  colony = gsub(":", "_", colony)
  colony_long_trips_df = read.csv(paste0(colony, "_complete_trips.csv"))
  colony_hVals = readRDS(paste0(colony, "_hVals.rds"))
  
  # here we know that the first points in the data set are from the colony center
  colony_location <- kitt_colony %>% 
    filter(Site == colony) %>%
    dplyr::select(Latitude, Longitude)
  
  # SHOULD WE DO THIS? *
  #colony_long_trips_df <- colony_long_trips_df[colony_long_trips_df$Dist2colony > 3, ] 
  
  colony_long_trips_moveHMM <- prepData(dplyr::select(colony_long_trips_df, ID, Latitude, Longitude),type="LL",coordNames=c("Longitude","Latitude"))
  
  n_zero_steps = sum(colony_long_trips_moveHMM$step == 0, na.rm = T)
  
  #saveRDS(colony_long_trips_moveHMM, paste0(colony, "_prep_data_", n_zero_steps, "_zero_steps.rds"))
  
  # CHECK THIS!!!!************
  print(sprintf("****** Found and removed %d 0-step movements ******", n_zero_steps))
  colony_long_trips_df = subset(colony_long_trips_df, colony_long_trips_moveHMM$step > 0)
  colony_long_trips_moveHMM = subset(colony_long_trips_moveHMM, step > 0)
  ######
  
  ## call to fitting function
  # m_3 <- fitHMM(data=colony_long_trips_moveHMM,
  #               nbStates=3,
  #               stepPar0=stepPar0,
  #               anglePar0=anglePar0,
  #               formula = ~1,
  #               verbose = 2)
  # 
  # saveRDS(m_3, paste0(colony, "_trainedHMM_3state.rds"))
  m_3 = readRDS(paste0(colony, "_trainedHMM_3state.rds"))
  
  ### get info from model
  #m_3
  #plot(m_3, plotCI=TRUE, ask = FALSE, plotTracks = FALSE)
  
  # Given a model (that we like/works) we want to make decoded tracks for each individual
  ## and then make KDEs of those behaviors
  
  ### we want to add the state as a column for each location/data point
  
  ### gives you list of assigned states in order but not attached to the daaframe
  states <- viterbi(m_3)
  
  ### make a little table of prop time spent in diff states
  props = prop.table(table(states))
  
  # Add states to original 'long' track dataframe
  colony_long_trips_df$state = states
  # Change 'ID" back to original ID (one per bird, not per trip)
  colony_long_trips_df$ID = colony_long_trips_df$origID
  
  # Plot states
  #ggplot(colony_long_trips_df, aes(x = Longitude, y = Latitude, color = factor(state))) + geom_point(alpha = 0.2)
  
  
  # for (s in 1:3) {
  #   # Extract just the points for state 1
  #   colony_long_trips_df_state = subset(colony_long_trips_df, state == s)
  #   # Plot
  #   ggplot(colony_long_trips_df_state, aes(x = Longitude, y = Latitude)) + geom_point()
  #   
  #   ### reprojecting tracks into "azim" projection
  #   colony_long_trips_df_state_azim <- projectTracks( dataGroup = colony_long_trips_df_state, projType = 'azim', custom=TRUE )
  #   
  # 
  #   ### Create KDE for this behaviour
  #   colony_KDE_state <- estSpaceUse(
  #     tracks = colony_long_trips_df_state_azim, 
  #     scale = colony_hVals$mag, 
  #     res = 1,
  #     levelUD = 50, 
  #     polyOut = TRUE
  #   )
  #   
  #   # Plot
  #   mapKDE(KDE = colony_KDE_state$UDPolygons, colony = colony_location)
  #   # Save
  #   saveRDS(colony_KDE_state, paste0(colony, "_Kittiwake_KDE_state", s, "_Aliceprior1km.rds"))
  # }
  
  # Calculate 'ALL' behavour KDE from sample of all behaviuor of size given by smallest behaviour
  min_n = sum(colony_long_trips_df$state == as.numeric(which.min(props)))
  
  colony_long_trips_df_sample = sample_n(colony_long_trips_df, size = min_n)
  colony_long_trips_df_sample_azim <- projectTracks( dataGroup = colony_long_trips_df_sample, projType = 'azim', custom=TRUE )
  colony_KDE_sample <- estSpaceUse(
    tracks = colony_long_trips_df_sample_azim, 
    scale = colony_hVals$mag, 
    res = 1,
    levelUD = 50, 
    polyOut = TRUE
  )
  mapKDE(KDE = colony_KDE_sample$UDPolygons, colony = colony_location)
  
  
  
  saveRDS(colony_KDE_sample, paste0(colony, "_Kittiwake_KDE_SAMPLE_Aliceprior1km.rds"))
  

  KDE_state1 = readRDS(paste0(colony, "_Kittiwake_KDE_state1_Aliceprior1km.rds"))
  KDE_state2 = readRDS(paste0(colony, "_Kittiwake_KDE_state2_Aliceprior1km.rds"))
  KDE_state3 = readRDS(paste0(colony, "_Kittiwake_KDE_state3_Aliceprior1km.rds"))
  KDE_state_SAMPLE = readRDS(paste0(colony, "_Kittiwake_KDE_SAMPLE_Aliceprior1km.rds"))
  
  e1 = raster::extent(KDE_state1$UDPolygons)
  e2 = raster::extent(KDE_state2$UDPolygons)
  e3 = raster::extent(KDE_state3$UDPolygons)
  e4 = raster::extent(KDE_state_SAMPLE$UDPolygons)
  
  min_lon = min(e1[1], e2[1], e3[1], e4[1])
  max_lon = max(e1[2], e2[2], e3[2], e4[2])
  min_lat = min(e1[3], e2[3], e3[3], e4[3])
  max_lat = max(e1[4], e2[4], e3[4], e4[4])
  
  # Plot KDEs for state 1 (fixing lat/long range to be consistent across plots below)
  p1 = ggplot(KDE_state1$UDPolygons) + 
    geom_sf(aes(col = .data$id), fill = NA) + 
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) + 
    borders("world", colour = "black", fill = NA) + 
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
    ylab("Latitude") + xlab("Longitude") + 
    ggtitle(paste0("A: ", format(100*props[1], digits = 4), "%"))
  
  # Plot KDEs for state 2 (fixing lat/long range to be consistent across plots below)
  p2 = ggplot(KDE_state2$UDPolygons) + 
    geom_sf(aes(col = .data$id), fill = NA) + 
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) + 
    borders("world", colour = "black", fill = NA)+ 
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
    ylab("Latitude") + xlab("Longitude")+ 
    ggtitle(paste0("B: ", format(100*props[2], digits = 4), "%"))
  
  
  # Plot KDEs for state 3 (fixing lat/long range to be consistent across plots below)
  p3 = ggplot(KDE_state3$UDPolygons) + 
    geom_sf(aes(col = .data$id), fill = NA) + 
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) + 
    borders("world", colour = "black", fill = NA) + 
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
    ylab("Latitude") + xlab("Longitude")+ 
    ggtitle(paste0("C: ", format(100*props[3], digits = 4), "%"))
              
  
  # Plot KDEs for state 3 (fixing lat/long range to be consistent across plots below)
  p3_sample = ggplot(KDE_state_SAMPLE$UDPolygons) + 
    geom_sf(aes(col = .data$id), fill = NA) + 
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) + 
    borders("world", colour = "black", fill = NA) + 
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
    ylab("Latitude") + xlab("Longitude")+ 
    ggtitle(paste0("Sample: ", format(100*min(props), digits = 4), "%"))
  
  # Combine into three column plot                                                           
  p1 + p2 + p3 + p3_sample
  ggsave(paste0(colony, "_behavioural_KDEs_3State_Kittiwakes_Aliceprior1km.pdf"), width = 12, height = 6)
  
  
  # Load sample 'All' KDE object 
  colony_KDE_all = readRDS(paste0(colony, "_Kittiwake_KDE_innerBuff0p5_returnBuff2_dur1_1km.rds"))
  
  # Make fourth plot of all KDE
  p4 = ggplot(colony_KDE_all$UDPolygons) + 
    geom_sf(aes(col = .data$id), fill = NA) + 
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) + 
    borders("world", colour = "black", fill = NA) + 
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
    ylab("Latitude") + xlab("Longitude")+ 
    ggtitle("All")
  
  # Make a plot with three columns for each behaviour with 'all' plot beneath each
  p4 + (p1 + p2 + p3 + p3_sample + plot_layout(nrow = 2, ncol = 2)) + plot_layout(ncol = 2)
  ggsave(paste0(colony, "_behavioural_KDEs_3State_Kittiwakes_vsALL_Aliceprior1km.pdf"), width = 12, height = 8, scale = 1.2)
  
  
  # Make a plot with three columns for each behaviour with 'all' plot beneath each
  p4 + p1 + p2 + p3 + p3_sample
  ggsave(paste0(colony, "_all_KDEs.pdf"), width = 12, height = 8, scale = 1.2)
  
  
  pot_site_1 <- findSite(
    KDE_state1$KDE.Surface, represent = 90, levelUD = 50, polyOut = TRUE)
  pot_site_2 <- findSite(
    KDE_state2$KDE.Surface, represent = 90, levelUD = 50, polyOut = TRUE)
  pot_site_3 <- findSite(
    KDE_state3$KDE.Surface, represent = 90, levelUD = 50, polyOut = TRUE)
  pot_site_SAMPLE <- findSite(
    KDE_state_SAMPLE$KDE.Surface, represent = 90, levelUD = 50, polyOut = TRUE)
  pot_site_ALL = findSite(
    colony_KDE_all$KDE.Surface, represent = 90, levelUD = 50, polyOut = TRUE)
  
  saveRDS(pot_site_1, paste0(colony, "_Kittiwake_KDE_potSite_1_Aliceprior1km.rds"))
  saveRDS(pot_site_2, paste0(colony, "_Kittiwake_KDE_potSite_2_Aliceprior1km.rds"))
  saveRDS(pot_site_3, paste0(colony, "_Kittiwake_KDE_potSite_3_Aliceprior1km.rds"))
  saveRDS(pot_site_SAMPLE, paste0(colony, "_Kittiwake_KDE_potSite_SAMPLE_Aliceprior1km.rds"))
  saveRDS(pot_site_ALL, paste0(colony, "_Kittiwake_KDE_potSite_ALL_Aliceprior1km.rds"))
  
  # Important! Avoids error in mapSite
  sf::sf_use_s2(FALSE)
  
  #map1 = mapSite(pot_site_1)
  #map2 = mapSite(pot_site_2)
  #map3 = mapSite(pot_site_3)
  #map_SAMPLE = mapSite(pot_site_SAMPLE)
  #map_ALL = mapSite(pot_site_ALL)
  
  # Subsample to just those areas inside the potential site
  # Convert to 'vect' so we can quickly calculate area of each site using 'terra's 'expanse' function
  # Sum the areas to give total area of site
  ## MUST GET TERRA open for this to work
  
  area1 = sum(expanse(vect(subset(pot_site_1, potentialSite == TRUE)), unit = "km"))
  area2 = sum(expanse(vect(subset(pot_site_2, potentialSite == TRUE)), unit = "km"))
  area3 = sum(expanse(vect(subset(pot_site_3, potentialSite == TRUE)), unit = "km"))
  area_SAMPLE = sum(expanse(vect(subset(pot_site_SAMPLE, potentialSite == TRUE)), unit = "km"))
  area_ALL = sum(expanse(vect(subset(pot_site_ALL, potentialSite == TRUE)), unit = "km"))

  
  areas = data.frame(colony = colony, 
                     represent = 90, 
                     levelUD = 50,
                     area1 = area1, 
                     area2 = area2, 
                     area3 = area3, 
                     area_SAMPLE = area_SAMPLE, 
                     area_ALL = area_ALL)
  
  write.csv(areas, paste0(colony, "_Kittiwake_KDE_AREAS_Aliceprior1km.csv"))
  # For a given behaviour, 
  # - extract an individuals KDE polygons
  # - Calculate overlapping area
  # - Determine percentage of 'original'
  
  
  # Function that takes two multi-polygons (all tracks KDE and behavioural KDE)
  # and for each individual, calculate the percentage overlap between those
  # polygons
  calculate_overlap = function(all_poly, behaviour_poly, this_behaviour = "unknown") {
    
    # Get list of individuals
    # *** Should this be from 'all'? or 'behaviour'? ****
    ids = unique(all_poly$id)
    
    # place to store resulst for each individual
    results_list = list()
    # For each individual
    for (id in ids) {
      # Get the bits of 'all' KDE and behavioural KDE for this individual
      this_all_poly = all_poly[all_poly$id == id, ]
      this_behaviour_poly = behaviour_poly[behaviour_poly$id == id, ]
      
      # Make sure there's data, if not, return nothing
      if ((nrow(this_all_poly) > 0) & (nrow(this_behaviour_poly) > 0)) {
        # If we have data, calc intersection
        intersection = sf::st_intersection(this_all_poly, this_behaviour_poly)
        
        # Work out area of all, behaviour and intersection
        behaviour_area = sf::st_area(this_behaviour_poly)
        all_area = sf::st_area(this_all_poly)
        
        # Fudge as sometimes there's no intersection, so we shoudl return 0
        if (nrow(intersection) == 0) {
          area_intersect = 0
          units(area_intersect) = "m^2"
        } else {
          area_intersect = sf::st_area(intersection)
        }
        
        # Put results in dataframe
        results = data.frame(
          behaviour_area = behaviour_area,
          all_area = all_area,
          area_intersect = area_intersect,
          pct_beh = area_intersect / behaviour_area,
          pct_all = area_intersect / all_area,
          behaviour = this_behaviour
        )
        # Add to list
        results_list[[id]] = results
      }
      
    }
    # Turn list of results into big dataframe and return
    results_area_df = data.table::rbindlist(results_list, )
    return(results_area_df)
  }
  
  # Get all poly
  all_poly = colony_KDE_all$UDPolygons
  
  # Use the function to calcuate overlap for each behaviour
  results_area_df = calculate_overlap(all_poly, KDE_state1$UDPolygons, "A")
  results_area_df2 = calculate_overlap(all_poly, KDE_state2$UDPolygons, "B")
  results_area_df3 = calculate_overlap(all_poly, KDE_state3$UDPolygons, "C")
  results_area_sample = calculate_overlap(all_poly, KDE_state_SAMPLE$UDPolygons, "Sample")
  
  # We could also compare each behaviou to each other
  #results_area_df4 = calculate_overlap(KDE_state1$UDPolygons, KDE_state3$UDPolygons, "AC")
  #results_area_df5 = calculate_overlap(KDE_state2$UDPolygons, KDE_state3$UDPolygons, "BC")
  #results_area_df6 = calculate_overlap(KDE_state1$UDPolygons, KDE_state2$UDPolygons, "AB")
  
  
  
  # Combine into single dataframe
  all_results_df = rbind(results_area_df, results_area_df2, results_area_df3, results_area_sample)
  # If we did compare behaviours, need to add them here
  #all_results_df = rbind(results_area_df, results_area_df2, results_area_df3, results_area_df4, results_area_df5, results_area_df6)
  
  # Make 'longer' so pct_beh and pct_all are sep rows
  all_results_df_long = pivot_longer(all_results_df, c(pct_beh, pct_all))
  
  write.csv(all_results_df_long, paste0(colony, "_UD_comparison_results.csv"))
  
  library(units)
  
  # Change facet labels
  name.labs <- c("Percentage of full track KDE that is behaviour", "Percentage behaviour inside full track KDE")
  names(name.labs) <- c("pct_all", "pct_beh")
  
  # Plot
  KDEviolins <- ggplot(all_results_df_long, aes(x = behaviour, y = value, group = behaviour, fill = behaviour)) + 
    geom_violin(trim=TRUE, alpha = 0.5)+
    labs(title="",x="Behaviour", y = "Percentage")+
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.5, alpha = 0.4,
                 position=position_dodge(1)) + 
    geom_boxplot(width=0.1) +
    scale_fill_brewer(palette="Dark2") + theme_bw() + 
    facet_wrap(~name, labeller = labeller(name = name.labs))
  
  ggsave(plot = KDEviolins, paste(colony, "_KDEviolins_1km.pdf"), width = 12, height = 8)

}

# Exploring stats for all colonies...

all_results_combined = list()
for (colony in colonies_more_than_8$Site) {
  # Load each colonies behaviour
  colony = gsub(":", "_", colony)
  all_results_df_long = read.csv(paste0(colony, "_UD_comparison_results.csv"))
  all_results_df_long$behaviour = fct_relevel(all_results_df_long$behaviour, "Sample", "A", "B", "C")
  all_results_combined[[colony]] = all_results_df_long
}
all_results_combined_df = data.table::rbindlist(all_results_combined, idcol = "Colony")

write.csv(all_results_combined_df, file = "all_area_overlap_results_combined_df.csv")

library(lme4)
library(sjPlot)
library(forcats)

all_results_combined_df = read.csv("all_area_overlap_results_combined_df.csv")
all_results_combined_df$behaviour = fct_relevel(all_results_combined_df$behaviour, "Sample", "A", "B", "C")

beh_vs_all = subset(all_results_combined_df, name == "pct_beh")
beh_vs_all$ind_id = factor(beh_vs_all$all_area)
m = lmer(value ~  behaviour + (1|Colony), data = beh_vs_all)
summary(m)
plot_model(m, show.intercept = T)
plot_model(m, type = "re")
plot_model(m, type = "pred", axis.title	
           = c("Behaviour", "Proportion overlap"))
tab_model(m)

beh_prop_of_all = subset(all_results_combined_df, name == "pct_all")
m2 = lmer(value ~  behaviour + (1|Colony), data = beh_prop_of_all)
summary(m2)
plot_model(m2, show.intercept = T)
plot_model(m2, type = "re")
tab_model(m2)

library(glmmTMB)

# Transformation from Smithson & Verkuilen (2006): https://stats.stackexchange.com/questions/31300/dealing-with-0-1-values-in-a-beta-regression
beh_vs_all$value_prime = (beh_vs_all$value*(nrow(beh_vs_all) - 1) + 0.5)/nrow(beh_vs_all)

#delta = 1/(2*nrow(beh_vs_all))
#beh_vs_all$value[beh_vs_all$value == 0] = delta
#beh_vs_all$value[beh_vs_all$value > 0.99999999] = 1 - delta
model <- glmmTMB(value_prime ~ behaviour + (1|behaviour/Colony), 
                 family = beta_family(link = "logit"), data = beh_vs_all)

plot_model(model, show.intercept = T)
plot_model(model, type = "pred", axis.title	
 = c("Behaviour", "Proportion overlap"))
plot_model(model, type = "re")
tab_model(model, show.r2 = FALSE) 

##############################
###trying to change names in model

write.csv(beh_vs_al, file = "beh_vs_all.csv")

behave2 <- beh_vs_all

behave2$Behaviour <- behave2$behaviour

# change behaviour names
behave2$behaviour <- ifelse(behave2$behaviour == "A", "Resting",
                               ifelse(behave2$behaviour == "B", "Foraging",
                                      ifelse(behave2$behaviour == "C", "Commuting", behave2$behaviour)))


# Transformation from Smithson & Verkuilen (2006): https://stats.stackexchange.com/questions/31300/dealing-with-0-1-values-in-a-beta-regression
behave2$value_prime = (behave2$value*(nrow(behave2) - 1) + 0.5)/nrow(behave2)

## model
modelb2 <- glmmTMB(value_prime ~ Behaviour + (1|Behaviour/Colony), 
                 family = beta_family(link = "logit"), data = behave2)

plot_model(modelb2, show.intercept = T)
plot_model(modelb2, type = "pred", axis.title	
           = c("Behaviour", "Proportion overlap"))
plot_model(modelb2, type = "re")
tab_model(modelb2, show.r2 = FALSE) 

################################

behaviour_overlap_summaries = beh_vs_all %>% 
  group_by(Colony, behaviour) %>% 
  summarise(average = mean(value),
            sd = sd(value))

# What's the range of values for behaviour 'c'?
behaviour_overlap_summaries %>% 
  group_by(behaviour) %>%
  summarise(min = min(average),
            mean = mean(average),
            max = max(average),
            sd = sd(average))
