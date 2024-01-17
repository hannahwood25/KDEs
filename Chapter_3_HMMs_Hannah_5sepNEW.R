#### Kittiwake HMMs

# process
# step 1: choose which tracks to analyse (species, island, length etc)
# step 2: make KDEs for all individuals
# step 3: export saved KDEs 
# step 4a: train HMMs on whole colony and then apply to individuals (justification - scale paramaters of KDEs based on long tracks from whole colony)
# step 4b: create KDEs for each behavior for each individual
# step 4c: export shape files of behavioral KDEs
# step 5: compare percentage overlap of individuals

### packages

library(track2KBA) # load package
library(tidyverse)
library(moveHMM)
library(readr)
library(sf)

### get data from the colonies of more than 8 individuals

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


## make a loop where we select all the data from colony long trips for colonies with more than 10 individuals
for (colony in colonies_more_than_8$Site) {
  
  colony = gsub(":", "_", colony)
  
  complete_trips_df = read.csv(paste0(colony, "_complete_trips.csv"))
  colony_hVals = readRDS(paste0(colony, "_hVals.rds"))
  
  # here we know that the first points in the data set are from the colony center
  # selct the lat and long for each colony site
  # kitt_colony has been calculated from the mean Lat and Long of parts of the tracks labelled colony
  colony_location <- kitt_colony %>% 
    dplyr::filter(Site == colony) %>%
    dplyr::select(Latitude, Longitude)
  
  # Calculate number of positions per individual per year and write out as table
  year_ind_table = table(complete_trips_df$Year, complete_trips_df$ID)
  write.csv(year_ind_table, paste0(colony, "_year_ind_table.csv"))
  
  
  # SHOULD WE DO THIS? *
  #colony_long_trips_df <- colony_long_trips_df[colony_long_trips_df$Dist2colony > 3, ] 
  
  ## pre-processing of the tracking data which moveHMM requires
  complete_trips_df_moveHMM <- prepData(dplyr::select(complete_trips_df, ID, Latitude, Longitude),type="LL",coordNames=c("Longitude","Latitude"))
  
  ## remove any points where the bird is staying still (i.e sitting in the colony)
  n_zero_steps = sum(complete_trips_df_moveHMM$step == 0, na.rm = T)
  
  ## making a spatial raster? Not sure
  saveRDS(complete_trips_df_moveHMM, paste0(colony, "_prep_data_", n_zero_steps, "_zero_steps.rds"))
  #colony_long_trips_moveHMM = readRDS(paste0(colony, "_prep_data_", n_zero_steps, "_zero_steps.rds"))
  
  # CHECK THIS!!!!************
  print(sprintf("****** Found and removed %d 0-step movements ******", n_zero_steps))
  complete_trips_df = subset(complete_trips_df, complete_trips_df_moveHMM$step > 0)
  complete_trips_df_moveHMM = subset(complete_trips_df_moveHMM, step > 0)
  ######
  
  ## call to fitting function
  
  m_3 <- fitHMM(data=complete_trips_df_moveHMM,
                nbStates=3,
                stepPar0=stepPar0,
                anglePar0=anglePar0,
                formula = ~1,
                verbose = 2)
  
  saveRDS(m_3, paste0(colony, "_trainedHMM_3state.rds"))
  #m_3 = readRDS(paste0(colony, "_trainedHMM_3state.rds"))
  
  ### get info from model
  #m_3
  plot(m_3, plotCI=TRUE, ask = FALSE, plotTracks = FALSE)
  
  params = data.frame(t(m_3$mle$stepPar))
  params$state = rownames(params)
  
  params_ang = data.frame(t(m_3$mle$anglePar))
  colnames(params_ang)[1] = "vm_mean"
  params_ang$state = rownames(params_ang)
  
  all_params = merge(params, params_ang, by = "state")
  
  write.csv(all_params, paste0(colony, "_trainedHMM_3state.csv"))
  #params_order = params %>% arrange(mean) %>% mutate(index = row_number())
  #params = merge(params, dplyr::select(params_order, mean, index), by = "mean")
  
  #params = pivot_longer(params, state)
  
  # Given a model (that we like/works) we want to make decoded tracks for each individual
  ## and then make KDEs of those behaviors
  
  ### we want to add the state as a column for each location/data point
  
  ### gives you list of assigned states in order but not attached to the daaframe
  states <- viterbi(m_3)
  
  ### make a little table of prop time spent in diff states
  props = prop.table(table(states))
  
  write.csv(props, paste0(colony, "hmmprops.csv"))
  
  # Add states to original 'long' track dataframe
  complete_trips_df$state = states
  
  # Change 'ID" back to original ID (one per bird, not per trip)
  #### does this mean we calculated HMMs for each trip not each individual?
  complete_trips_df$ID = complete_trips_df$origID
  
  write.csv(complete_trips_df, paste0(colony, "tripswithstates.csv"))
  
  # Plot states
  # ggplot(complete_trips_df, aes(x = Longitude, y = Latitude, color = factor(state))) + geom_point(alpha = 0.2)
  
  legend_labels <- c("Resting", "Foraging", "Commuting")
  
  ggplot(complete_trips_df, aes(x = Longitude, y = Latitude, color = factor(state))) +
    geom_point(alpha = 0.2) +
    scale_color_manual(
      name = "Behaviour",  # Set the legend title
      labels = legend_labels,  # Set the custom legend labels
      values = c("1" = "#009E73", "2" = "#E69F00", "3" = "#8494FF")  # Adjust color values as needed
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 1))) 
  
  ## nicer light blue "#56B4E9"
  
  ## save the plot
  ggsave(paste0(colony, "statesmap.pdf"), width = 12, height = 8)
  
  for (s in 1:3) {
    # Extract just the points for state 1
    complete_trips_df_state = subset(complete_trips_df, state == s)
    # Plot
    ggplot(complete_trips_df_state, aes(x = Longitude, y = Latitude)) + geom_point()
    
    ### reprojecting tracks into "azim" projection
    complete_trips_df_state_azim <- projectTracks( dataGroup = complete_trips_df_state, projType = 'azim', custom=TRUE )
    
  
    ### Create KDE for this behaviour
    colony_KDE_state <- estSpaceUse(
      tracks = complete_trips_df_state_azim, 
      scale = colony_hVals$mag, 
      res = 1,
      levelUD = 50, 
      polyOut = TRUE
    )
    
    #png(paste0(colony, "_Kittiwake_KDE_state", s, "_repr_assessment.png"))
    repr <- repAssess(
      tracks    = complete_trips_df_state_azim,
      KDE       = colony_KDE_state$KDE.Surface,
      levelUD   = 50,
      iteration = 1,
      bootTable = FALSE)
    # dev.off()
    # 
    repr$state = s
    repr$colony = colony
    write.csv(repr, paste0(colony, "_Kittiwake_KDE_state", s, "_repr_assessment.csv"))
    
    # Plot
    mapKDE(KDE = colony_KDE_state$UDPolygons, colony = colony_location)
    # Save
    saveRDS(colony_KDE_state, paste0(colony, "_Kittiwake_KDE_state", s, "_Aliceprior1km.rds"))
  }
  
  KDE_state1 = readRDS(paste0(colony, "_Kittiwake_KDE_state1_Aliceprior1km.rds"))
  KDE_state2 = readRDS(paste0(colony, "_Kittiwake_KDE_state2_Aliceprior1km.rds"))
  KDE_state3 = readRDS(paste0(colony, "_Kittiwake_KDE_state3_Aliceprior1km.rds"))
  
  e1 = extent(KDE_state1$UDPolygons)
  e2 = extent(KDE_state2$UDPolygons)
  e3 = extent(KDE_state3$UDPolygons)
  
  min_lon = min(e1[1], e2[1], e3[1])
  max_lon = max(e1[2], e2[2], e3[2])
  min_lat = min(e1[3], e2[3], e3[3])
  max_lat = max(e1[4], e2[4], e3[4])
  
  # Load library for multiple plots
  library(patchwork)
  
  # Plot KDEs for state 1 (fixing lat/long range to be consistent across plots below)
  p1 = ggplot(KDE_state1$UDPolygons) + 
    geom_sf(aes(col = .data$id), fill = NA) + 
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) + 
    borders("world", colour = "black", fill = NA) + 
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
    ylab("Latitude") + xlab("Longitude") + 
    ggtitle(paste0("Rest: ", format(100*props[1], digits = 4), "%"))
  
  # Plot KDEs for state 2 (fixing lat/long range to be consistent across plots below)
  p2 = ggplot(KDE_state2$UDPolygons) + 
    geom_sf(aes(col = .data$id), fill = NA) + 
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) + 
    borders("world", colour = "black", fill = NA)+ 
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
    ylab("Latitude") + xlab("Longitude")+ 
    ggtitle(paste0("Forage/Search: ", format(100*props[2], digits = 4), "%"))
  
  
  # Plot KDEs for state 3 (fixing lat/long range to be consistent across plots below)
  p3 = ggplot(KDE_state3$UDPolygons) + 
    geom_sf(aes(col = .data$id), fill = NA) + 
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) + 
    borders("world", colour = "black", fill = NA) + 
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
    ylab("Latitude") + xlab("Longitude")+ 
    ggtitle(paste0("Commuting/Flight: ", format(100*props[3], digits = 4), "%"))
                 
  # Combine into three column plot                                                           
  p1 + p2 + p3
  ggsave(paste0(colony, "_behavioural_KDEs_3State_Kittiwakes_Aliceprior1km.pdf"), width = 12, height = 6)
  
  
  # Load 'All' KDE object
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
  (p1 + p2 + p3) / (p4 + p4 + p4)
  ggsave(paste0(colony, "_behavioural_KDEs_3State_Kittiwakes_vsALL_Aliceprior1km.pdf"), width = 12, height = 8)
  
  ### want to make the all plo big and then the others on the side
  
  
  ### re-projecting tracks into "azim" projection
  complete_trips_df_azim <- projectTracks( dataGroup = complete_trips_df, projType = 'azim', custom=TRUE )
  
  png(paste0(colony, "_Kittiwake_KDE_state_ALL_repr_assessment.png"))
  repr <- repAssess(
    tracks    = complete_trips_df_azim, 
    KDE       = colony_KDE_all$KDE.Surface,
    levelUD   = 50,
    iteration = 1, 
    bootTable = FALSE)
  dev.off()
  
  repr$state = "ALL"
  repr$colony = colony
  write.csv(repr, paste0(colony, "_Kittiwake_KDE_state_ALL_repr_assessment.csv"))
  
  
  
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
  
  # Important! Avoids error in calculate_overlap
  sf::sf_use_s2(FALSE)
  
  # Use the function to calcuate overlap for each behaviour
  results_area_df = calculate_overlap(all_poly, KDE_state1$UDPolygons, "A")
  results_area_df2 = calculate_overlap(all_poly, KDE_state2$UDPolygons, "B")
  results_area_df3 = calculate_overlap(all_poly, KDE_state3$UDPolygons, "C")
  
  # We could also compare each behaviou to each other
  #results_area_df4 = calculate_overlap(KDE_state1$UDPolygons, KDE_state3$UDPolygons, "AC")
  #results_area_df5 = calculate_overlap(KDE_state2$UDPolygons, KDE_state3$UDPolygons, "BC")
  #results_area_df6 = calculate_overlap(KDE_state1$UDPolygons, KDE_state2$UDPolygons, "AB")
  
  
  
  # Combine into single dataframe
  all_results_df = rbind(results_area_df, results_area_df2, results_area_df3)
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
  
  ggsave(paste(colony, "_KDEviolins_1km.pdf"), width = 12, height = 8)

}
