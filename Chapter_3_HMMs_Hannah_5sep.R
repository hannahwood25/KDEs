#### Kittiwake HMMs

# process
# step 1: choose which tracks to analyse (species, island, length etc)
# step 2: make KDEs for all individuals
# step 3: export saved KDEs 
# step 4a: train HMMs on whole colony and then apply to individuals (justification - scale paramaters of KDEs based on long tracks from whole colony)
# step 4b: create KDEs for each behavior for each individual
# step 4c: export shape files of behavioral KDEs
# step 5: compare percentage overlap of individuals

setwd("~/Desktop/R 5 sep")

### packages

library(track2KBA) # load package
library(tidyverse)
library(moveHMM)
library(readr)
install.packages("sf")
library(sf)
# Load library for multiple plots
library(patchwork)

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
  
# old plot  
#  ggplot(complete_trips_df, aes(x = Longitude, y = Latitude, color = factor(state))) + 
#    geom_point(alpha = 0.2)
 
  # Define your custom legend labels
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
}

