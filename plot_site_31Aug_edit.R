library(dplyr)
library(patchwork)
library(track2KBA)
library(ggplot2)
library(terra)

kitt_summary = kitt %>%
  group_by(Site) %>%
  summarise(n_ind = n_distinct(ID),
            n_trips = n_distinct(TripID))


kitt_colony = kitt %>%
  group_by(Site) %>%
  filter(TripID == "COLONY") %>%
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude))


# here we know that the first points in the data set are from the colony center
colony_location <- kitt_colony %>% 
  filter(Site == colony) %>%
  dplyr::select(Latitude, Longitude)

kitt_summary

colonies_more_than_8 = read.csv("colony_morethan8_summ.csv")

colonies_more_than_8 = subset(colonies_more_than_8, Site != "FAI")

all_results = list()

for (colony in colonies_more_than_8$Site) {
  
  colony = gsub(":", "_", colony)
  
  long_colony_trips = readRDS(paste0(colony, "_complete_trips.rds"))
  
  long_colony_trips_df = read.csv(paste0(colony, "_complete_trips.csv"))
  
  # Plot these trips
  #mapTrips(trips = long_colony_trips, colony = colony_location)
  
  ### look up what this means
  colony_hVals = readRDS(paste0(colony, "_hVals.rds"))
  
  colony_KDE = readRDS(paste0(colony, "_Kittiwake_KDE_innerBuff0p5_returnBuff2_dur1_1km.rds"))
  
  ## identify potential sites
  pot_site_poly <- findSite(colony_KDE$KDE.Surface, 
                       represent = 90, 
                       levelUD = 50,
                       polyOut = TRUE)
  
  # Important! Avoids error in mapSite
  sf::sf_use_s2(FALSE)
  
  map = mapSite(pot_site_poly, colony = colony_location)
  
  ggsave(filename = paste0(colony, "_rasterALL.pdf"), plot = map, width = 12, height = 12)
  
  KDE_state1 = readRDS(paste0(colony, "_Kittiwake_KDE_state1_Aliceprior1km.rds"))
  KDE_state2 = readRDS(paste0(colony, "_Kittiwake_KDE_state2_Aliceprior1km.rds"))
  KDE_state3 = readRDS(paste0(colony, "_Kittiwake_KDE_state3_Aliceprior1km.rds"))
  KDE_SAMPLE = readRDS(paste0(colony, "_Kittiwake_KDE_SAMPLE_Aliceprior1km.rds"))
  
  ## identify potential sites
  pot_site_poly1 <- findSite(KDE_state1$KDE.Surface, represent = 90, levelUD = 50, polyOut = TRUE)
  pot_site_poly2 <- findSite(KDE_state2$KDE.Surface, represent = 90, levelUD = 50, polyOut = TRUE)
  pot_site_poly3 <- findSite(KDE_state3$KDE.Surface, represent = 90, levelUD = 50, polyOut = TRUE)
  pot_site_sample <- findSite(KDE_SAMPLE$KDE.Surface, represent = 90, levelUD = 50, polyOut = TRUE)
  
  map1 = mapSite(pot_site_poly1, colony = colony_location)
  map2 = mapSite(pot_site_poly2, colony = colony_location)
  map3 = mapSite(pot_site_poly3, colony = colony_location)
  map4 = mapSite(pot_site_sample, colony = colony_location)
  
  all_map = (map  + ((map1 + map2) / (map3 + map4))) + 
    plot_layout(ncol = 2, guides = "collect") & theme(legend.position='bottom') 
    #scale_fill_continuous(limits = range(c(pot_site_poly$N_animals, pot_site_poly1$N_animals, pot_site_poly2$N_animals, pot_site_poly3$N_animals))) &
    #scale_color_continuous(limits = range(c(pot_site_poly$N_animals, pot_site_poly1$N_animals, pot_site_poly2$N_animals, pot_site_poly3$N_animals)))
  
  ggsave(filename = paste0(colony, "_raster_combined.pdf"), plot = all_map, width = 12, height = 8)
  
  # Calculate areas for each potential site
  pot_site_all = subset(pot_site_poly['potentialSite'], potentialSite == TRUE)
  pot_site1 = subset(pot_site_poly1['potentialSite'], potentialSite == TRUE)
  pot_site2 = subset(pot_site_poly2['potentialSite'], potentialSite == TRUE)
  pot_site3 = subset(pot_site_poly3['potentialSite'], potentialSite == TRUE)
  pot_siteS = subset(pot_site_sample['potentialSite'], potentialSite == TRUE)
  
  area_site_all = sum(expanse(vect(pot_site_all), unit = "km"))
  area_site1 = sum(expanse(vect(pot_site1), unit = "km"))
  area_site2 = sum(expanse(vect(pot_site2), unit = "km"))
  area_site3 = sum(expanse(vect(pot_site3), unit = "km"))
  area_site_sample = sum(expanse(vect(pot_siteS), unit = "km"))
  
  area_results = data.frame(area_site_all = area_site_all, area_site1 = area_site1, area_site2 = area_site2, area_site3 = area_site3, area_site_sample = area_site_sample)
  all_results[[colony]] = area_results
}

all_results_df = data.table::rbindlist(all_results, idcol = "colony")

write.csv(all_results_df, "pot_site_area_results_20230627.csv")

all_results_df = read.csv("pot_site_area_results_20230627.csv", row.names = 1)

all_results_df_long = tidyr::pivot_longer(all_results_df, -colony, names_to = "behaviour", values_to = "area_km")

library(RColorBrewer)
my.cols <- brewer.pal(6 , "Dark2")

ggplot(all_results_df_long, aes(x = behaviour, y = log10(area_km), group = behaviour, fill = behaviour)) + 
  geom_boxplot(alpha = 0.4, width=0.1) + 
  #geom_point() + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.5, alpha = 0.4,
               position=position_dodge(1)) +
  geom_violin(alpha = 0.6, color = NA) +
  scale_fill_manual(values = c("#66A61E", "#1B9E77", "#D95F02", "#7570B3", "#E7298A")) + theme_bw() 


library(lme4)

# Mixed effects model to explore differences in area of potential sites
lme1 = lmer(log10(area_km) ~ behaviour + (1|colony), data = all_results_df_long)

library(sjPlot)

plot_model(lme1, show.intercept = T)
plot_model(lme1, type = "pred") 

tab_model(lme1) 





# 
# pot_site = subset(pot_site_poly1['potentialSite'], potentialSite == TRUE)
# plot(pot_site)
# sum(expanse(vect(pot_site), unit = "km"))
# 
# pot_site = subset(pot_site_poly2['potentialSite'], potentialSite == TRUE)
# plot(pot_site)
# sum(expanse(vect(pot_site), unit = "km"))
# 
# pot_site = subset(pot_site_poly3['potentialSite'], potentialSite == TRUE)
# plot(pot_site)
# sum(expanse(vect(pot_site), unit = "km"))

