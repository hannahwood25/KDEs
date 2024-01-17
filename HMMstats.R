### Stats on HMMs for paper


library(tidyverse)
library(readr)


hmmstats <- read_csv("colony_state_parameters.CSV")

hmmaverages = hmmstats %>%
  dplyr::group_by(state) %>%
  dplyr::summarise(mean = mean(mean),
                   sd = mean(sd),
                   vmmean = mean(vm_mean),
                   conc = mean(concentration))

write.csv(hmmaverages, "hmmaverages.csv")



# Create a new variable that combines variable names with state
library(tidyr)

library(tidyr)

reshaped_data <- pivot_wider(hmmstats, id_cols = colony, names_from = state, values_from = c(mean, sd, vm_mean, concentration))

write.csv(reshaped_data, "hmmeachcolony.csv")
