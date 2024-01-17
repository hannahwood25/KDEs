### KDE overlap values


#### Packages 
library(tidyverse)
library(readr)
library(dplyr)

setwd("/Users/hannahwood/Desktop/R 5 sep/")

#############################################################################

####### Stats from trip which are complete and used to do the KDEs

### read in colony trip summaries

bar <- read_csv("BAR_UD_comparison_results.csv")
bem <- read_csv("BEM_UD_comparison_results.csv")
coq <- read_csv("COQ_UD_comparison_results.csv")
csy <- read_csv("CSY_UD_comparison_results.csv")
# fai <- read_csv("FAI_Kittiwake_KDE_state_ALL_repr_assessment.csv")
fil <- read_csv("FIL_UD_comparison_results.csv")
fow <- read_csv("FOW_UD_comparison_results.csv")
iom <- read_csv("IOM_UD_comparison_results.csv")
lam <- read_csv("LAM_UD_comparison_results.csv")
cop <- read_csv("ORK_COP_UD_comparison_results.csv")
mks <- read_csv("ORK_MKS_UD_comparison_results.csv")
puf <- read_csv("PUF_UD_comparison_results.csv")
# rat <- read_csv("RAT_UD_comparison_results.csv")
sab <- read_csv("SAB_UD_comparison_results.csv")
sci <- read_csv("SCI_STM_UD_comparison_results.csv")
win <- read_csv("WIN_UD_comparison_results.csv")

### bind into one file
# List of your data frames with names
data_frames <- list(bar = bar, bem = bem, coq = coq, csy = csy, fil = fil, fow = fow, 
                    iom = iom, lam = lam, cop = cop, mks = mks, puf = puf, sab = sab, 
                    sci = sci, win = win)

# Combine the data frames into a single data frame and add a column with the object names
combined_data <- bind_rows(.id = "file_name", data_frames)

# Print the combined data frame
print(combined_data)

write.csv(combined_data, "combined_data.csv")

###########################  remove pct_all

library(dplyr)

# List of your data frames with names
data_frames <- list(bar = bar, bem = bem, coq = coq, csy = csy, fil = fil, fow = fow, 
                    iom = iom, lam = lam, cop = cop, mks = mks, puf = puf, sab = sab, 
                    sci = sci, win = win)

# Combine the data frames into a single data frame and add a column with the object names
combined_data <- bind_rows(.id = "file_name", data_frames)

# Remove rows containing the phrase "pct_all" in any column
filtered_data <- combined_data %>%
  filter_all(all_vars(!str_detect(., "pct_all")))

# Print the filtered data frame
print(filtered_data)


# Print the filtered data frame
write.csv(filtered_data, "KDEoverlap_alldata.csv")

#################
## mean and standard deviation of behaviors captured overall

KDEoverlap <- filtered_data %>%
  group_by(behaviour) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE))

## save file
write.csv(KDEoverlap, "KDEoverlap_summary.csv")

###############
## mean and standard deviation of behaviors captured overall

KDEoverlap_colonies <- filtered_data %>%
  group_by(file_name, behaviour) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE))

head(KDEoverlap_colonies)

## save file
write.csv(KDEoverlap_colonies, "KDEoverlap_colonies.csv")
