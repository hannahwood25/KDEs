Chapter3_figure1_HMMtracks

### packages

library(tidyverse)
library(moveHMM)
library(readr)
library(sf)
library(ggplot2)
# Plot states

complete_trips_df <- read.csv("BEMtripswithstates.csv")

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
  # Extract just the points for the current state
  complete_trips_df_state <- subset(complete_trips_df, state == s)
  
  # Plot and print each state separately
  p <- ggplot(complete_trips_df_state, aes(x = Longitude, y = Latitude)) + geom_point()
  print(p)
}


######## Plotting each state separately


# Define the color scale outside the loop
color_scale <- scale_color_manual(
  name = "Behaviour",
  labels = legend_labels,
  values = c("1" = "#009E73", "2" = "#E69F00", "3" = "#8494FF")
)

# Initial plot with color scale
initial_plot <- ggplot(complete_trips_df, aes(x = Longitude, y = Latitude, color = factor(state))) +
  geom_point(alpha = 0.2) +
  color_scale +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

# Save the initial plot
ggsave(paste0(colony, "statesmap.pdf"), width = 12, height = 8)

# Loop through each state and plot with the same color scale
for (s in 1:3) {
  # Extract just the points for the current state
  complete_trips_df_state <- subset(complete_trips_df, state == s)
  
  # Plot with the same color scale
  p <- ggplot(complete_trips_df_state, aes(x = Longitude, y = Latitude, color = factor(state))) +
    geom_point() +
    color_scale
  
  # Print or save the plot as needed
  print(p)
  ggsave(paste0(colony, "state_", s, ".pdf"), plot = p, width = 8, height = 6)
}

getwd()


########################

# Initial plot without legend
initial_plot <- ggplot(complete_trips_df, aes(x = Longitude, y = Latitude, color = factor(state))) +
  geom_point(alpha = 0.2)

print(initial_plot)

# Save the initial plot
ggsave(paste0(colony, "statesmap_nolegend.pdf"), plot = initial_plot, width = 12, height = 8)



##########################
# Determine the size of the smallest state
min_state_size <- min(table(complete_trips_df$state))

### min_state_size is 18975

sample = sample_n(complete_trips_df, size = min_state_size)


sampleplot <- ggplot(sample, aes(x = Longitude, y = Latitude)) + geom_point()

p <- ggplot(sample, aes(x = Longitude, y = Latitude, color = factor(state))) +
  geom_point() +
  theme(legend.position = "none") 

all <- ggplot(complete_trips_df, aes(x = Longitude, y = Latitude, color = factor(state))) +
  geom_point() +
  theme(legend.position = "none") 

p


p <- ggplot(sample, aes(x = Longitude, y = Latitude)) +
  geom_point(color = "hotpink") +
  theme(legend.position = "none")  # Remove the legend for this plot

# Print or save the plot as needed
print(p)
