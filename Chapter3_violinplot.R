### Chapter3_violinplot


### latest version
ggsave(plot = KDEviolins, ("CSYviolins_manuscrip.pdf"), width = 12, height = 8)


library(units)


all_results_df_long <- read.csv("COQ_UD_comparison_results.csv")
          

# Change facet labels
name.labs <- c("Percentage of full track KDE that is behaviour", "Percentage behaviour inside full track KDE")
names(name.labs) <- c("pct_all", "pct_beh")

# Plot both
KDEviolins <- ggplot(all_results_df_long, aes(x = behaviour, y = value, group = behaviour, fill = behaviour)) + 
  geom_violin(trim=TRUE, alpha = 0.5)+
  labs(title="",x="Behaviour", y = "Percentage")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.5, alpha = 0.4,
               position=position_dodge(1)) + 
  geom_boxplot(width=0.1) +
  scale_fill_brewer(palette="Dark2") + theme_bw() + 
  facet_wrap(~name, labeller = labeller(name = name.labs))

print(KDEviolins)


############ Just plot one
filtered_data <- subset(all_results_df_long, name == "pct_beh")

# Define color values for each behavior
behaviour_colors <- c("A" = "#1B9E77", "B" = "#D95F02", "C" = "#7570B3", "Sample" = "#E7298A")

# Convert 'behaviour' to a factor with specified order
filtered_data$behaviour <- factor(filtered_data$behaviour, levels = c("Sample", "A", "B", "C"))

KDEviolins <- ggplot(filtered_data, aes(x = behaviour, y = value, group = behaviour, fill = behaviour)) + 
  geom_violin(trim = TRUE, alpha = 0.5) +
  labs(title = "", x = "Behaviour", y = "Proportion") +
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, alpha = 0.4, position = position_dodge(1)) + 
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values = behaviour_colors) +  # Set fill colors directly
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text.x = element_text(size = 12)) +  # Increase font size of y-axis labels
  scale_y_continuous(breaks = seq(0.0, 1.00, by = 0.10), limits = c(0.0, 1.00)) + 
  scale_x_discrete(labels = c("A" = "Resting", "B" = "Foraging", "C" = "Transiting", "Sample" = "Sample"))

print(KDEviolins)


ggsave("manuscript_violins.pdf", plot = KDEviolins, width = 9, height = 8)
