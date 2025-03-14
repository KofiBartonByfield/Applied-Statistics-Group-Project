# clear environment
rm(list = ls())

# load in libraries
library(ggplot2)

# read in the data
european_terror <- read.csv('data/european_terror.csv')





# Quantifying 'Big Attacks'
# ==========================

# total fatalities
total_kills <- sum(european_terror$nkill, na.rm = TRUE)

# data for CCDF
ccdf_data <- data.frame(
  nkill = sort(unique(european_terror$nkill)),
  proportion = sapply(sort(unique(european_terror$nkill)), function(x) {
    sum(european_terror$nkill[european_terror$nkill >= x], na.rm = TRUE) / total_kills
  })
)

threshold_50 <- min(ccdf_data$nkill[ccdf_data$proportion <= 0.5])
threshold_30 <- min(ccdf_data$nkill[ccdf_data$proportion <= 0.3])


# plot CCDF
ggplot(ccdf_data, aes(x = nkill, y = proportion)) +
  geom_step(aes(colour = 'Fraction of fatalities'), size = 1.2, colour = "black") + 
  
  # Threshold lines with more subtle styling
  geom_segment(aes(x = threshold_30, xend = threshold_30, y = 0, yend = 0.3), 
               linetype = "dotted", colour = "gray30", size = 1) + 
  
  geom_segment(aes(x = threshold_50, xend = threshold_50, y = 0, yend = 0.5), 
               linetype = "dotted", colour = "gray30", size = 1) + 
  
  # Threshold text with adjusted position and cleaner font
  geom_text(aes(x = threshold_30, y = 0.3, label = paste0('x = ', threshold_30, '\n y = 0.3')), 
            hjust = 1, vjust = -0.8, size = 3.5, colour = "black", fontface = "plain") +
  
  geom_text(aes(x = threshold_50, y = 0.5, label = paste0('x = ', threshold_50, '\n y = 0.5')), 
            hjust = 1, vjust = -0.8, size = 3.5, colour = "black", fontface = "plain") + 
  
  # Titles and axis labels with refined fonts
  labs(title = "Thresholds Defining Significant Attacks",
       subtitle = "Number of fatalities per attack versus fraction of total fatalities",
       x = "Number of Fatalities per Attack",
       y = "Fraction of Total Fatalities") +
  
  # Log scale for the x-axis
  scale_x_log10() +
  
  # Clean and professional theme adjustments
  theme_minimal() +
  theme(
    legend.position = "none",   # Remove legend for simplicity
    axis.text = element_text(size = 11, colour = "black"),  # Adjusted axis text size and colour
    axis.title = element_text(size = 13, colour = "black", face = "bold"),  # Bold axis titles
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, colour = "darkblue"),  # Title style
    plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "gray40"),  # Subtitle style
    panel.grid = element_line(colour = "lightgray", size = 0.5), # Subtle grid lines
    panel.grid.major = element_line(size = 0.5),  # Grid lines for clarity
    panel.grid.minor = element_blank(),  # No minor grid lines
    axis.line = element_line(color = "black", size = 0.5),  # Clear axis lines
    plot.background = element_rect(fill = "white", colour = "white")  # Clean plot background
  )

ggsave("plots/fatalities_cdf.png")

# Figure 1. Thresholds used to define big attacks. The red line shows the 
# percentage of total fatalities accounted for at different thresholds.
# Showing attacks killing 35 or more people account for 30% of fatalities 
# from the European terrorist attacks. 
# Attacks killing 8 or more people account for 50% of all fatalities. 
# We also see that only 









# Time Series data
# ================

ggplot(european_terror, aes(x = iyear)) +
  geom_histogram(binwidth = 1, 
                 fill = 'lightgrey', 
                 color = 'black',  
                 size = 0.2) +
  labs(title = 'Distribution of European Terror Attacks Over Time',
       x = 'Year',
       y = 'Count') +
  scale_x_continuous(
    breaks = seq(from = min(european_terror$iyear, na.rm = TRUE), 
                 to = max(european_terror$iyear, na.rm = TRUE), 
                 by = 5)  
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.background = element_rect(fill = "white"),  
        plot.background = element_rect(fill = "white"),  
        axis.line = element_line(color = "black", size = 0.5),
        panel.grid = element_blank())


ggsave("plots/occurance_hist.png")



# Distribution of Fatalities
# ==========================
ggplot(european_terror, aes(x = nkill)) +
  geom_histogram(binwidth = 5, fill = "lightblue",color = "black", size = 0.2, na.rm = TRUE) +
  scale_y_continuous(trans = "log10") +
  labs(title = "Distribution of Number of Fatalities",
       x = "Number of Fatalities", 
       y = "Frequency (log)") +
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 35,hjust = 1),
        panel.background = element_rect(fill = "white"),  # White background for plot area
        plot.background = element_rect(fill = "white"),  # White background for the entire plot
        axis.line = element_line(color = "black", size = 0.5),
        panel.grid = element_blank())
ggsave("plots/fatality_dist.png", width = 7, height = 8, dpi = 300, limitsize = FALSE)



