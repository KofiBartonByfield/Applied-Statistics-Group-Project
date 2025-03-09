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
  geom_step(aes(colour = 'Fraction of fatalities'), size = 1) + 
  
  geom_segment(aes(x = threshold_30, xend = threshold_30, y = 0, yend = 0.3), 
               linetype = "dotted", colour = "black", size = 1) + 
  geom_segment(aes(x = threshold_50, xend = threshold_50, y = 0, yend = 0.5), 
               linetype = "dotted", colour = "black", size = 1) + 
  
  geom_text(aes(x = threshold_30, y = 0.3, label = paste0('x = ', threshold_30, '\ny = 0.3')), 
            hjust = -0.2, vjust = -0.3, size = 3, colour = "black") +
  
  geom_text(aes(x = threshold_50, y = 0.5, label = paste0(' x = ', threshold_50, '\ny = 0.5')), 
            hjust = -0.2, vjust = -0.3, size = 3, colour = "black") + 
  
  labs(title = "Thresholds Used to Define Big Attacks",
       x = "Number of Fatalities per Attack",
       y = "Fraction of Total Fatalities"
  ) +
  scale_x_log10() +
  theme(
    legend.position = c(0.8, 0.8),   # Move the legend inside the plot (adjust the coordinates as needed)
    legend.title = element_blank(),   # Remove the legend title
    # legend.text = element_text(size = 15),  # Adjust legend text size if needed
    # text = element_text(size = 14),  # Adjust general text size
    panel.background = element_rect(fill = "white"),  # White background for plot area
    plot.background = element_rect(fill = "white"),  # White background for the entire plot
    axis.line = element_line(color = "black", size = 0.5)  # Make axis lines visible
  )

ggsave("plots/fatalities_cdf.png")

# Figure 1. Thresholds used to define big attacks. The red line shows the 
# percentage of total fatalities accounted for at different thresholds.
# Showing attacks killing 35 or more people account for 30% of fatalities 
# from the European terrorist attacks. 
# Attacks killing 8 or more people account for 50% of all fatalities. 
# We also see that only 
sum(european_terror$big_attack) / length(european_terror$big_attack) *100
# 1.20% of the attacks are 'big attacks' despite making up 50% of the fatalities









# Time Series data
# ================

european_terror$date_recorded <- as.Date(european_terror$date_recorded, format = "%Y-%m-%d")

ggplot(european_terror, aes(x = date_recorded)) +
  geom_histogram(binwidth = 400, 
                 fill = 'lightgrey', 
                 color = 'black',  
                 size = 0.2) +
  labs(title = 'Distribution of European Terror Attacks Over Time',
       x = 'Months',
       y = 'Count') +
  scale_x_date(
    breaks = seq(from = min(european_terror$date_recorded), 
                 to = max(european_terror$date_recorded), 
                 by = "60 months"),
    date_labels = "%Y"  
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35,hjust = 1),
        panel.background = element_rect(fill = "white"),  # White background for plot area
        plot.background = element_rect(fill = "white"),  # White background for the entire plot
        axis.line = element_line(color = "black", size = 0.5),
        panel.grid = element_blank())

ggsave("plots/occurance_hist.png")




