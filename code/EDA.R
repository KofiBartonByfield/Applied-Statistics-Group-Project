# clear environment
rm(list = ls())

# load in libraries
library(dplyr)
library(lubridate)
library(ggplot2)



# read in the data
# downloaded from:
# https://www.start.umd.edu/download-global-terrorism-database
global_terror <- read.csv('data/globalterrorismdb_0522dist.csv')


# look at column names
colnames(global_terror)

# 135 different variables
# 209,706 entries
dim(global_terror)



# independent variables
# ---------------------
cols <- c('gname', 'targtype1', 'weaptype1', 'country')

for (col in cols) {
  cat('\nColumn:', col, '\n')
  
  # Count NA values
  cat('NA count:', sum(is.na(global_terror[[col]])))
  
  # Count 'Unknown' values
  cat('\nUnknown count:', sum(global_terror[[col]] == 0 | global_terror[[col]] == 'Unknown', na.rm = TRUE), '\n')
  
}
# we see we only have missing data for gname out of these variables



## create a time of year column
# ---------------------------

# we have 0 'Unknown' YEAR entries
# with 0 NAs
sum(global_terror$iyear == 0)
sum(is.na(global_terror$iyear))

# we have 20 'Unknown' month entries 
# with 0 NAs
sum(global_terror$imonth == 0)
sum(is.na(global_terror$imonth))

# we have 891 'Unknown' day entries
# with 0 NAs
sum(global_terror$iday == 0)
sum(is.na(global_terror$iday))



# replace 'Unknown' months with the means
global_terror$imonth[global_terror$imonth == 0] <- round(mean(global_terror$imonth, na.rm = TRUE))
# global_terror$iday[global_terror$iday == 0] <- round(mean(global_terror$iday, na.rm = TRUE))

# 
# # Create a new date column
# global_terror$date_recorded <- as.Date(
#                                       paste(global_terror$iyear, 
#                                             global_terror$imonth, 
#                                             global_terror$iday, 
#                                             sep = '-'),
#                                       format = '%Y-%m-%d'
# )
# 
# # Now no NA's
# summary(global_terror$date_recorded)
# 
# 
# ### create `months since start` column for the poisson regression
# # idea from :
# # https://doi.org/10.1093/oxfordjournals.aje.a117183
# 
# 
# # find the earliest recorded date
# start_date <- min(global_terror$date_recorded, na.rm = TRUE)
# 
# # compute months since start
# global_terror$months_since_start <- interval(start_date, global_terror$date_recorded) %/% months(1)
# 
# summary(global_terror$months_since_start)
# 
# # just to get an understanding of the distribution
# plot(global_terror$months_since_start)
# 
# 
# 
# 
# 



# # time of year quantified
global_terror$time_of_year <-  ifelse(global_terror$imonth %in% c(3, 4, 5), 'Spring',
                         ifelse(global_terror$imonth %in% c(6, 7, 8), 'Summer',
                         ifelse(global_terror$imonth %in% c(9, 10, 11), 'Autumn',
                         ifelse(global_terror$imonth %in% c(12, 1, 2), 'Winter',
                  0)))) # NA for non-existent months



sum(is.na(global_terror$time_of_year))
sum(global_terror$time_of_year == 0)







# dependent variable(s)
# ---------------------
summary(global_terror$nkill)
# 12527 NA values
# do we replace these with 0?


summary(global_terror$nwound) 
# 19936 NA values


# replacing NAs with 0
global_terror$nwound[is.na(global_terror$nwound)] <- 0
global_terror$nkill[is.na(global_terror$nkill)] <- 0

# check this worked
summary(global_terror$nkill)
summary(global_terror$nwound) 
# no NAs



# create subset
european_terror <- global_terror %>% select(#eventid,
                                            nkill,
                                            # nwound,
                                            # months_since_start, 
                                            time_of_year,
                                            # date_recorded,
                                            gname, 
                                            targtype1, 
                                            weaptype1, 
                                            country,
                                            region) %>%
                                  filter(region %in% c(8, 9)) # only keep Europe.




head(european_terror)
dim(european_terror)
# View(european_terror)
# 22654 entries     




# Quantifying 'Big Attacks'
# ==========================

# https://pmc.ncbi.nlm.nih.gov/articles/PMC3995656/pdf/pone.0093732.pdf

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
  
  geom_label(aes(x = threshold_30, y = 0.3, label = paste0('x = ', threshold_30, '\ny = 0.3')), 
             hjust = 0, vjust = -0.1, size = 4, colour = "black", fill = "ivory") +
  
  geom_label(aes(x = threshold_50, y = 0.5, label = paste0('x = ', threshold_50, '\ny = 0.5')), 
             hjust = -0.1, vjust = -0.05, size = 4, colour = "black", fill = "ivory") +

  
  labs(title = "Thresholds Used to Define Big Attacks",
    x = "Number of Fatalities per Attack",
    y = "Fraction of Total Fatalities"
  ) +
  scale_x_log10() +
  theme(
    legend.position = c(0.8, 0.8),   # Move the legend inside the plot (adjust the coordinates as needed)
    legend.title = element_blank(),   # Remove the legend title
    legend.text = element_text(size = 15),  # Adjust legend text size if needed
    text = element_text(size = 14),  # Adjust general text size
    panel.background = element_rect(fill = "white"),  # White background for plot area
    plot.background = element_rect(fill = "white"),  # White background for the entire plot
    axis.line = element_line(color = "black", size = 0.5)  # Make axis lines visible
  )

ggsave("plots/fatalities_cdf.png")


# Figure 1. Thresholds used to define big attacks. The bolded line shows the percentage of total fatalities accounted for at different thresholds.
# For example, attacks killing 49 or more people account for 30% of fatalities from all terrorist attacks combined. Attacks killing 21 or more people
# account for 50% of all fatalities. We also plot the empirical cumulative distribution function. This shows, for example, that nearly 50% of all terrorist
# attacks results in zero fatalities, and approxima


# We defined a ‘big attack’ by determining the threshold above
# which attacks cumulatively account for 50% of all fatalities.
# = > 6

european_terror$big_attack <- as.numeric(european_terror$nkill > threshold_50)

summary(european_terror)


# how many of the european attacks are 'big attacks'
sum(european_terror$big_attack) / length(european_terror$big_attack) *100
# 1.20% of the attacks are 'big attacks'




# Removing all the unclaimed attacks..
# https://pmc.ncbi.nlm.nih.gov/articles/PMC3995656/pdf/pone.0093732.pdf

# we have 8833 unclaimed attacks
sum(european_terror$gname == 'Unknown')


european_terror <- european_terror %>% filter(gname != 'Unknown')



dim(european_terror)


names <- as.data.frame(table(european_terror$gname))

write.csv(names, "data/gnames_table.csv")


write.csv(european_terror, "data/european_terror.csv")












































# 
# # Visualisations
# # ==========================================
# 
# boxplot(european_terror$nvictims, 
#         main = 'Boxplot of Number of Victims', 
#         ylab = 'Number of Wounded', 
#         col = 'orange')
# 
# 
# ## log the y axis 
# # ---------------------------
# 
# # log 1+nvictims (avoids log(0) error)
# boxplot(log1p(european_terror$nvictims), 
#         main = 'Log-Transformed Boxplot of Victims', 
#         ylab = 'log(1 + nwound)', 
#         col = 'orange')
# 
# 
# 
# # distribution of the days since
# # ------------------------------
# # Create a histogram
# hist(european_terror$months_since_start, 
#      main = 'Distribution of European Terror Attacks Over Time', 
#      xlab = sprintf("Months Since %s", min(european_terror$date_recorded)),
#      ylab = 'Count', 
#      col = 'skyblue', 
#      border = 'black', 
#      breaks = 50, 
#      probability = TRUE)  
# 
# # Add density line
# lines(density(european_terror$months_since_start, na.rm = TRUE), 
#       col = 'red', 
#       lwd = 2)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ============================================================================ #
# 
# colnames(european_terror)
# 
# model1 <- glm(nvictims ~ as.factor(country),
#               data = european_terror,
#               family = poisson)
# 
# model2 <- glm(nvictims ~ as.factor(targtype1) ,
#              data = european_terror,
#              family = poisson)
# 
# # model2 <- glm(nvictims ~ . ,
# #               data = european_terror,
# #               family = poisson)
# 
# 
# stargazer::stargazer(model1, model2, type = 'text')
# 
# 
# 
# 
# 
# 
# 
# 
# qqnorm(european_terror$nvictims)
# qqline(european_terror$nvictims, col = "red")
# 
# 
# 
# 
# european_terror
# 





