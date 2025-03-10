# clear environment
rm(list = ls())

# load in libraries
library(dplyr)
library(lubridate)



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
cols <- c('gname', 'targtype1_txt', 'weaptype1_txt', 'country_txt', 'attacktype1_txt')

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
global_terror$iday[global_terror$iday == 0] <- round(mean(global_terror$iday, na.rm = TRUE))


# Create a new date column
global_terror$date_recorded <- as.Date(
                                      paste(global_terror$iyear,
                                            global_terror$imonth,
                                            global_terror$iday,
                                            sep = '-'),
                                      format = '%Y-%m-%d'
)

# Now no NA's
summary(global_terror$date_recorded)


### create `months since start` column for the poisson regression
# idea from :
# https://doi.org/10.1093/oxfordjournals.aje.a117183


# find the earliest recorded date
start_date <- min(global_terror$date_recorded, na.rm = TRUE)

# compute months since start
global_terror$months_since_start <- interval(start_date, global_terror$date_recorded) %/% months(1)

summary(global_terror$months_since_start)

# just to get an understanding of the distribution
# plot(global_terror$months_since_start)








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
european_terror <- global_terror %>% dplyr::select(nkill,
                                            iyear,
                                            imonth,
                                            gname, 
                                            targtype1_txt, 
                                            weaptype1_txt,
                                            attacktype1_txt,
                                            suicide,
                                            country_txt,
                                            extended,
                                            # natlty1_txt,
                                            region) %>%
                                  filter(region %in% c(8, 9)) # only keep Europe.




head(european_terror)
dim(european_terror)
# View(european_terror)
# 22654 entries     



# Removing all the unclaimed attacks..
# https://pmc.ncbi.nlm.nih.gov/articles/PMC3995656/pdf/pone.0093732.pdf

# we have 8833 unclaimed attacks
sum(european_terror$gname == 'Unknown')

# we have unknown target types
sum(european_terror$targtype1_txt == 'Unknown')

# we have unknown weapon types
sum(european_terror$weaptype1_txt == 'Unknown')

sum(european_terror$nkill == 0)




# remove all unknown data and 0 fatalities
european_terror <- european_terror  %>%
  filter(gname != 'Unknown') %>%
  filter(targtype1_txt != 'Unknown') %>%
  filter(weaptype1_txt != 'Unknown') %>%
  filter(attacktype1_txt != 'Unknown') %>%
  filter(nkill != 0)

# ============================================================
# take out region
# ============================================================




# Quantifying 'Big Attacks'
# ==========================

# https://pmc.ncbi.nlm.nih.gov/articles/PMC3995656/pdf/pone.0093732.pdf

# total fatalities
# total_kills <- sum(european_terror$nkill, na.rm = TRUE)
# 
# # data for CCDF
# ccdf_data <- data.frame(
#   nkill = sort(unique(european_terror$nkill)),
#   proportion = sapply(sort(unique(european_terror$nkill)), function(x) {
#     sum(european_terror$nkill[european_terror$nkill >= x], na.rm = TRUE) / total_kills
#   })
# )
# 
# 
# 
# threshold_50 <- min(ccdf_data$nkill[ccdf_data$proportion <= 0.5])
# threshold_30 <- min(ccdf_data$nkill[ccdf_data$proportion <= 0.3])



# We defined a ‘big attack’ by determining the threshold above
# which attacks cumulatively account for 50% of all fatalities.
# = > 8

# european_terror$big_attack <- as.numeric(european_terror$nkill > threshold_50)
# 
# summary(european_terror)
# 
# 
# # how many of the european attacks are 'big attacks'
# sum(european_terror$big_attack) / length(european_terror$big_attack) *100
# # 4.05% of the attacks are 'big attacks'
# 
# 
# 
# dim(european_terror)
# 
# 
# names <- as.data.frame(table(european_terror$gname))



# ============================================================
# only keep the names of groups with more than x() appearances
# > 10
# ============================================================
# make a frequency table of group names
group_counts <- table(european_terror$gname)

# Identify groups with fewer than 60 appearances
low_count_groups <- names(group_counts[group_counts < 60])

# Replace those groups with 'Other'
european_terror$gname <- ifelse(european_terror$gname %in% low_count_groups, "Other", european_terror$gname)

# Check the updated dataset
table(european_terror$gname)






# write to csv
# write.csv(names, "data/gnames_table.csv")
write.csv(european_terror, "data/european_terror.csv", row.names = FALSE)





