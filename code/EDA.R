# clear environment
rm(list = ls())

# load in libraries
library(dplyr)
library(lubridate)


# read in the data
# downloaded from:
# https://www.start.umd.edu/download-global-terrorism-database

global_terror <- read.csv('data/globalterrorismdb_0522dist.csv')



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






## create a date column
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



# replace 'Unknown' days and months with the means
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
plot(global_terror$months_since_start)





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

# create new column on victims
global_terror$nvictims <- global_terror$nkill + global_terror$nwound

# no NAs
summary(global_terror$nvictims)

# just to get an understanding of the distribution
plot(global_terror$nvictims)


# create subset
subset_df <- global_terror %>% select(nvictims, months_since_start, gname, targtype1, weaptype1, country)




# Visualisations
# --------------

boxplot(subset_df$nvictims, 
        main = 'Boxplot of Number of Wounded', 
        ylab = 'Number of Wounded', 
        col = 'orange')


## log the y axis 
# ---------------------------

# log 1+nvictims (avoids log(0) error)
boxplot(log1p(subset_df$nvictims), 
        main = 'Log-Transformed Boxplot of Wounded', 
        ylab = 'log(1 + nwound)', 
        col = 'orange')



# distribution of the days since
# ------------------------------
plot(subset_df$months_since_start, 
     main = 'Distribution of Months Since Start', 
     xlab = 'Count', 
     ylab = 'Months Since Start', 
     pch = 16, 
     cex = 0.6)





table(subset_df$gname)

threshold <- 50  # Define a threshold (e.g., groups with <50 occurrences)
top_groups <- names(table(global_terror$gname)[table(global_terror$gname) >= threshold])

global_terror$gname <- ifelse(global_terror$gname %in% top_groups, global_terror$gname, "Other")
global_terror$gname <- as.factor(global_terror$gname)






