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






# time of year quantified
global_terror$time_of_year <-  ifelse(global_terror$imonth %in% c(3, 4, 5), 'Spring',
                         ifelse(global_terror$imonth %in% c(6, 7, 8), 'Summer',
                         ifelse(global_terror$imonth %in% c(9, 10, 11), 'Autumn',
                         ifelse(global_terror$imonth %in% c(12, 1, 2), 'Winter',
                  0)))) # NA for non-existent months













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
europe_terror <- global_terror %>% select(nvictims, 
                                      months_since_start, 
                                      time_of_year,
                                      date_recorded,
                                      gname, 
                                      targtype1, 
                                      weaptype1, 
                                      country,
                                      region) %>%
                            filter(region %in% c(8, 9)) # only keep Europe.


head(europe_terror)
dim(europe_terror)
# 22654 entries     
# 9 columns






# count the number of attacks per group
sorted_gname <- sort(table(europe_terror$gname), decreasing = TRUE)

# identify groups with more than 100 attacks
selected_groups <- names(sorted_gname[sorted_gname > 100])

# keep selected groups, replace others with "Other"
europe_terror$gname <- ifelse(europe_terror$gname %in% selected_groups, 
                              europe_terror$gname, "Other")

# check results
table(europe_terror$gname)


View(europe_terror)









# Visualisations
# --------------

boxplot(europe_terror$nvictims, 
        main = 'Boxplot of Number of Wounded', 
        ylab = 'Number of Wounded', 
        col = 'orange')


## log the y axis 
# ---------------------------

# log 1+nvictims (avoids log(0) error)
boxplot(log1p(europe_terror$nvictims), 
        main = 'Log-Transformed Boxplot of Wounded', 
        ylab = 'log(1 + nwound)', 
        col = 'orange')



# distribution of the days since
# ------------------------------
# Create a histogram
hist(europe_terror$months_since_start, 
     main = 'Distribution of European Terror Attacks Over Time', 
     xlab = sprintf("Months Since %s", min(europe_terror$date_recorded)),
     ylab = 'Count', 
     col = 'skyblue', 
     border = 'black', 
     breaks = 50, 
     probability = TRUE)  

# Add density line
lines(density(europe_terror$months_since_start, na.rm = TRUE), 
      col = 'red', 
      lwd = 2)










# ============================================================================ #

colnames(europe_terror)

model1 <- glm(nvictims ~ as.factor(country),
              data = europe_terror,
              family = poisson)

model2 <- glm(nvictims ~ as.factor(targtype1) ,
             data = europe_terror,
             family = poisson)

model2 <- glm(nvictims ~ . ,
              data = europe_terror,
              family = poisson)


stargazer::stargazer(model2, type = 'text')






# 
# 
# qqnorm(europe_terror$)
# qqline(judge_means$lib_vote_share, col = "red")









