# clear environment
rm(list = ls())

# load in libraries
library(dplyr)
library(lubridate)


# ===================================
# read in the data (downloaded from:)
# https://www.start.umd.edu/download-global-terrorism-database
# ============================================================
global_terror <- read.csv('data/globalterrorismdb_0522dist.csv')


# look at column names
colnames(global_terror)

# 135 different variables
# 209,706 entries
dim(global_terror)



# ==================================
# filter for European Data only
# ==================================
european_terror <- global_terror %>% dplyr::select(nkill,
                                                   iyear,
                                                   imonth,
                                                   gname, 
                                                   targtype1_txt, 
                                                   weaptype1_txt,
                                                   attacktype1_txt,
                                                   region) %>%
  filter(region %in% c(8, 9)) # only keep Europe.


# ==================================
# function for checking missing data
# ==================================

check_missing_data <- function(data) {
  has_nonzero <- FALSE
  
  for (col in colnames(data)) {
    na_count <- sum(is.na(data[[col]]))
    unknown_count <- sum(data[[col]] == 0 | data[[col]] == 'Unknown', na.rm = TRUE)
    
    if (na_count > 0 || unknown_count > 0) {
      has_nonzero <- TRUE
      cat('\nColumn:', col, '\n')
      if (na_count > 0) cat('NA count:', na_count, '\n')
      if (unknown_count > 0) cat('Unknown count:', unknown_count, '\n')
    }
  }
  
  if (!has_nonzero) {
    cat('There are 0 NA or Unknown data\n')
  }
}


check_missing_data(european_terror)




# ============================
# create a time of year column
# ============================

# we have 0 'Unknown' YEAR entries
# with 0 NAs
sum(european_terror$iyear == 0)
sum(is.na(european_terror$iyear))

# we have 4 'Unknown' month entries 
# with 0 NAs
sum(european_terror$imonth == 0)
sum(is.na(european_terror$imonth))


# replace 'Unknown' months with the means
european_terror$imonth[european_terror$imonth == 0] <- round(mean(global_terror$imonth, na.rm = TRUE))


# time of year quantified
european_terror$time_of_year <-  ifelse(european_terror$imonth %in% c(3, 4, 5), 'Spring',
                         ifelse(european_terror$imonth %in% c(6, 7, 8), 'Summer',
                         ifelse(european_terror$imonth %in% c(9, 10, 11), 'Autumn',
                         ifelse(european_terror$imonth %in% c(12, 1, 2), 'Winter',
                  0)))) # NA for non-existent months


# check this has worked
sum(is.na(european_terror$time_of_year))
sum(european_terror$time_of_year == 0)






# =====================================================
# remove: unknown data, 0 fatality cases and region col
# https://pmc.ncbi.nlm.nih.gov/articles/PMC3995656/pdf/pone.0093732.pdf
# =====================================================================


summary(european_terror$nkill)
# 1080 NA values

# check missing data
check_missing_data(european_terror)

european_terror <- european_terror  %>%
  filter(gname != 'Unknown') %>%
  filter(targtype1_txt != 'Unknown') %>%
  filter(weaptype1_txt != 'Unknown') %>%
  filter(attacktype1_txt != 'Unknown') %>%
  filter(!is.na(nkill)) %>%
  filter(nkill != 0)%>%
  select(-region)

check_missing_data(european_terror)
colnames(european_terror)



# ============================================================
# remove the 'gname' of groups with < 60 appearances 
# (produces a top 10 of groups)
# ============================================================


# make a frequency table of group names
group_counts <- table(european_terror$gname)

# identify groups with fewer than 60 appearances
low_count_groups <- names(group_counts[group_counts < 60])

# replace those groups names with 'Other'
european_terror$gname <- ifelse(european_terror$gname %in% low_count_groups,
                                "Other", european_terror$gname)

# check the updated data set
# (now there are 10 group names)
table(european_terror$gname)








# write to csv
write.csv(european_terror, "data/european_terror.csv", row.names = FALSE)








# ======================
# inspecting the factors
# ======================

for (col in colnames(european_terror)) {
  print(paste("Number of unique values for column:", col))
  print(length(unique(european_terror[[col]])))
  
  # print(unique(european_terror[[col]]))
  print('')
  
}


unique(european_terror$targtype1_txt)

