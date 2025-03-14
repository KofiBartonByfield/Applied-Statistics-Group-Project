# clear environment
rm(list = ls())

# load in libraries
library(ggplot2)
library(stargazer)
library(MASS)
library(AER) # dispersiontest()
library(fixest)


# read in the data
european_terror <- read.csv('data/european_terror.csv', stringsAsFactors = TRUE)

# looking at the distribution of the data:
# grid.raster(readPNG("plots/fatality_dist.png"))
# we see an abundance of 0s



## ==================
# re-level  baselines
## ==================

# function to find the most comon column
most_common_per_column <- function(data) {

  for (col in colnames(data)) {
    
    # skip numerical columns
    if (is.numeric(data[[col]])) next  

    max_count <- max(table(data[[col]], useNA = "no"))
    max_category <- names(freq_table[freq_table == max_count])
    
    if (max_count > 0) {
      cat('\nColumn:', col, '\n')
      cat('Most common category:', paste(max_category, collapse = ", "), '\n')
      cat('Count:', max_count, '\n')
    }
  }

}

# find the most comon cols
most_common_per_column(european_terror)


# --------------------------------
# re-level all with top occurrence
# (except 'gname' which uses other)
# ---------------------------------

european_terror$gname <- relevel(european_terror$gname, 
                                 ref = "Other")

european_terror$targtype1_txt  <- relevel(european_terror$targtype1_txt , 
                                          ref = "Private Citizens & Property")

european_terror$weaptype1_txt  <- relevel(european_terror$weaptype1_txt, 
                                          ref = "Firearms")

european_terror$attacktype1_txt   <- relevel(european_terror$attacktype1_txt, 
                                             ref = "Assassination")

european_terror$country_txt  <- relevel(european_terror$country_txt, 
                                        ref = "United Kingdom")

european_terror$time_of_year <- relevel(european_terror$time_of_year, 
                                        ref = "Summer")







# ==========
# run models
# ==========

# normal poisson model
poisson_model <- glm(nkill ~ ., 
              data = european_terror, family = poisson)

nb_model <- glm.nb(nkill ~ ., 
                   data = european_terror) 



# check dispertion score
dispersiontest(poisson_model)
# not amazing...


# look at the general plots from the model
plot(poisson_model)


# Q-Q plot
residuals_poisson <- residuals(poisson_model, type = "pearson") 
qqnorm(residuals_poisson, main = "Q-Q Plot of Poisson Residuals")
qqline(residuals_poisson, col = "blue")

residuals_nb <- residuals(nb_model, type = "pearson") 
qqnorm(residuals_nb, main = "Q-Q Plot of Nb Residuals")
qqline(residuals_nb, col = "blue")





stargazer(poisson_model, type='text')
# IRA has negative effect on kills (from baseline)


# =============
# Fixed Effects
# =============

fe_1 <- fepois(nkill ~ targtype1_txt + weaptype1_txt + attacktype1_txt |  country_txt, data = european_terror)
fe_2 <- fepois(nkill ~ targtype1_txt + weaptype1_txt + attacktype1_txt |  country_txt + iyear, data = european_terror)
fe_3 <- fepois(nkill ~ targtype1_txt + weaptype1_txt + attacktype1_txt |  country_txt + iyear time_of_year, data = european_terror)


etable(fe_1,fe_2, fe_3,  
       vcov = "twoway", 
       headers = c("Country", 'Country+year', 'country+year+ToY'))










