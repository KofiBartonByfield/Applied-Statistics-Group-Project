# clear environment
rm(list = ls())

# load in libraries
library(ggplot2)
library(stargazer)
library(MASS)
library(AER) # dispersiontest()
# library(pscl) # zip
# library(png)
# library(grid)

# read in the data
european_terror <- read.csv('data/european_terror.csv', stringsAsFactors = TRUE)

# looking at the distribution of the data:
# grid.raster(readPNG("plots/fatality_dist.png"))
# we see an abundance of 0s


# run models
# ==========

# normal poisson model
poisson_model <- glm(nkill ~ ., 
              data = european_terror, family = poisson)


dispersiontest(poisson_model)

stargazer(poisson_model, type='text')




## ================================================
# releve baselines
## ================================================


# https://biometry.github.io/APES/LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.pdf
# https://doi.org/10.1016/0304-4076(90)90014-K








# reference catagories
european_terror$time_of_year <- relevel(european_terror$time_of_year, ref = "Autumn")
european_terror$weaptype1_txt <- relevel(european_terror$weaptype1_txt, ref = "Firearms")








