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
poisson_model <- glm(nkill ~ time_of_year + weaptype1_txt + targtype1_txt + country_txt + region + big_attack, 
              data = european_terror, family = poisson)


# negative binomial model
neg_binom_model <- glm.nb(nkill ~ time_of_year + weaptype1_txt + targtype1_txt + country_txt + region + big_attack,  
                 data = european_terror)



stargazer(poisson_model, neg_binom_model,  type = 'text')




AIC(poisson_model, neg_binom_model)

# this shows the improvement in the Negative Binomial vs Poisson
anova(poisson_model, neg_binom_model)


dispersion_statistic1 <-    sum(resid(poisson_model, type = "pearson")^2) / poisson_model$df.residual
dispersion_statistic2 <-    sum(resid(neg_binom_model, type = "pearson")^2) / neg_binom_model$df.residual





dispersiontest(poisson_model)
print(dispersion_statistic1)  # For Poisson
print(dispersion_statistic2)  # For Negative Binomial
 # this suggests nb over poisson but unsure on this reliability...



# https://biometry.github.io/APES/LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.pdf
# https://doi.org/10.1016/0304-4076(90)90014-K








# reference catagories
european_terror$time_of_year <- relevel(european_terror$time_of_year, ref = "Autumn")
european_terror$weaptype1_txt <- relevel(european_terror$weaptype1_txt, ref = "Firearms")








