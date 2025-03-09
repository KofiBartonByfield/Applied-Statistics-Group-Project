# clear environment
rm(list = ls())

# load in libraries
library(ggplot2)
library(stargazer)
library(MASS)
library(AER) # dispersiontest()
library(pscl) # zip
library(png)
library(grid)

# read in the data
european_terror <- read.csv('data/european_terror.csv', stringsAsFactors = TRUE)

# looking at the distirbution of the data:
grid.raster(readPNG("plots/fatality_dist.png"))
# we see an abundance of 0s




# run models
# ==========

# normal poisson model
model1 <- glm(nkill ~ time_of_year + weaptype1_txt + targtype1_txt, 
              data = european_terror, family = poisson)

# zip poisson model
model1.2 <- zeroinfl(nkill ~ time_of_year + weaptype1_txt + targtype1_txt , 
                     data=european_terror, dist="poisson")

# negative binomial model
model2 <- glm.nb(nkill ~ time_of_year + weaptype1_txt + targtype1_txt , 
                 data = european_terror)

# https://stats.oarc.ucla.edu/r/dae/zinb/#:~:text=Data%20Analysis%20Examples-,Zero%2DInflated%20Negative%20Binomial%20Regression%20%7C%20R%20Data%20Analysis%20Examples,over%2Ddispersed%20count%20outcome%20variables.
# zip negative binomial model 
model2.2 <- zeroinfl(nkill ~ time_of_year + weaptype1_txt + targtype1_txt, 
                     data = european_terror, dist = "negbin")



stargazer(model1, model1.2, model2, model2.2,  type = 'text')





AIC(model1, model1.2, model2, model2.2)

# this shows the improvement in the Negative Binomial vs Poisson
anova(model1, model2)


dispersion_statistic1 <-    sum(resid(model1, type = "pearson")^2) / model1$df.residual
dispersion_statistic1.2 <-  sum(resid(model1.2, type = "pearson")^2) / model1.2$df.residual
dispersion_statistic2 <-    sum(resid(model2, type = "pearson")^2) / model2$df.residual
dispersion_statistic2.2 <-  sum(resid(model2.2, type = "pearson")^2) / model2.2$df.residual

print(dispersion_statistic1)  # For Poisson
print(dispersion_statistic1.2)  # For ZI Poisson
print(dispersion_statistic2)  # For Negative Binomial
print(dispersion_statistic2.2)  # For ZI Negative Binomial

# This shows that ZI NB is the strongest model



# https://biometry.github.io/APES/LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.pdf
# https://doi.org/10.1016/0304-4076(90)90014-K








# reference catagories
european_terror$time_of_year <- relevel(european_terror$time_of_year, ref = "Autumn")
european_terror$weaptype1_txt <- relevel(european_terror$weaptype1_txt, ref = "Firearms")



model3 <- glm.nb(nkill ~ weaptype1_txt +  big_attack, 
                 data = european_terror)
model3.1 <- glm.nb(nkill ~ weaptype1_txt * big_attack, 
                 data = european_terror)

stargazer(model3, model3.1,  type = 'text')



model4 <- glm.nb(nkill ~ time_of_year, 
                 data = european_terror)
model4.1 <- glm.nb(nkill ~ time_of_year * big_attack, 
                   data = european_terror)

stargazer(model4, model4.1,  type = 'text')



model4 <- glm.nb(nkill ~ country_txt * big_attack, 
                   data = european_terror)

stargazer(model4,  type = 'text')

# 'big attacks' affect Greece, Italy a lot more...








