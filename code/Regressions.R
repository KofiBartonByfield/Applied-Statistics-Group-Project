# clear environment
rm(list = ls())

# load in libraries
library(ggplot2)
library(stargazer)
library(MASS)
library(AER) # dispersiontest()
library(pscl) # zip

# read in the data
european_terror <- read.csv('data/european_terror.csv', stringsAsFactors = TRUE)





# run models

# poisson model
model1 <- glm(nkill ~ time_of_year, 
              data = european_terror, 
              family = poisson)

# negative binomial
model2 <- glm.nb(nkill ~ time_of_year, 
                 data = european_terror)

# negative binomail with extra term
model2.2 <- glm.nb(nkill ~ time_of_year + big_attack, 
                 data = european_terror)

# zip model
model3 <- zeroinfl(nkill ~ time_of_year, data=european_terror, dist="poisson")



# https://biometry.github.io/APES/LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.pdf
# https://doi.org/10.1016/0304-4076(90)90014-K
# look at over dispersion test ()
dispersiontest(model1)

sum(resid(model1, type = "pearson")^2) / model1$df.residual
sum(resid(model2, type = "pearson")^2) / model2$df.residual
sum(resid(model3, type = "pearson")^2) / model3$df.residual
sum(resid(model2.2, type = "pearson")^2) / model2.2$df.residual


stargazer(model1, model2, model3, model2.2,  type = 'text')


sum(model2.2$deviance / model2.2$residuals)


