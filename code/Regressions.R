# clear environment
rm(list = ls())

# load in libraries
library(ggplot2)
library(stargazer)
library(MASS)

# read in the data
european_terror <- read.csv('data/european_terror.csv', stringsAsFactors = TRUE)



# run models
model1 <- glm(nkill ~ time_of_year, 
              data = european_terror, 
              family = poisson)

model2 <- glm.nb(nkill ~ time_of_year, 
                 data = european_terror)

model2.2 <- glm.nb(nkill ~ time_of_year + big_attack, 
                 data = european_terror)


stargazer(model1, model2, model2.2,  type = 'text')




model3 <- glm(nkill ~ targtype1_txt * big_attack, 
              data = european_terror, 
              family = poisson)


model4 <- glm.nb(nkill ~ targtype1_txt * big_attack, 
                 data = european_terror)

stargazer(model3, model4,  type = 'text')





