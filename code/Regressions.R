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


## ==================
# re-level  baselines
## ==================

# function to find the most common column
most_common_per_column <- function(data) {

  for (col in colnames(data)) {
    

    # skip numerical columns
    if (is.numeric(data[[col]])) next  
    
    
    freq_table <- table(data[[col]])
    max_count <- max(table(data[[col]], useNA = "no"))
    max_category <- names(freq_table[freq_table == max_count])
    
    if (max_count > 0) {
      cat('\nColumn:', col, '\n')
      cat('Most common category:', paste(max_category, collapse = ", "), '\n')
      cat('Count:', max_count, '\n')
    }
  }

}

# find the most common cols
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

european_terror$time_of_year <- relevel(european_terror$time_of_year, 
                                        ref = "Summer")







# ==========
# run models
# ==========

ols_model <- lm(nkill ~ ., 
                data = european_terror)


# normal poisson model
poisson_model <- glm(nkill ~ ., 
              data = european_terror, family = poisson)


nb_model <- glm.nb(nkill ~ ., 
                   data = european_terror) 

# ================
# model evaluation
# ================


# ---------
# Q-Q plots
# ---------

png("plots/qqplot_ols.png")
par(mar = c(5, 5, 4, 2) + 0.1)
residuals_ols <- residuals(ols_model, type = "pearson") 
qqnorm(residuals_ols, main = "Q-Q Plot of OLS Model", 
       cex.main = 2,
       cex.axis = 1.2,
       cex.lab = 2)
qqline(residuals_ols, col = "blue", lwd = 2.5)
dev.off()

png("plots/qqplot_poisson.png")
par(mar = c(5, 5, 4, 2) + 0.1)
residuals_poisson <- residuals(poisson_model, type = "pearson") 
qqnorm(residuals_poisson, main = "Q-Q Plot of Poisson Model", 
       cex.main = 2,
       cex.axis = 1.2,
       cex.lab = 2)
qqline(residuals_poisson, col = "blue", lwd = 2.5)
dev.off()

png("plots/qqplot_nb.png")
par(mar = c(5, 5, 4, 2) + 0.1)
residuals_nb <- residuals(nb_model, type = "pearson") 
qqnorm(residuals_nb, main = "Q-Q Plot of Negative Binomial Model", 
       cex.main = 2,
       cex.axis = 1.2,
       cex.lab = 2)
qqline(residuals_nb, col = "blue", lwd = 2.5)
dev.off()


# dispersion test
dispersiontest(poisson_model)

# check AIC
AIC(ols_model, poisson_model, nb_model)

library(xtable)


aic_values <- data.frame(
  Model = c("OLS model", "Poisson Model", "Negative Binomial Model"),
  AIC = c(AIC(ols_model), AIC(poisson_model), AIC(nb_model))
)



sink('tables/aic_table.tex')

# Generate and print the LaTeX code without row names
print(xtable(aic_values), type = "latex", include.rownames = FALSE)

# Close the sink to stop redirecting output to the file
sink()



# ========================
# interpreting the outputs
# ======================== 

# compare their outputs
stargazer(ols_model, poisson_model, nb_model, type = 'text')






stargazer(nb_model, 
          type = 'text',
          omit = c('iyear', 'imonth'),
          apply.coef = exp,
          out = 'tables/complete_model.tex',
          covariate.labels = c('Basque Fatherland and Freedom',
                               'Chechen Rebels',
                               "Donetsk People's Republic",
                               'Irish National Liberation Army (INLA)',
                               'Irish Republican Army (IRA)',
                               'Irish Republican Extremists',
                               'Protestant extremists',
                               'Ulster Freedom Fighters (UFF)',
                               'Ulster Volunteer Force (UVF)',
                               'Target = Airport',
                               'Target = Business',
                               'Target = Educational Institution',
                               'Target = Food or Water Supply',
                               'Target = Government (Diplomatic)',
                               'Target =  Government (General)',
                               'Target = Journalists',
                               'Target = Maritime',
                               'Target = Military',
                               'Target = NGO',
                               'Target = Other',
                               'Target = Police',
                               'Target = Religious Figure or Institution',
                               'Target = Telecommunication',
                               'Target = Terrorist or Non-State Militia',
                               'Target = Tourists',
                               'Target = Transportation',
                               'Target = Utilities',
                               'Target = Violent Political Party',
                               'Weapon = Chemical',
                               'Weapon = Explosives',
                               'Weapon = Incendiary',
                               'Weapon = Melee',
                               'Weapon = Other',
                               'Weapon = Vehicle',
                               'Armed Assault',
                               'Bombing or Explosion',
                               'Facility or Infrastructure Attack',
                               'Hijacking',
                               'Hostage Taking (Barricade)',
                               'Hostage Taking (Kidnapping)',
                               'Unarmed Assault',
                               'Time of Year = Autumn',
                               'Time of Year = Spring',
                               'Time of Year = Winter'
                              ),
          title = 'Full Model Output',
          column.sep.width = "0.1pt"
            )





# ==================
# Attack type table
# ==================
stargazer(nb_model, 
          type = "text", 
          apply.coef = exp, 
          omit = c('targ', 'gname', 'weap' , 'iyear', 'imonth' , 'time_of', 'Const'),
          out = 'tables/attack_model.tex',
          covariate.labels = c('Armed Assault',
                                'Bombing or Explosion',
                                'Facility or Infrastructure Attack',
                                'Hijacking',
                                'Hostage Taking (Barricade)',
                                'Hostage Taking (Kidnapping)',
                                'Unarmed Assault'),
          dep.var.labels   = 'Number of Fatalities',
          title = 'Type of Attack')  




# ==================
# Weapon type table
# ==================
stargazer(nb_model, 
          type = "text", 
          apply.coef = exp, 
          out = 'tables/weapon_model.tex',
          omit = c('attack', 'targ', 'gname' , 'iyear', 'imonth' , 'time_of', 'Const'),
          covariate.labels = c('Weapon = Chemical',
                               'Weapon = Explosives',
                               'Weapon = Incendiary',
                               'Weapon = Melee',
                               'Weapon = Other',
                               'Weapon = Vehicle'),
          dep.var.labels   = 'Number of Fatalities',
          title = 'Weapon Used in the Attack'
          
)  


# ==================
# Name type table
# ==================
stargazer(nb_model, 
          type = "text", 
          apply.coef = exp, 
          out = 'tables/name_model.tex',
          omit = c('attack', 'targ', 'weap' , 'iyear', 'imonth' , 'time_of', 'Const'),
          covariate.labels = c('Basque Fatherland and Freedom',
                               'Chechen Rebels',
                               "Donetsk People's Republic",
                               'Irish National Liberation Army (INLA)',
                               'Irish Republican Army (IRA)',
                               'Irish Republican Extremists',
                               'Protestant extremists',
                               'Ulster Freedom Fighters (UFF)',
                               'Ulster Volunteer Force (UVF)'),
          dep.var.labels   = 'Number of Fatalities',
          title = 'Terrorist Group Name'
)  






# ==================
# Target type table
# ==================
stargazer(nb_model, 
          type = "text", 
          apply.coef = exp, 
          out = 'tables/target_model.tex',
          omit = c('attack', 'gname', 'weap' , 'iyear', 'imonth' , 'time_of', 'Const'),
          covariate.labels = c('Target = Airport',
                               'Target = Business',
                               'Target = Educational Institution',
                               'Target = Food or Water Supply',
                               'Target = Government (Diplomatic)',
                               'Target = Government (General)',
                               'Target = Journalists',
                               'Target = Maritime',
                               'Target = Military',
                               'Target = NGO',
                               'Target = Other',
                               'Target = Police',
                               'Target = Religious Figure or Institution',
                               'Target = Telecommunication',
                               'Target = Terrorist or Non-State Militia',
                               'Target = Tourists',
                               'Target = Transportation',
                               'Target = Utilities',
                               'Target = Violent Political Party'),
          dep.var.labels   = 'Number of Fatalities',
          title = 'Target of Attack'
)  

# ----------------------------------
# Removing Non Significant Variables
# ----------------------------------

stargazer(nb_model, 
          type = "text", 
          apply.coef = exp, 
          out = 'tables/partial_target_model.tex',
          omit = c('attack', 'gname', 'weap' , 'iyear', 'imonth' , 'time_of', 'Const',
                   'Food or Water Supply',
                   'NGO',
                   'Other',
                   'Telecommunication'),
          covariate.labels = c('Target = Airport',
                               'Target = Business',
                               'Target = Educational Institution',
                               'Target = Government (Diplomatic)',
                               'Target =  Government (General)',
                               'Target = Journalists',
                               'Target = Maritime',
                               'Target = Military',
                               'Target = Police',
                               'Target = Religious Figure or Institution',
                               'Target = Terrorist or Non-State Militia',
                               'Target = Tourists',
                               'Target = Transportation',
                               'Target = Utilities',
                               'Target = Violent Political Party'),
          dep.var.labels   = 'Number of Fatalities',
          title = 'Target of Attack'
)  






















## ==================
### Individual Models
## ==================
# 
# 
# 
# # Gname
# stargazer(glm.nb(nkill ~ gname, data = european_terror), 
#           type = 'text',
#           apply.coef = exp,
#           out = 'tables/gname_model.tex',
#           title = 'Terrorist Group Name',
#           covariate.labels = c('Basque Fatherland and Freedom',
#                                'Chechen Rebels',
#                                "Donetsk People's Republic",
#                                'Irish National Liberation Army (INLA)',
#                                'Irish Republican Army (IRA)',
#                                'Irish Republican Extremists',
#                                'Protestant extremists',
#                                'Ulster Freedom Fighters (UFF)',
#                                'Ulster Volunteer Force (UVF)',
#                                'Other')
# )
# 
# 
# 
# # Weapon Type
# stargazer(glm.nb(nkill ~ weaptype1_txt, data = european_terror), 
#           type = 'text',
#           apply.coef = exp,
#           out = 'tables/weapon_model.tex',
#           title = 'Weapon Used in the Attack',
#           covariate.labels = c('Weapon = Chemical',
#                                'Weapon = Explosives',
#                                'Weapon = Incendiary',
#                                'Weapon = Melee',
#                                'Weapon = Other',
#                                'Weapon = Vehicle',
#                                'Weapon = Firearms'))
# 
# # Target Type
# stargazer(glm.nb(nkill ~ targtype1_txt , data = european_terror), 
#           type = 'text',
#           apply.coef = exp,
#           out = 'tables/target_model.tex',
#           title = 'Target of Attack',
#           covariate.labels = c('Target = Airport',
#                                'Target = Business',
#                                'Target = Educational Institution',
#                                'Target = Food or Water Supply',
#                                'Target = Government (Diplomatic)',
#                                'Target = Government (General)',
#                                'Target = Journalists',
#                                'Target = Maritime',
#                                'Target = Military',
#                                'Target = NGO',
#                                'Target = Other',
#                                'Target = Police',
#                                'Target = Religious Figure or Institution',
#                                'Target = Telecommunication',
#                                'Target = Terrorist or Non-State Militia',
#                                'Target = Tourists',
#                                'Target = Transportation',
#                                'Target = Utilities',
#                                'Target = Violent Political Party',
#                                'Target = Private Citizens & Property'),
#           omit = c('Food or Water Supply',
#                    'NGO',
#                    'Other',
#                    'Telecommunication'))
# 
# 
# 
# # Attack Type
# stargazer(glm.nb(nkill ~ attacktype1_txt, data = european_terror) , 
#           type = 'text',
#           apply.coef = exp,
#           out = 'tables/attack_model.tex',
#           title = 'Type of Attack', 
#           covariate.labels = c('Armed Assault',
#                                'Bombing or Explosion',
#                                'Facility or Infrastructure Attack',
#                                'Hijacking',
#                                'Hostage Taking (Barricade)',
#                                'Hostage Taking (Kidnapping)',
#                                'Unarmed Assault',
#                                'Assassination'))
# 
# 



















