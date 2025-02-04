## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(rio)
library(lavaan)
library(ggplot2)
library(semPlot)
library(modsem)


## ----data----------------------------------------------------------------------------------------------------------
meece <- import(file = "data/meece.csv")


## ----wdcheck, eval = FALSE-----------------------------------------------------------------------------------------
## getwd()


## ----wdchange, eval = F--------------------------------------------------------------------------------------------
## # Mac OS:
## setwd("~/Dropbox/Work/Teaching/Measurement/R Labs")
## 
## # Windows:
## setwd("C:/Users/sonja/Dropbox/Work/Teaching/Measurement/R Labs")
## 
## # Note: the folder that you are using for this class will very
## # likely be in a different location.


## ----specify-------------------------------------------------------------------------------------------------------
mod1 <- '
ActiveEn ~ TaskMast 
ActiveEn ~ EgoSoc 
ActiveEn ~ IntMot

EgoSoc ~ TaskMast 
EgoSoc ~ SciAtt 
EgoSoc ~ IntMot

TaskMast ~ SciAtt 
TaskMast ~ IntMot

SciAtt ~~ IntMot'


## ----specify2------------------------------------------------------------------------------------------------------
mod1 <- '
ActiveEn ~ TaskMast + EgoSoc + IntMot

EgoSoc ~ TaskMast + SciAtt + IntMot
TaskMast ~ SciAtt + IntMot

SciAtt ~~ IntMot'


## ----known---------------------------------------------------------------------------------------------------------
(5 *(5 + 1)) / 2


## ----inv-----------------------------------------------------------------------------------------------------------
# Correlation matrix to get first look at magnitude of associations
cor(meece)

# Find the determinant of the covariance matrix
det(cov(meece))

# Find the inverse of the covariance matrix
solve(cov(meece))


## ----est-----------------------------------------------------------------------------------------------------------
fit1 <- sem(model = mod1, data = meece)


## ----diagram-------------------------------------------------------------------------------------------------------
semPaths(fit1, rotation = 2)


## ----eval0---------------------------------------------------------------------------------------------------------
# The observed covariance matrix
obs_cov <- round(cov(meece[,c(5,4,3,1,2)]), digits = 3)

# The model-implied covariance matrix
imp_cov <- lavInspect(fit1, what = "implied")[[1]]

obs_cov
imp_cov

# The difference between observed and implied gives an indication 
# of what parts of the data are well-represented in the model
obs_cov - imp_cov


## ----eval1---------------------------------------------------------------------------------------------------------
# The observed correlation matrix
obs_cor <- round(cor(meece[,c(5,4,3,1,2)]), digits = 3)

# The model-implied correlation matrix
imp_cor <- lavInspect(fit1, what = "cor.ov")

obs_cor
imp_cor

# The difference between observed and implied gives an indication of 
# what parts of the data are well-represented in the model
obs_cor - imp_cor


## ----interpret-----------------------------------------------------------------------------------------------------
summary(fit1, std = TRUE)


## ----std-----------------------------------------------------------------------------------------------------------
standardizedSolution(fit1, type = "std.all")


## ----mod2, eval = FALSE--------------------------------------------------------------------------------------------
## mod2 <- '
## 
## '
## 
## fit2 <- sem(model = mod2, data = meece)
## 
## summary(fit2, std = TRUE)


## ----lm------------------------------------------------------------------------------------------------------------
fit_lm <- lm(ActiveEn ~ EgoSoc + IntMot + EgoSoc*IntMot, 
             data = meece)
summary(fit_lm)


## ----intlav, eval = FALSE------------------------------------------------------------------------------------------
## mod3 <- '
## ActiveEn ~ EgoSoc + IntMot + EgoSoc*IntMot
## '
## 
## fit3 <- sem(model = mod3, data = meece)
## summary(fit3)


## ----intlav2-------------------------------------------------------------------------------------------------------
mod3 <- '
ActiveEn ~ EgoSoc + IntMot + EgoSoc:IntMot
'

fit3 <- modsem(model = mod3, data = meece, method = "pind")
summary(fit3, std = TRUE)


## ----simpleslope---------------------------------------------------------------------------------------------------
# Set up the values at which we want to probe the impact of 
# Task Mastery Goals:
# First we compute the value that is one SD below the mean, then 
# the mean, and finally the value that is one SD above the mean
simple_slope_values <- c(mean(meece$IntMot) - sd(meece$IntMot),
                   mean(meece$IntMot),
                   mean(meece$IntMot) + sd(meece$IntMot))

# Round to 2 decimals for readability
simple_slope_values <- round(simple_slope_values, 2)

plot_interaction("EgoSoc", "IntMot", "ActiveEn", "EgoSocIntMot", vals_z = simple_slope_values, 
                 model = fit3)


## ----jn------------------------------------------------------------------------------------------------------------
plot_jn(x = "EgoSoc", z = "IntMot", y = "ActiveEn", model = fit3, max_z = 6)

