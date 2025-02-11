## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(rio)
library(lavaan)
library(semTools)
library(semhelpinghands)


## ----data----------------------------------------------------------------------------------------------------------
kabc <- import(file = "data/kabc.csv")


## ----specify1------------------------------------------------------------------------------------------------------
mod1 <- '
CogA =~ hm + nr + wo + gc + tr + sm + ma + ps
'


## ----est-----------------------------------------------------------------------------------------------------------
fit1a <- cfa(model = mod1, data = kabc)


## ----est2----------------------------------------------------------------------------------------------------------
fit1b <- cfa(model = mod1, data = kabc, 
             std.lv = TRUE)


## ----est3----------------------------------------------------------------------------------------------------------
fit1c <- cfa(model = mod1, data = kabc, 
             effect.coding = TRUE)


## ----net-----------------------------------------------------------------------------------------------------------
net(fit1a, fit1b, fit1c)


## ----compare-------------------------------------------------------------------------------------------------------
group_by_models(list("marker" = fit1a, 
                     "variance" = fit1b,
                     "effect" = fit1c))


## ----standardized--------------------------------------------------------------------------------------------------
group_by_models(list("marker" = fit1a,
                     "variance" = fit1b,
                     "effect" = fit1c),
                use_standardizedSolution = TRUE, 
                col_names = "est.std")


## ----eval----------------------------------------------------------------------------------------------------------
summary(fit1a, fit.measures = T, estimates = F)


## ----evallocal-----------------------------------------------------------------------------------------------------
residuals(fit1a, type = "normalized")
residuals(fit1a, type = "cor.bollen")


## ----specify2------------------------------------------------------------------------------------------------------
mod2 <- '
Sequent =~ hm + nr + wo
Simult =~ gc + tr + sm + ma + ps
'


## ----est4----------------------------------------------------------------------------------------------------------
fit2a <- cfa(model = mod2, data = kabc)


## ----eval2---------------------------------------------------------------------------------------------------------
summary(fit2a, fit.measures = T,  estimates = F)


## ----evallocal2----------------------------------------------------------------------------------------------------
residuals(fit2a, type = "normalized")
residuals(fit2a, type = "cor.bollen")


## ----comp1---------------------------------------------------------------------------------------------------------
comp12 <- compareFit(fit1a, fit2a)
summary(comp12)


## ----specify3------------------------------------------------------------------------------------------------------
mod3 <- '
Sequent =~ hm + nr + wo
Simult =~ gc + tr + sm + ma + ps + hm 
'


## ----est5----------------------------------------------------------------------------------------------------------
fit3a <- cfa(model = mod3, data = kabc)


## ----eval3---------------------------------------------------------------------------------------------------------
summary(fit3a, fit.measures = T, estimates = F)


## ----evallocal3----------------------------------------------------------------------------------------------------
residuals(fit3a, type = "normalized")
residuals(fit3a, type = "cor.bollen")


## ----comp3---------------------------------------------------------------------------------------------------------
comp23 <- compareFit(fit3a, fit2a)
summary(comp23)


## ----approxfit-----------------------------------------------------------------------------------------------------
comp123 <- compareFit(fit3a, fit2a, fit1a)
summary(comp123)


## ----int1----------------------------------------------------------------------------------------------------------
summary(fit3a, std = T, rsquare = T)


## ----omega---------------------------------------------------------------------------------------------------------
compRelSEM(fit3a)


## ----specify4------------------------------------------------------------------------------------------------------
mod4 <- '
Sequent =~ hm + nr + wo
Simult =~ gc + tr + sm + ma + ps 

nr ~~ wo
'


## ----est6----------------------------------------------------------------------------------------------------------
fit4a <- cfa(model = mod4, data = kabc)


## ----eval4---------------------------------------------------------------------------------------------------------
summary(fit4a, fit.measures = T, estimates = F)


## ----evallocal4----------------------------------------------------------------------------------------------------
residuals(fit4a, type = "normalized")
residuals(fit4a, type = "cor.bollen")


## ----comp4---------------------------------------------------------------------------------------------------------
comp24 <- compareFit(fit4a, fit2a)
summary(comp24)


## ----int2----------------------------------------------------------------------------------------------------------
summary(fit4a, std = T, rsquare = T)


## ----omega2--------------------------------------------------------------------------------------------------------
compRelSEM(fit4a)

