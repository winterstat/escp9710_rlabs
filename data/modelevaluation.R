## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(rio)
library(lavaan)
library(semTools)
library(semhelpinghands)


## ----data----------------------------------------------------------------------------------------------------------
meece <- import(file = "data/meece.csv")


## ----specify1------------------------------------------------------------------------------------------------------
mod1 <- '
ActiveEn ~ b1*TaskMast + b2*EgoSoc

TaskMast ~ a1*IntMot
EgoSoc ~ a2*SciAtt


SciAtt ~~ IntMot

# indirect and total effect between IntMot > AciveEn
im.tm.ind := a1*b1

# indirect effects between SciAtt > AciveEn
sa.es.ind := a2*b2
'


## ----est-----------------------------------------------------------------------------------------------------------
fit1 <- sem(model = mod1, data = meece)


## ----eval----------------------------------------------------------------------------------------------------------
summary(fit1, fit.measures = T, estimates = F)


## ----evallocal-----------------------------------------------------------------------------------------------------
residuals(fit1, type = "standardized")
residuals(fit1, type = "normalized")
residuals(fit1, type = "cor.bollen")


## ----specify2------------------------------------------------------------------------------------------------------
mod2 <- '
ActiveEn ~ b1*TaskMast + b2*EgoSoc

TaskMast ~ a1*IntMot
EgoSoc ~ a2*SciAtt


TaskMast ~~ EgoSoc

SciAtt ~~ IntMot

# indirect and total effect between IntMot > AciveEn
im.tm.ind := a1*b1

# indirect effects between SciAtt > AciveEn
sa.es.ind := a2*b2
'


## ----est2----------------------------------------------------------------------------------------------------------
fit2 <- sem(model = mod2, data = meece)


## ----eval2---------------------------------------------------------------------------------------------------------
summary(fit2, fit.measures = T,  estimates = F)


## ----evallocal2----------------------------------------------------------------------------------------------------
residuals(fit2, type = "cor.bollen")


## ----net1----------------------------------------------------------------------------------------------------------
net(fit1, fit2)


## ----comp1---------------------------------------------------------------------------------------------------------
comp12 <- compareFit(fit1, fit2)
summary(comp12)


## ----specify3------------------------------------------------------------------------------------------------------
mod3 <- '
ActiveEn ~ b1*TaskMast + b2*EgoSoc

TaskMast ~ a1*IntMot + a3*SciAtt
EgoSoc ~ a2*SciAtt


TaskMast ~~ EgoSoc

SciAtt ~~ IntMot

# indirect and total effect between IntMot > AciveEn
im.tm.ind := a1*b1

# indirect effects between SciAtt > AciveEn
sa.es.ind := a2*b2
sa.tm.ind := a3*b1
sa.total.ind := (a2*b2) + (a3*b1)
'


## ----est3----------------------------------------------------------------------------------------------------------
fit3 <- sem(model = mod3, data = meece)


## ----eval3---------------------------------------------------------------------------------------------------------
summary(fit3, fit.measures = T, estimates = F)


## ----evallocal3----------------------------------------------------------------------------------------------------
residuals(fit3, type = "cor.bollen")


## ----comp3---------------------------------------------------------------------------------------------------------
comp23 <- compareFit(fit3, fit2)
summary(comp23)


## ----specify4------------------------------------------------------------------------------------------------------
mod4 <- '
ActiveEn ~ b1*TaskMast + b2*EgoSoc

TaskMast ~ a1*IntMot + a3*SciAtt
EgoSoc ~ a2*SciAtt + a4*IntMot


TaskMast ~~ EgoSoc

SciAtt ~~ IntMot

# indirect and total effect between IntMot > AciveEn
im.tm.ind := a1*b1
im.es.ind := a4*b1
im.total.ind := (a1*b1) + (a4*b1)

# indirect effects between SciAtt > AciveEn
sa.es.ind := a2*b2
sa.tm.ind := a3*b1
sa.total.ind := (a2*b2) + (a3*b1)
'


## ----est4----------------------------------------------------------------------------------------------------------
fit4 <- sem(model = mod4, data = meece)


## ----eval4---------------------------------------------------------------------------------------------------------
summary(fit4, fit.measures = T, estimates = F)


## ----evallocal4----------------------------------------------------------------------------------------------------
residuals(fit4, type = "cor.bollen")


## ----comp4---------------------------------------------------------------------------------------------------------
comp34 <- compareFit(fit3, fit4)
summary(comp34)


## ----est5----------------------------------------------------------------------------------------------------------
fit4 <- sem(model = mod4, data = meece,
            se = "bootstrap", bootstrap = 1000,
            iseed = 8789)


## ----int1----------------------------------------------------------------------------------------------------------
parameterEstimates(fit4, boot.ci.type = "bca.simple", 
                   ci = TRUE, se = TRUE, 
                   zstat = FALSE, pvalue = FALSE,
                   output = "text")


## ----rsquare-------------------------------------------------------------------------------------------------------
lavInspect(fit4, what = "rsquare")


## ----import2-------------------------------------------------------------------------------------------------------
meece2 <- import("data/meece2.csv")

# Create group variable for each dataset
meece$sample <- 1
meece2$sample <- 2

# Combine two groups into one dataframe
meece_all <- rbind(meece, meece2)


## ----specifyg1-----------------------------------------------------------------------------------------------------
mod_g1 <- '
ActiveEn ~ c(b1.g1, b1.g2)*TaskMast + c(b2.g1, b2.g2)*EgoSoc

TaskMast ~ c(a1.g1, a1.g2)*IntMot
EgoSoc ~ c(a2.g1, a2.g2)*IntMot

TaskMast ~~ EgoSoc

# Group = 1
# indirect and total effect between IntMot > AciveEn
im.tm.ind.g1 := a1.g1*b1.g1
im.es.ind.g1 := a2.g1*b2.g1
im.total.ind.g1 := (a1.g1*b1.g1) + (a2.g1*b2.g1)

# Group = 2
# indirect and total effect between IntMot > AciveEn
im.tm.ind.g2 := a1.g2*b1.g2
im.es.ind.g2 := a2.g2*b2.g2
im.total.ind.g2 := (a1.g2*b1.g2) + (a2.g2*b2.g2)
'


## ----estgr---------------------------------------------------------------------------------------------------------
fit_g1 <- sem(model = mod_g1, data = meece_all, group = "sample")


## ----evalgr--------------------------------------------------------------------------------------------------------
summary(fit_g1, fit.measures = T)


## ----evalgr2-------------------------------------------------------------------------------------------------------
group_by_groups(fit_g1)


## ----specifyg2-----------------------------------------------------------------------------------------------------
mod_g2 <- '
ActiveEn ~ c(b1, b1)*TaskMast + c(b2, b2)*EgoSoc

TaskMast ~ c(a1, a1)*IntMot
EgoSoc ~ c(a2, a2)*IntMot


TaskMast ~~ EgoSoc

# Group = 1 and 2 because no separate effects
# indirect and total effect between IntMot > AciveEn
im.tm.ind.g1 := a1*b1
im.es.ind.g1 := a2*b2
im.total.ind.g1 := (a1*b1) + (a2*b2)
'


## ----estgr2--------------------------------------------------------------------------------------------------------
fit_g2 <- sem(model = mod_g2, data = meece_all, group = "sample")


## ----evalgr3-------------------------------------------------------------------------------------------------------
group_by_groups(fit_g2)


## ----comp_g12------------------------------------------------------------------------------------------------------
comp_g12 <- compareFit(fit_g1, fit_g2)
summary(comp_g12)


## ----localgroup----------------------------------------------------------------------------------------------------
residuals(fit_g2, type = "cor")


## ----wald----------------------------------------------------------------------------------------------------------
lavTestWald(fit_g1, constraints = "b1.g1 == b1.g2")


## ----wald2---------------------------------------------------------------------------------------------------------
lavTestWald(fit_g1, constraints = "im.tm.ind.g1 == im.tm.ind.g2")


## ----mod1----------------------------------------------------------------------------------------------------------
modificationIndices(fit1, sort. = TRUE, minimum.value = 10)

