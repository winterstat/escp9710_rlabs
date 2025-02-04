## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(rio)
library(lavaan)
library(semTools)
library(ggplot2)
library(GGally)


## ----data----------------------------------------------------------------------------------------------------------
meece <- import(file = "data/meece.csv")


## ----distributions, message = FALSE, warning = FALSE---------------------------------------------------------------
ggpairs(meece, progress = FALSE, diag = list(continuous = "barDiag"))


## ----skew----------------------------------------------------------------------------------------------------------
# Univariate skew and kurtosis
apply(meece, 2, skew)
apply(meece, 2, kurtosis)

# Multivariate skew and kurtosis
mardiaSkew(meece)
mardiaKurtosis(meece)


## ----specify2------------------------------------------------------------------------------------------------------
mod1 <- '
ActiveEn ~ b1*TaskMast + b2*EgoSoc + c1*IntMot

TaskMast ~ a1*SciAtt + a2*IntMot
EgoSoc ~ a3*SciAtt + a4*IntMot + b3*TaskMast


SciAtt ~~ IntMot

# indirect and total effect between IntMot > AciveEn
im.tm.ind := a2*b1
im.tm.es.ind := a2*b3*b2
im.es.ind := a4*b2
im.total.ind := (a2*b1) + (a2*b3*b2) + (a4*b2) 
im.total := (a2*b1) + (a2*b3*b2) + (a4*b2) + c1

# indirect effects between SciAtt > AciveEn
sa.tm.ind := a1*b1
sa.tm.es.ind := a1*b3*b2
sa.es.ind := a3*b2
sa.total.ind := (a1*b1) + (a1*b3*b2) + (a3*b2)
'


## ----est-----------------------------------------------------------------------------------------------------------
fit1 <- sem(model = mod1, data = meece,
            se = "bootstrap", bootstrap = 100,
            iseed = 8789)


## ----interpret, message = FALSE, warning = FALSE-------------------------------------------------------------------
parameterEstimates(fit1, boot.ci.type = "bca.simple", 
                   ci = TRUE, se = TRUE, 
                   zstat = FALSE, pvalue = FALSE,
                   output = "text")


## ----helpload, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(semhelpinghands)


## ----helpuse, message = FALSE, warning = FALSE---------------------------------------------------------------------
ci_boot <- standardizedSolution_boot_ci(fit1)

print(ci_boot,
      output = "text")


## ----modmedspec, message = FALSE, warning = FALSE------------------------------------------------------------------
library(modsem)

mod2 <- '
SciAtt ~ b*ActiveEn + c*EgoSoc

ActiveEn ~ a1*EgoSoc + a2*IntMot + a3*EgoSoc:IntMot

# index of moderated mediation
ind.mod.im := a3*b
'

fit2 <- modsem(model = mod2, data = meece, method = "pind",
               se = "bootstrap", bootstrap = 100,
               iseed = 8789)


## ----modmedparam, message = FALSE, warning = FALSE-----------------------------------------------------------------
fit2_lav <- extract_lavaan(fit2)

parameterEstimates(fit2_lav, boot.ci.type = "bca.simple", 
                   ci = TRUE, se = TRUE, 
                   zstat = FALSE, pvalue = FALSE,
                   output = "text")


## ----simpleslopes1, message = FALSE, warning = FALSE---------------------------------------------------------------
mean(meece$IntMot)
mean(meece$IntMot) - sd(meece$IntMot)
mean(meece$IntMot) + sd(meece$IntMot)


## ----simpleslopes2, message = FALSE, warning = FALSE---------------------------------------------------------------
mod3 <- '
SciAtt ~ b*ActiveEn + c*EgoSoc

ActiveEn ~ a1*EgoSoc + a2*IntMot + a3*EgoSoc:IntMot

# index of moderated mediation
ind.mod.im := a3*b

# simple slopes
im.low := a1 + a3*2.31
im.mean := a1 + a3*2.85
im.high := a1 + a3*3.39

# conditional indirect effect
a1.b.low := im.low * b
a1.b.mean := im.mean * b
a1.b.high := im.high * b
'

fit3 <- modsem(model = mod3, data = meece, method = "pind",
               se = "bootstrap", bootstrap = 100,
            iseed = 8789)



## ----simpleslopes3, message = FALSE, warning = FALSE---------------------------------------------------------------
fit3_lav <- extract_lavaan(fit3)

parameterEstimates(fit3_lav, boot.ci.type = "bca.simple", 
                   ci = TRUE, se = TRUE, 
                   zstat = FALSE, pvalue = FALSE,
                   output = "text")

