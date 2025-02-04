## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(rio)
library(ggplot2)
library(tidyr)
library(lavaan)
library(semTools)
library(modsem)


## ----data----------------------------------------------------------------------------------------------------------
kids <- read.csv("data/projectkids.csv")


## ----distributions, message = FALSE, warning = FALSE---------------------------------------------------------------
kids %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x=value)) +
  geom_histogram() + 
  facet_wrap(vars(name), scales = "free")


## ----skew, message = FALSE, warning = FALSE------------------------------------------------------------------------
# Univariate skew and kurtosis
apply(kids, 2, skew)
apply(kids, 2, kurtosis)


## ----hpc_var-------------------------------------------------------------------------------------------------------
var(kids$hpc_mean)
# residual = (1 - .96) * 0.393
(1 - .96) * 0.393


## ----specify1------------------------------------------------------------------------------------------------------
big_cfa <- '
readprob =~ cldq_1 + cldq_2 + cldq_4 + cldq_5 + cldq_6
attention =~ swan_1 + swan_2 + swan_3 + swan_4 + swan_5 + swan_6 + 
             swan_7 + swan_8 + swan_9
             
hwp =~ 1*hpc_mean
hpc_mean ~~ 0.01572*hpc_mean
'

fit_cfa <- cfa(big_cfa, kids, 
               estimator = "mlr", 
               missing = "fiml")

summary(fit_cfa, fit.measures = T, estimates = F)


## ----resid1--------------------------------------------------------------------------------------------------------
residuals(fit_cfa, type = "cor.bollen")$cov


## ----modind1-------------------------------------------------------------------------------------------------------
modindices(fit_cfa, sort. = TRUE, minimum.value = 15)


## ----bigcfa2-------------------------------------------------------------------------------------------------------
big_cfa2 <- '
readprob =~ cldq_1 + cldq_2 + cldq_4 + cldq_5 + cldq_6
attention =~ swan_1 + swan_2 + swan_3 + swan_4 + swan_5 + swan_6 + 
             swan_7 + swan_8 + swan_9
             
hwp =~ 1* hpc_mean
hpc_mean ~~ 0.01572*hpc_mean

swan_1 ~~ swan_2
'

fit_cfa2 <- cfa(big_cfa2, kids, 
                estimator = "mlr", 
                missing = "fiml")

summary(fit_cfa2, fit.measures = T, estimates = F)


## ----comp1---------------------------------------------------------------------------------------------------------
comp_12 <- compareFit(fit_cfa, fit_cfa2)
comp_12@nested


## ----modind2-------------------------------------------------------------------------------------------------------
modindices(fit_cfa2, sort. = TRUE, minimum.value = 15)


## ----bigcfa3-------------------------------------------------------------------------------------------------------
big_cfa3 <- '
readprob =~ cldq_1 + cldq_2 + cldq_4 + cldq_5 + cldq_6
attention =~ swan_1 + swan_2 + swan_3 + swan_4 + swan_5 + swan_6 + 
             swan_7 + swan_8 + swan_9
             
hwp =~ 1* hpc_mean
hpc_mean ~~ 0.01572*hpc_mean

swan_1 ~~ swan_2
cldq_5 ~~ cldq_6
'

fit_cfa3 <- cfa(big_cfa3, kids, 
                estimator = "mlr", 
                missing = "fiml")

summary(fit_cfa3, fit.measures = T, estimates = F)


## ----comp2---------------------------------------------------------------------------------------------------------
comp_23 <- compareFit(fit_cfa3, fit_cfa2)
comp_23@nested


## ----modind3-------------------------------------------------------------------------------------------------------
modindices(fit_cfa3, sort. = TRUE, minimum.value = 15)


## ----rsquare-------------------------------------------------------------------------------------------------------
lavInspect(fit_cfa3, what = "rsquare")


## ----omega---------------------------------------------------------------------------------------------------------
compRelSEM(fit_cfa)


## ----structural----------------------------------------------------------------------------------------------------
big_sem <- '
# Measurement Model
readprob =~ cldq_1 + cldq_2 + cldq_4 + cldq_5 + cldq_6
attention =~ swan_1 + swan_2 + swan_3 + swan_4 + swan_5 + swan_6 + 
             swan_7 + swan_8 + swan_9
             
hwp =~ 1* hpc_mean
hpc_mean ~~ 0.01572*hpc_mean

swan_1 ~~ swan_2
cldq_5 ~~   cldq_6

# Structural Model
readprob ~ b*hwp
hwp ~ a*attention


# Indirect Effects
ind.att.hwp := a*b
'

fit_sem <- sem(big_sem, kids, 
               estimator = "mlr", 
               missing = "fiml")

summary(fit_sem, fit.measures = T, estimates = F)


## ----structural2---------------------------------------------------------------------------------------------------
big_sem2 <- '
# Measurement Model
readprob =~ cldq_1 + cldq_2 + cldq_4 + cldq_5 + cldq_6
attention =~ swan_1 + swan_2 + swan_3 + swan_4 + swan_5 + swan_6 + 
             swan_7 + swan_8 + swan_9

hwp =~ 1* hpc_mean
hpc_mean ~~ 0.01572*hpc_mean

swan_1 ~~ swan_2
cldq_5 ~~   cldq_6

# Structural Model
readprob ~ b*hwp + c*attention
hwp ~ a*attention


# Indirect Effects
ind.att.hwp := a*b
tot.att := c + a*b
'

fit_sem2 <- sem(big_sem2, kids, 
                estimator = "mlr", 
                missing = "fiml")

comp_34 <- compareFit(fit_sem, fit_sem2)
comp_34@nested


## ----interpret-----------------------------------------------------------------------------------------------------
fit_semb <- sem(big_sem, kids, 
                estimator = "ml", 
                missing = "fiml",
                se = "bootstrap", 
                bootstrap = 1000,
                iseed = 8789)

parameterEstimates(fit_semb, boot.ci.type = "bca.simple", 
                   ci = TRUE, se = TRUE, 
                   zstat = FALSE, pvalue = FALSE,
                   output = "text")

lavInspect(fit_semb, what = "rsquare")


## ----interact1-----------------------------------------------------------------------------------------------------
big_sem3 <- '
# Measurement Model
readprob =~ cldq_1 + cldq_2 + cldq_4 + cldq_5 + cldq_6
attention =~ swan_1 + swan_2 + swan_3 + swan_4 + swan_5 + swan_6 + 
             swan_7 + swan_8 + swan_9

hwp =~ 1* hpc_mean
hpc_mean ~~ 0.01572*hpc_mean

swan_1 ~~ swan_2
cldq_5 ~~   cldq_6

# Structural Model
hwp ~ readprob + attention + readprob:attention
'


## ----interact2-----------------------------------------------------------------------------------------------------
fit_sem_mod_pi <- modsem(model = big_sem3, 
                      data = kids, 
                      missing = "fiml",
                      method = "dblcent")

# verbose = TRUE print model estimation progress in the console
# fit_sem_mod_lms <- modsem(model = big_sem3, 
#                      data = kids, 
#                      method = "lms", verbose = TRUE)
# fit_sem_mod_qml <- modsem(model = big_sem3, 
#                      data = kids, 
#                      method = "qml", verbose = TRUE)


## ----interact3-----------------------------------------------------------------------------------------------------
summary(fit_sem_mod_pi, std = T)


## ----jn, message = FALSE, warning = FALSE--------------------------------------------------------------------------
plot_jn(x = "readprob", z = "attention",  y = "hwp", model = fit_sem_mod_pi, max_z = 4)

