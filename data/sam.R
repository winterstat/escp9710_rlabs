## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(rio)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lavaan)
library(semTools)
library(semhelpinghands)


## ----data----------------------------------------------------------------------------------------------------------
kids <- read.csv("data/projectkids.csv")

set.seed(9710)
kids100 <- kids %>% drop_na() %>% sample_n(100)


## ----equi1---------------------------------------------------------------------------------------------------------
mod0 <- '
chaos =~ chaos2 + chaos3 + chaos6
'

mod1 <- '
chaos =~ chaos2 + chaos3 + chaos6 + hpc_mean'

mod2 <- '
chaos =~ chaos2 + chaos3 + chaos6

hpc_mean ~ chaos
'

fit0 <- sem(mod0, kids100)
fit1 <- sem(mod1, kids100)
fit2 <- sem(mod2, kids100)


## ----equi2---------------------------------------------------------------------------------------------------------
comp12 <- compareFit(fit1, fit2)
comp12@nested


## ----equi3---------------------------------------------------------------------------------------------------------
net(fit1, fit2)


## ----compest1------------------------------------------------------------------------------------------------------
group_by_models(list("cfa" = fit1, "sem" = fit2)) %>%
  filter_by(op = c("=~", "~"))


## ----change1-------------------------------------------------------------------------------------------------------
mod3 <- '
chaos =~ chaos2 + chaos3 + chaos6
readprob =~ cldq_1 + cldq_2 + cldq_4 + cldq_5 + cldq_6

readprob ~ chaos
'

fit3 <- sem(mod3, kids100)

group_by_models(list( "outcome_hpc" = fit2, "outcome_read" = fit3)) %>% 
  filter_by(op = c("=~","~"), lhs ="chaos")


## ----change2-------------------------------------------------------------------------------------------------------
group_by_models(list( "sem_hpc" = fit2, "sem_read" = fit3),
                use_standardizedSolution=TRUE) %>% 
  filter_by(op = c("=~","~"), lhs ="chaos")


## ----change3-------------------------------------------------------------------------------------------------------
fit3_sam <- sam(mod3, kids100, sam.method = "local")

group_by_models(list("cfa" = fit0, "sem_read" = fit3,"sam_read"=fit3_sam),
                                 use_standardizedSolution=TRUE) %>% 
  filter_by(op = c("=~","~"))


## ----bigcfa--------------------------------------------------------------------------------------------------------
cfa_read <- '
readprob =~ cldq_1 + cldq_2 + cldq_4 + cldq_5 + cldq_6
cldq_5 ~~ cldq_6
'

cfa_attention <- '
attention =~ swan_1 + swan_2 + swan_3 + swan_4 + swan_5 + swan_6 + 
             swan_7 + swan_8 + swan_9
swan_1 ~~ swan_2
'

cfa_chaos <- '
chaos =~ chaos2 + chaos3 + chaos6
'

cfa_math <- '
mathprob =~ cldq_18 + cldq_19 + cldq_20
'

fit_cfa_read <- cfa(cfa_read, kids100)
fit_cfa_math <- cfa(cfa_math, kids100)
fit_cfa_chaos <- cfa(cfa_chaos, kids100)
fit_cfa_attention <- cfa(cfa_attention, kids100)


## ----structural----------------------------------------------------------------------------------------------------
big_sem <- '
# Measurement Model
readprob =~ cldq_1 + cldq_2 + cldq_4 + cldq_5 + cldq_6
attention =~ swan_1 + swan_2 + swan_3 + swan_4 + swan_5 + swan_6 + 
             swan_7 + swan_8 + swan_9
             
chaos =~ chaos2 + chaos3 + chaos6
mathprob =~ cldq_18 + cldq_19 + cldq_20
             
hwp =~ 1* hpc_mean
hpc_mean ~~ 0.01572*hpc_mean

swan_1 ~~ swan_2
cldq_5 ~~   cldq_6

# Structural Model
attention ~ a1*chaos
hwp ~ a2*chaos + b1*attention

readprob ~ b2*hwp
mathprob ~ b3*hwp

readprob ~~ mathprob

# Indirect Effects
cha.att.hwp.read := a1*b1*b2
cha.att.hwp.math := a1*b1*b3
cha.hwp.read := a2*b2
cha.hwp.math := a2*b3
'

# Estimate SEM and SAM
fit4_sem <- sem(big_sem, kids100)
fit4_sam <- sam(big_sem, kids100)


## ----est_comp------------------------------------------------------------------------------------------------------
group_by_models(list( "cfa_read" = fit_cfa_read,
                      "sem" = fit4_sem),
                use_standardizedSolution=TRUE) %>% 
  filter_by(op = c("=~"), lhs = "readprob")

group_by_models(list("cfa_math" = fit_cfa_math,
                     "sem" = fit4_sem),
                use_standardizedSolution=TRUE) %>% 
  filter_by(op = c("=~"), lhs = "mathprob")

group_by_models(list("cfa_attention" = fit_cfa_attention,
                     "sem" = fit4_sem),
                use_standardizedSolution=TRUE) %>% 
  filter_by(op = c("=~"), lhs = "attention")

group_by_models(list("cfa_chaos" = fit_cfa_chaos,
                     "sem" = fit4_sem),
                use_standardizedSolution=TRUE) %>% 
  filter_by(op = c("=~"), lhs = "chaos")


## ----sam1----------------------------------------------------------------------------------------------------------
group_by_models(list("sem" = fit4_sem, 
                     "sam" = fit4_sam), 
                col_names = c("est", "pvalue")) %>% 
  filter_by(op = c("~", ":="))


## ----big_sam_summary-----------------------------------------------------------------------------------------------
summary(fit4_sam, remove.step1 = FALSE)


## ----fullsample----------------------------------------------------------------------------------------------------
fit5_sem <- sem(big_sem, kids)
fit5_sam <- sam(big_sem, kids)

group_by_models(list("sem" = fit5_sem, 
                     "sam" = fit5_sam), 
                col_names = c("est", "pvalue")) %>% 
  filter_by(op = c("~", ":="))


## ----interact1-----------------------------------------------------------------------------------------------------
big_sem_int <- '
# Measurement Model
readprob =~ cldq_1 + cldq_2 + cldq_4 + cldq_5 + cldq_6
attention =~ swan_1 + swan_2 + swan_3 + swan_4 + swan_5 + swan_6 + 
             swan_7 + swan_8 + swan_9

mathprob =~ cldq_18 + cldq_19 + cldq_20

# Structural Model
mathprob ~ readprob + attention + readprob:attention
'

fit_int_sam <- sam(big_sem_int, data = kids, se="none")

summary(fit_int_sam)

