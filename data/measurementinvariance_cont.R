## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(rio)
library(lavaan)
library(semTools)
library(semhelpinghands)
library(tidyverse)


## ----data----------------------------------------------------------------------------------------------------------
DASS21 <- import(file = "data/DASS21.csv")

DASS21$engnat <- factor(DASS21$engnat, 
                        levels = c("EnglishNative", "ELL"))


## ----group---------------------------------------------------------------------------------------------------------
table(DASS21$engnat)


## ----specify1------------------------------------------------------------------------------------------------------
cfa_config <- '
anxiety =~ q2 + q4 + q7 + q9 + q15 + q19 + q20
stress =~ q1 + q6 + q8 + q11 + q12 + q14 + q18

q4 ~~ q19
q4 ~~ q7
'


## ----config--------------------------------------------------------------------------------------------------------
fit.config <- measEq.syntax(configural.model = cfa_config, 
                            data = DASS21, 
                            group = "engnat", 
                            ID.fac = "effects",
                            meanstructure = TRUE, 
                            return.fit = TRUE,
                            estimator = "mlr")

# Print out the model syntax, so you can
# see what semTools is helping us do:
cat(as.character(fit.config@call$model))


## ----global1-------------------------------------------------------------------------------------------------------
summary(fit.config, fit.measures = T, estimates = F)


## ----local1--------------------------------------------------------------------------------------------------------
residuals(fit.config, type = "cor.bollen")


## ----weakpar-------------------------------------------------------------------------------------------------------
group_by_groups(fit.config)


## ----weakfit-------------------------------------------------------------------------------------------------------
fit.weak <- measEq.syntax(configural.model = cfa_config, 
                          data = DASS21, 
                          group = "engnat", 
                          ID.fac = "effects", 
                          meanstructure = TRUE, 
                          return.fit = TRUE,
                          estimator = "mlr",
                          group.equal = "loadings")

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.weak@call$model))


## ----weakest-------------------------------------------------------------------------------------------------------
comp_12 <- compareFit(fit.config, fit.weak)
summary(comp_12)


## ----weakpar2------------------------------------------------------------------------------------------------------
group_by_groups(fit.weak)


## ----strong--------------------------------------------------------------------------------------------------------
fit.strong <- measEq.syntax(configural.model = cfa_config, 
                            data = DASS21, 
                            group = "engnat", 
                            ID.fac = "effects", 
                            meanstructure = TRUE, 
                            return.fit = TRUE,
                            estimator = "mlr",
                            group.equal = c("loadings", "intercepts"))

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.strong@call$model))


## ----strongest-----------------------------------------------------------------------------------------------------
comp_23 <- compareFit(fit.weak, fit.strong)
summary(comp_23)


## ----resid2--------------------------------------------------------------------------------------------------------
residuals(fit.strong, type = "cor.bollen")$EnglishNative$mean
residuals(fit.strong, type = "cor.bollen")$ELL$mean


## ----modind, message = FALSE, warning = FALSE----------------------------------------------------------------------
lavTestScore(fit.strong)$uni[5:32,]


## ----modind2-------------------------------------------------------------------------------------------------------
lavTestScore(fit.strong, epc = TRUE, standardized = FALSE)$epc %>% 
  filter(str_detect(label, "nu."))


## ----partial1------------------------------------------------------------------------------------------------------
fit.partial1 <- measEq.syntax(configural.model = cfa_config, 
                              data = DASS21, 
                              group = "engnat", 
                              ID.fac = "effects", 
                              meanstructure = TRUE, 
                              return.fit = TRUE,
                              estimator = "mlr",
                              group.equal = c("loadings", "intercepts"),
                              group.partial = "q18 ~ 1")

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.partial1@call$model))


## ----comp3---------------------------------------------------------------------------------------------------------
comp_23b <- compareFit(fit.weak, fit.partial1)
summary(comp_23b)


## ----resid3--------------------------------------------------------------------------------------------------------
residuals(fit.partial1, type = "cor.bollen")$EnglishNative$mean
residuals(fit.partial1, type = "cor.bollen")$ELL$mean


## ----modind3-------------------------------------------------------------------------------------------------------
lavTestScore(fit.partial1)$uni[5:31,]


## ----modind4-------------------------------------------------------------------------------------------------------
lavTestScore(fit.partial1, epc = TRUE, standardized = FALSE)$epc %>% 
  filter(str_detect(label, "nu."))


## ----partial2------------------------------------------------------------------------------------------------------
fit.partial2 <- measEq.syntax(configural.model = cfa_config, 
                              data = DASS21, 
                              group = "engnat", 
                              ID.fac = "effects", 
                              meanstructure = TRUE, 
                              return.fit = TRUE,
                              estimator = "mlr",
                              group.equal = c("loadings", "intercepts"),
                              group.partial = c("q18~1", "q2~1"))

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.partial2@call$model))


## ----comp4---------------------------------------------------------------------------------------------------------
comp_23c <- compareFit(fit.weak, fit.partial2)
summary(comp_23c)


## ----resid5--------------------------------------------------------------------------------------------------------
residuals(fit.partial2, type = "cor.bollen")$EnglishNative$mean
residuals(fit.partial2, type = "cor.bollen")$ELL$mean


## ----diffcfirmsea--------------------------------------------------------------------------------------------------
# Change in CFI
cat(paste0("Change in CFI: ", comp_23c@fit.diff$cfi.robust))

# Change in RMSEA
cat(paste0("Change in RMSEA: ", comp_23c@fit.diff$rmsea.robust))


## ----strong_fv-----------------------------------------------------------------------------------------------------
fit.partial2.lv <- measEq.syntax(configural.model = cfa_config, 
                              data = DASS21, 
                              group = "engnat", 
                              ID.fac = "effects", 
                              meanstructure = TRUE, 
                              return.fit = TRUE,
                              estimator = "mlr",
                              group.equal = c("loadings", "intercepts", 
                                          "lv.variances"),
                              group.partial = c("q18~1", "q2~1"))

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.partial2.lv@call$model))


## ----comp10--------------------------------------------------------------------------------------------------------
comp_3c4 <- compareFit(fit.partial2, fit.partial2.lv)
summary(comp_3c4)


## ----strong_fcv----------------------------------------------------------------------------------------------------
fit.partial2.lcv <- measEq.syntax(configural.model = cfa_config, 
                              data = DASS21, 
                              group = "engnat", 
                              ID.fac = "effects", 
                              meanstructure = TRUE, 
                              return.fit = TRUE,
                              estimator = "mlr",
                              group.equal = c("loadings", "intercepts", 
                                          "lv.variances",
                                          "lv.covariances"),
                              group.partial = c("q18~1", "q2~1"))

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.partial2.lcv@call$model))


## ----comp11--------------------------------------------------------------------------------------------------------
comp_45 <- compareFit(fit.partial2.lv, fit.partial2.lcv)
summary(comp_45)


## ----compmmean1----------------------------------------------------------------------------------------------------
group_by_groups(fit.partial2.lcv) %>% 
  filter((op == "~1" & (lhs == "stress" | lhs == "anxiety")) | 
           op == "~~" & (lhs == "stress" | lhs == "anxiety"))


## ----companxiety---------------------------------------------------------------------------------------------------
# mean in English Native Speaker: 2.269
# mean in ELL Speaker: 2.088
# variance: 0.527

(2.269 - 2.088) / sqrt(0.527)


## ----compstress----------------------------------------------------------------------------------------------------
# mean in English Native Speaker: 2.506
# mean in ELL Speaker: 2.261
# variance: 0.535

(2.506 - 2.261) / sqrt(0.535)


## ----wald----------------------------------------------------------------------------------------------------------
# H0: Average anxiety is equivalent across 
# English Native and ELL speakers
lavTestWald(fit.partial2.lcv, constraints = "alpha.1.g1 == alpha.1.g2")

# H0: Average stress is equivalent across 
# English Native and ELL speakers
lavTestWald(fit.partial2.lcv, constraints = "alpha.2.g1 == alpha.2.g2")


## ----bonus---------------------------------------------------------------------------------------------------------
# Create mean score variables:
DASS21 <- DASS21 %>%
  rowwise() %>%
  mutate(anx_mean = mean(c(q2, q4, q7, q9, q15, q19, q20)),
         str_mean = mean(c(q1, q6, q8, q11, q12, q14, q18)))

# Test significance of mean difference (both significant)
t.test(anx_mean ~ engnat, data = DASS21)
t.test(str_mean ~ engnat, data = DASS21)

# Get cohen's D 
# (both under-estimated compared to latent mean comparison):
psych::cohen.d(DASS21$anx_mean, group = DASS21$engnat)
psych::cohen.d(DASS21$str_mean, group = DASS21$engnat)

