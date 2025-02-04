## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(rio)
library(lavaan)
library(semTools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(semhelpinghands)
library(ggdist)


## ----data----------------------------------------------------------------------------------------------------------
DASS21 <- import(file = "data/DASS21.csv")

DASS21$engnat <- factor(DASS21$engnat, 
                        levels = c("EnglishNative", "ELL"))


## ----vis-----------------------------------------------------------------------------------------------------------
DASS21 %>%
  select(q2, q4, q7, q9, q15, q19, q20) %>%
  pivot_longer(cols = q2:q20) %>%
  ggplot(aes(x = value)) + 
  geom_bar() + 
  facet_wrap(vars(name)) +
  ggtitle("Anxiety Subscale")

DASS21 %>%
  select(q1, q6, q8, q11, q12, q14, q18) %>%
  pivot_longer(cols = q1:q18) %>%
  ggplot(aes(x = value)) + 
  geom_bar() + 
  facet_wrap(vars(name)) +
  ggtitle("Stress Subscale")


## ----specify1------------------------------------------------------------------------------------------------------
cfa_config <- '
anxiety =~ q2 + q4 + q7 + q9 + q15 + q19 + q20
stress =~ q1 + q6 + q8 + q11 + q12 + q14 + q18

q4 ~~ q19
q4 ~~ q7
'


## ----config--------------------------------------------------------------------------------------------------------
fit.config <- measEq.syntax(configural.model = cfa_config, data = DASS21, 
                            group = "engnat", ordered = T,
                            ID.fac = "std.lv", 
                            ID.cat = "Wu.Estabrook.2016",
                            parameterization = "delta",
                            meanstructure = TRUE, 
                            return.fit = TRUE,
                            estimator = "wlsmv")

# Print out the model syntax, so you can
# see what semTools is helping us do:
cat(as.character(fit.config@call$model))


## ----global1-------------------------------------------------------------------------------------------------------
fitMeasures(fit.config, 
            fit.measures = c("chisq.scaled", "df.scaled", 
                             "pvalue.scaled", "cfi.scaled", 
                             "rmsea.scaled", "rmsea.ci.lower.scaled", 
                             "rmsea.ci.upper.scaled", "srmr"))


## ----local1--------------------------------------------------------------------------------------------------------
residuals(fit.config, type = "cor.bollen")$EnglishNative$cov
residuals(fit.config, type = "cor.bollen")$ELL$cov


## ----weakpar-------------------------------------------------------------------------------------------------------
group_by_groups(fit.config)


## ----threshfit-----------------------------------------------------------------------------------------------------
fit.thresh <- measEq.syntax(configural.model = cfa_config, data = DASS21, 
                            group = "engnat", ordered = T,
                            ID.fac = "std.lv", 
                            ID.cat = "Wu.Estabrook.2016",
                            parameterization = "delta",
                            meanstructure = TRUE, 
                            return.fit = TRUE,
                            estimator = "wlsmv",
                            group.equal = "thresholds")

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.thresh@call$model))


## ----threshest-----------------------------------------------------------------------------------------------------
comp_12 <- compareFit(fit.config, fit.thresh)
comp_12@nested


## ----threshpar-----------------------------------------------------------------------------------------------------
# Look at threshold parameter estimates across groups by using filter 
# to include only results with the threshold operator "|"
group_by_groups(fit.thresh) %>% filter(op == "|")


## ----weakfit-------------------------------------------------------------------------------------------------------
fit.weak <- measEq.syntax(configural.model = cfa_config, data = DASS21, 
                            group = "engnat", ordered = T,
                            ID.fac = "std.lv", 
                            ID.cat = "Wu.Estabrook.2016",
                            parameterization = "delta",
                            meanstructure = TRUE, 
                            return.fit = TRUE,
                            estimator = "wlsmv",
                            group.equal = c("thresholds","loadings"))

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.weak@call$model))


## ----weeakest------------------------------------------------------------------------------------------------------
comp_23 <- compareFit(fit.thresh, fit.weak)
comp_23@nested


## ----strong--------------------------------------------------------------------------------------------------------
fit.strong <- measEq.syntax(configural.model = cfa_config, data = DASS21, 
                            group = "engnat", ordered = T,
                            ID.fac = "std.lv", 
                            ID.cat = "Wu.Estabrook.2016",
                            parameterization = "delta",
                            meanstructure = TRUE, 
                            return.fit = TRUE,
                            estimator = "wlsmv",
                            group.equal = c("thresholds", 
                                            "loadings", 
                                            "intercepts"))

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.strong@call$model))


## ----strongest-----------------------------------------------------------------------------------------------------
comp_34 <- compareFit(fit.weak, fit.strong)
comp_34@nested


## ----resid2--------------------------------------------------------------------------------------------------------
residuals(fit.strong, type = "cor.bollen")$EnglishNative$mean

residuals(fit.strong, type = "cor.bollen")$ELL$mean


## ----resid2b-------------------------------------------------------------------------------------------------------
residuals(fit.weak, type = "cor.bollen")$ELL$mean - residuals(fit.strong, type = "cor.bollen")$ELL$mean


## ----mi1-----------------------------------------------------------------------------------------------------------
modificationIndices(fit.strong, sort. = TRUE, op = "~1", 
                    minimum.value = 15)


## ----partial1------------------------------------------------------------------------------------------------------
fit.partial1 <- measEq.syntax(configural.model = cfa_config, data = DASS21, 
                            group = "engnat", ordered = T,
                            ID.fac = "std.lv", 
                            ID.cat = "Wu.Estabrook.2016",
                            parameterization = "delta",
                            meanstructure = TRUE, 
                            return.fit = TRUE,
                            estimator = "wlsmv",
                            group.equal = c("thresholds", 
                                            "loadings", 
                                            "intercepts"),
                            group.partial = c("q2 ~ 1"))

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.partial1@call$model))


## ----comp3---------------------------------------------------------------------------------------------------------
comp_34b <- compareFit(fit.weak, fit.partial1)
comp_34b@nested


## ----mi2-----------------------------------------------------------------------------------------------------------
modificationIndices(fit.partial1, sort. = TRUE, op = "~1", 
                    minimum.value = 15)


## ----partial2------------------------------------------------------------------------------------------------------
fit.partial2 <- measEq.syntax(configural.model = cfa_config, data = DASS21, 
                            group = "engnat", ordered = T,
                            ID.fac = "std.lv", 
                            ID.cat = "Wu.Estabrook.2016",
                            parameterization = "delta",
                            meanstructure = TRUE, 
                            return.fit = TRUE,
                            estimator = "wlsmv",
                            group.equal = c("thresholds", 
                                            "loadings", 
                                            "intercepts"),
                            group.partial = c("q2 ~ 1", "q18 ~ 1"))

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.partial2@call$model))


## ----comp4---------------------------------------------------------------------------------------------------------
comp_34c <- compareFit(fit.weak, fit.partial2)
print(comp_34c@nested)


## ----mi3-----------------------------------------------------------------------------------------------------------
modificationIndices(fit.partial2, sort. = TRUE, op = "~1", 
                    minimum.value = 10)


## ----partial3------------------------------------------------------------------------------------------------------
fit.partial3 <- measEq.syntax(configural.model = cfa_config, data = DASS21, 
                            group = "engnat", ordered = T,
                            ID.fac = "std.lv", 
                            ID.cat = "Wu.Estabrook.2016",
                            parameterization = "delta",
                            meanstructure = TRUE, 
                            return.fit = TRUE,
                            estimator = "wlsmv",
                            group.equal = c("thresholds", 
                                            "loadings", 
                                            "intercepts"),
                            group.partial = c("q2 ~ 1", "q18 ~ 1", "q14 ~ 1"))

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.partial3@call$model))


## ----comp5---------------------------------------------------------------------------------------------------------
comp_34d <- compareFit(fit.weak, fit.partial3)
print(comp_34d@nested)


## ----mi4-----------------------------------------------------------------------------------------------------------
modificationIndices(fit.partial3, sort. = TRUE, op = "~1", 
                    minimum.value = 5)


## ----partial4------------------------------------------------------------------------------------------------------
fit.partial4 <-measEq.syntax(configural.model = cfa_config, data = DASS21, 
                            group = "engnat", ordered = T,
                            ID.fac = "std.lv", 
                            ID.cat = "Wu.Estabrook.2016",
                            parameterization = "delta",
                            meanstructure = TRUE, 
                            return.fit = TRUE,
                            estimator = "wlsmv",
                            group.equal = c("thresholds", 
                                            "loadings", 
                                            "intercepts"),
                            group.partial = c("q2 ~ 1", "q18 ~ 1", 
                                              "q14 ~ 1", "q15 ~ 1"))

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.partial4@call$model))


## ----comp6---------------------------------------------------------------------------------------------------------
comp_34e <- compareFit(fit.weak, fit.partial4)
print(comp_34e@nested)


## ----resid4--------------------------------------------------------------------------------------------------------

residuals(fit.weak, type = "cor.bollen")$ELL$mean - residuals(fit.partial4, type = "cor.bollen")$ELL$mean


## ----diffcfirmsea--------------------------------------------------------------------------------------------------
# Change in CFI
cat(paste0("Change in CFI: ", round(comp_34e@fit.diff$cfi.scaled, 3)))

# Change in RMSEA
cat(paste0("Change in RMSEA: ", round(comp_34e@fit.diff$rmsea.scaled, 3)))


## ----vis2----------------------------------------------------------------------------------------------------------
# Get item 18 intercept value for ELL group
ell_int2 <- parameterEstimates(fit.partial4) %>% 
  filter(op == "~1" & lhs == "q18" & group == 2) %>% 
  select(est) %>% as.numeric()

# Get item 18 thresholds (invariant across groups)
ell_thresh2 <- parameterEstimates(fit.partial4) %>% 
  filter(op == "|" & lhs == "q18" & group == 2) %>% 
  select(est) %>% unlist()

# Set up the latent response variable normal distribution 
# for each group
tribble(
  ~ dist, ~args,~group,~int,
  "norm", list(0, 1),"1. English",0,
  "norm", list(ell_int2,1),"2. ELL",ell_int2
) %>% ggplot() +
  # Change the fill color of the distribution at each threshold 
  # value by cutting up the distribution
  ggdist::stat_slab(aes(xdist = dist, args = args, 
                        fill = after_stat(cut(x, c(-Inf, ell_thresh2, Inf)))), 
                    show.legend = FALSE) + 
  # Colors of the different sections of the latent response variable
  scale_fill_manual(values = c("#494B69","#9F5B72", 
                               "#D8707C","#FD9B41")) +
  # Add vertical lines at each threshold value
  geom_vline(xintercept = ell_thresh2[1]) +
  geom_vline(xintercept = ell_thresh2[2]) +
  geom_vline(xintercept = ell_thresh2[3]) +
  # Add a dashed vertical line at the latent response variable 
  # intercept value
  geom_vline(aes(xintercept = int), linetype = "dashed") +
  # Plot both groups separately
  facet_grid(rows = vars(group)) +
  # Add axis labels
  labs(x = "Underlying Latent Response Variable", y = "Density") +
  # Change plot theme/some formatting
  theme_classic() +
  theme(text = element_text(size = 20),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = .5))


## ----strong_fv-----------------------------------------------------------------------------------------------------
fit.partial4.lv <- measEq.syntax(cfa_config, data = DASS21, 
                            ID.fac = "std.lv", 
                            ID.cat = "Wu.Estabrook.2016",
                            group = "engnat", 
                            group.equal = c("thresholds", "loadings", 
                                            "intercepts", "lv.variances"),
                            group.partial = c("q2 ~ 1", "q18 ~ 1", 
                                              "q14 ~ 1", "q15 ~ 1"),
                            ordered = T,
                            parameterization = "delta",
                            meanstructure = TRUE, return.fit = TRUE,
                            estimator = "wlsmv")

# Print out the model syntax, so you can
# see what semTools is helping us do:
# cat(as.character(fit.partial4.lv@call$model))


## ----comp10--------------------------------------------------------------------------------------------------------
comp_4e5 <- compareFit(fit.partial4, fit.partial4.lv)
comp_4e5@nested


## ----compmmean1----------------------------------------------------------------------------------------------------
group_by_groups(fit.partial4) %>% 
  filter((op == "~1" & (lhs == "stress" | lhs == "anxiety")) | 
           op == "~~" & (lhs == "stress" | lhs == "anxiety"))


## ----companxiety---------------------------------------------------------------------------------------------------
# mean in English Native Speaker: 0 (reference group)
# mean in ELL Speaker: -0.159
# variance: 1 (reference group), 0.795 (ELL)

(0 - (-.159)) / sqrt((499*1 + 499*.795)/(499 + 499))


## ----compstress----------------------------------------------------------------------------------------------------
# mean in English Native Speaker: 0 (reference group)
# mean in ELL Speaker: -0.332
# variance: 1 (reference group), 0.778 (ELL)

(0 - (-.332)) / sqrt((499*1 + 499*.778)/(499 + 499))


## ----wald----------------------------------------------------------------------------------------------------------
# H0: Average anxiety is equivalent across 
# English Native and ELL speakers
lavTestWald(fit.partial4, constraints = "alpha.1.g1 == alpha.1.g2")

# H0: Average stress is equivalent across 
# English Native and ELL speakers
lavTestWald(fit.partial4, constraints = "alpha.2.g1 == alpha.2.g2")

