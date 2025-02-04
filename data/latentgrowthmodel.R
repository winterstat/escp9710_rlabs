## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(lavaan)         # used to estimate CFAs and SEMs
library(semTools)       # used to compare models
library(ggplot2)        # to visualize trajectories
library(dplyr)          # data wrangling
library(tidyr)          # data wrangling


## ----data----------------------------------------------------------------------------------------------------------
diary <- rio::import(file = "data/longitudinal.csv")


## ----disvis--------------------------------------------------------------------------------------------------------
diary %>%
  select(psp1:psp5) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x=value)) +
  geom_histogram(bins = 10) + 
  facet_wrap(vars(name), scales = "free")


## ----skew, message = FALSE, warning = FALSE------------------------------------------------------------------------
# Univariate skew and kurtosis
apply(diary[,2:5], 2, skew)
apply(diary[,2:5], 2, kurtosis)


## ----visual--------------------------------------------------------------------------------------------------------
diary %>% select(id:psp5) %>%
  summarize(across(psp1:psp5, ~mean(.x, na.rm = T))) %>%
  pivot_longer(cols = psp1:psp5, 
               names_to = "time", names_prefix = "psp", 
               names_transform = as.numeric, 
               values_to = "score") %>%
  ggplot(aes(x = time, y = score)) + 
  geom_point() +
  geom_line() +
  labs(x = "Time Point", y = "Average PSP Score") +
  theme_bw()


## ----lgm_specify---------------------------------------------------------------------------------------------------
lgm_linear <- '
i =~ 1*psp1 + 1*psp2 + 1*psp3 + 1*psp4 + 1*psp5
s =~ 0*psp1 + 1*psp2 + 2*psp3 + 3*psp4 + 4*psp5
'

lgm_quad <- '
i =~ 1*psp1 + 1*psp2 + 1*psp3 + 1*psp4 + 1*psp5
s =~ 0*psp1 + 1*psp2 + 2*psp3 + 3*psp4 + 4*psp5
q =~ 0*psp1 + 1*psp2 + 4*psp3 + 9*psp4 + 16*psp5
'

lgm_cube <- '
i =~ 1*psp1 + 1*psp2 + 1*psp3 + 1*psp4 + 1*psp5
s =~ 0*psp1 + 1*psp2 + 2*psp3 + 3*psp4 + 4*psp5
q =~ 0*psp1 + 1*psp2 + 4*psp3 + 9*psp4 + 16*psp5
c =~ 0*psp1 + 1*psp2 + 8*psp3 + 27*psp4 + 64*psp5
'

lgm_basis <- '
i =~ 1*psp1 + 1*psp2 + 1*psp3 + 1*psp4 + 1*psp5
s =~ 0*psp1 + psp2 + psp3 + psp4 + 1*psp5
'


## ----lgm est-------------------------------------------------------------------------------------------------------
# Fit the linear LGM using the growth() function
fit_linear <- growth(lgm_linear, data = diary, 
                     estimator = "mlr", missing = "fiml")

# Fit the quadratic LGM using the growth() function
fit_quad <- growth(lgm_quad, data = diary, 
                   estimator = "mlr", missing = "fiml")

# Fit the quadratic LGM using the growth() function
fit_cube <- growth(lgm_cube, data = diary, 
                   estimator = "mlr", missing = "fiml")

# Fit the basis LGM using the growth() function
fit_basis <- growth(lgm_basis, data = diary, 
                    estimator = "mlr", missing = "fiml")


## ----compare-------------------------------------------------------------------------------------------------------
net(fit_linear, fit_quad, fit_cube, fit_basis)


## ----compare2------------------------------------------------------------------------------------------------------
comp_lq <- compareFit(fit_linear, fit_quad)
comp_lq@nested


## ----compare3------------------------------------------------------------------------------------------------------
comp_lc <- compareFit(fit_linear, fit_cube)
comp_lc@nested


## ----compare3b-----------------------------------------------------------------------------------------------------
comp_lb <- compareFit(fit_linear, fit_basis)
comp_lb@nested


## ----compare4------------------------------------------------------------------------------------------------------
comp_qcb <- compareFit(fit_quad, fit_cube, fit_basis)
comp_qcb@nested


## ----compare5------------------------------------------------------------------------------------------------------
summary(fit_quad)


## ----fit1----------------------------------------------------------------------------------------------------------
summary(fit_basis, fit.measures = T, estimates = F)


## ----localfit1-----------------------------------------------------------------------------------------------------
residuals(fit_basis, type = "cor.bollen")


## ----param---------------------------------------------------------------------------------------------------------
summary(fit_basis, std = T, rsquare = T)


## ----visual2-------------------------------------------------------------------------------------------------------
# Get observed growth curve
obs_curve <- diary %>% select(id:psp5) %>%
  summarize(across(psp1:psp5, ~mean(.x, na.rm = T))) %>%
  pivot_longer(cols = psp1:psp5, 
               names_to = "time", names_prefix = "psp", 
               names_transform = as.numeric)

# Get predicted growth curve
pred_curve <- lavPredict(fit_basis) %>% as.data.frame() %>% 
  rowwise() %>%
  mutate(t1 = i + 0*s,
         t2 = i + .743*s,
         t3 = i + .904*s,
         t4 = i + .948*s,
         t5 = i + 1*s) %>%
  pivot_longer(cols = t1:t5, 
               names_to = "time", names_prefix = "t", 
               names_transform = as.numeric) %>%
  group_by(time) %>%
  summarize(value = mean(value)) %>%
  select(time, value)

# Create the plot:
bind_rows(obs_curve, pred_curve, .id = "type") %>%
  mutate(type = ifelse(type == "1", "Observed", "Predicted")) %>%
  ggplot(aes(x = time, group = type, color = type, linetype = type)) +
  geom_line(aes(y = value), 
            linewidth = .8) +
  geom_point(aes(y = value), 
             size = 1) +
  labs(y = "Average PSP Score") +
  scale_x_continuous("Day", breaks = c(1, 2, 3, 4,5), 
                     labels = c(1,5,9,13,17)) +
  theme_classic() +
  theme(legend.position = "bottom")


## ----visual3-------------------------------------------------------------------------------------------------------
# Get predicted growth curve
pred_curve2 <- lavPredict(fit_quad) %>% as.data.frame() %>% 
  rowwise() %>%
  mutate(t1 = i + 0*s + 0*q,
         t2 = i + 1*s + 1*q,
         t3 = i + 2*s + 4*q,
         t4 = i + 3*s + 9*q,
         t5 = i + 4*s + 16*q) %>%
  pivot_longer(cols = t1:t5, 
               names_to = "time", names_prefix = "t", 
               names_transform = as.numeric) %>%
  group_by(time) %>%
  summarize(value = mean(value)) %>%
  select(time, value)

# Create the plot:
bind_rows(obs_curve, pred_curve2, .id = "type") %>%
  mutate(type = ifelse(type == "1", "Observed", "Predicted")) %>%
  ggplot(aes(x = time, group = type, color = type, linetype = type)) +
  geom_line(aes(y = value), 
            linewidth = .8) +
  geom_point(aes(y = value), 
             size = 1) +
  labs(y = "Average PSP Score") +
  scale_x_continuous("Day", breaks = c(1, 2, 3, 4,5), 
                     labels = c(1,5,9,13,17)) +
  theme_classic() +
  theme(legend.position = "bottom")


## ----invariant-----------------------------------------------------------------------------------------------------
lgm_ti_cov <- '
i =~ 1*psp1 + 1*psp2 + 1*psp3 + 1*psp4 + 1*psp5
s =~ 0*psp1 + psp2 + psp3 + psp4 + 1*psp5

i ~ sexm
s ~ sexm
'

fit_ti_cov <- growth(lgm_ti_cov, data = diary, 
                     estimator = "mlr", missing = "fiml")

summary(fit_ti_cov, fit.measures = T, rsquare = T, std = T)


## ----varying-------------------------------------------------------------------------------------------------------
lgm_tv_cov <- '
i =~ 1*psp1 + 1*psp2 + 1*psp3 + 1*psp4 + 1*psp5
s =~ 0*psp1 + psp2 + psp3 + psp4 + 1*psp5

# regression paths
psp1 ~ ssa1
psp2 ~ ssa2
psp3 ~ ssa3
psp4 ~ ssa4
psp5 ~ ssa5

# covariances between time-varying covariate
ssa1 ~~ ssa2 + ssa3 + ssa4 + ssa5
ssa2 ~~ ssa3 + ssa4 + ssa5
ssa3 ~~ ssa4 + ssa5
ssa4 ~~ ssa5

# means of time varying covariate
ssa1 ~ 1
ssa2 ~ 1
ssa3 ~ 1
ssa4 ~ 1
ssa5 ~ 1
'

fit_tv_cov <- growth(lgm_tv_cov, data = diary, 
                     estimator = "mlr", missing = "fiml")

summary(fit_tv_cov, fit.measures = T, rsquare = T, std = T)


## ----distal--------------------------------------------------------------------------------------------------------
lgm_outcome <- '
i =~ 1*psp1 + 1*psp2 + 1*psp3 + 1*psp4 + 1*psp5
s =~ 0*psp1 + psp2 + psp3 + psp4 + + 1*psp5

# regression path to distal outcome
ssa5 ~ i + s

# intercept of distal outcome
ssa5 ~ 1 
'

fit_outcome <- growth(lgm_outcome, data = diary, 
                     estimator = "mlr", missing = "fiml")

summary(fit_outcome, fit.measures = T, rsquare = T, std = T)


## ----resideq-------------------------------------------------------------------------------------------------------
lgm_basis_eq <- '
i =~ 1*psp1 + 1*psp2 + 1*psp3 + 1*psp4 + 1*psp5
s =~ 0*psp1 + psp2 + psp3 + psp4 + 1*psp5

psp1 ~~ a*psp1
psp2 ~~ a*psp2
psp3 ~~ a*psp3
psp4 ~~ a*psp4
psp5 ~~ a*psp5
'

fit_basis_eq <- growth(lgm_basis_eq, data = diary, 
                     estimator = "mlr", missing = "fiml")

comp_bbeq <- compareFit(fit_basis, fit_basis_eq)
comp_bbeq@nested


## ----resideq2------------------------------------------------------------------------------------------------------
lgm_basis_eqc <- '
i =~ 1*psp1 + 1*psp2 + 1*psp3 + 1*psp4 + 1*psp5
s =~ 0*psp1 + psp2 + psp3 + psp4 + 1*psp5

psp1 ~~ a*psp1
psp2 ~~ a*psp2
psp3 ~~ a*psp3
psp4 ~~ a*psp4
psp5 ~~ a*psp5

# lag-1 residual covariances
psp1 ~~ b*psp2
psp2 ~~ b*psp3
psp3 ~~ b*psp4
psp4 ~~ b*psp5

# lag-2 residual covariances
psp1 ~~ c*psp3
psp2 ~~ c*psp4
psp3 ~~ c*psp5

# lag-3 residual covariances
psp1 ~~ d*psp4
psp2 ~~ d*psp5

# lag-4 residual covariances
psp1 ~~ e*psp5

# constraints to apply the reducing residual covariance over time assumption
c == b^2
d == b^3
e == b^4
'

fit_basis_eqc <- growth(lgm_basis_eqc, data = diary, 
                     estimator = "mlr", missing = "fiml")

comp_eqc <- compareFit(fit_basis_eq, fit_basis_eqc)
comp_eqc@nested


## ----eqc_est-------------------------------------------------------------------------------------------------------
summary(fit_basis_eqc, std = T)


## ----ssagrowth-----------------------------------------------------------------------------------------------------
lgm_linear2 <- '
i =~ 1*ssa1 + 1*ssa2 + 1*ssa3 + 1*ssa4 + 1*ssa5
s =~ 0*ssa1 + 1*ssa2 + 2*ssa3 + 3*ssa4 + 4*ssa5
'

lgm_quad2 <- '
i =~ 1*ssa1 + 1*ssa2 + 1*ssa3 + 1*ssa4 + 1*ssa5
s =~ 0*ssa1 + 1*ssa2 + 2*ssa3 + 3*ssa4 + 4*ssa5
q =~ 0*ssa1 + 1*ssa2 + 4*ssa3 + 9*ssa4 + 16*ssa5
'

lgm_cube2 <- '
i =~ 1*ssa1 + 1*ssa2 + 1*ssa3 + 1*ssa4 + 1*ssa5
s =~ 0*ssa1 + 1*ssa2 + 2*ssa3 + 3*ssa4 + 4*ssa5
q =~ 0*ssa1 + 1*ssa2 + 4*ssa3 + 9*ssa4 + 16*ssa5
c =~ 0*ssa1 + 1*ssa2 + 8*ssa3 + 27*ssa4 + 64*ssa5
'

lgm_basis2 <- '
i =~ 1*ssa1 + 1*ssa2 + 1*ssa3 + 1*ssa4 + 1*ssa5
s =~ 0*ssa1 + ssa2 + ssa3 + ssa4 + 1*ssa5
'

# Fit the linear LGM using the growth() function
fit_linear2 <- growth(lgm_linear2, data = diary, 
                     estimator = "mlr", missing = "fiml")

# Fit the quadratic LGM using the growth() function
fit_quad2 <- growth(lgm_quad2, data = diary, 
                   estimator = "mlr", missing = "fiml")

# Fit the quadratic LGM using the growth() function
fit_cube2 <- growth(lgm_cube2, data = diary, 
                   estimator = "mlr", missing = "fiml")

# Fit the basis LGM using the growth() function
fit_basis2 <- growth(lgm_basis2, data = diary, 
                    estimator = "mlr", missing = "fiml")

comp2_lq <- compareFit(fit_linear2, fit_quad2)
comp2_lq@nested

comp2_lc <- compareFit(fit_linear2, fit_cube2)
comp2_lc@nested

comp2_lb <- compareFit(fit_linear2, fit_basis2)
comp2_lb@nested

comp2_qc <- compareFit(fit_quad2, fit_cube2)
comp2_qc@nested

comp2_qcb <- compareFit(fit_quad2, fit_cube2, fit_basis2)
comp2_qcb@nested

# Look at variances of linear and quadratic slope in quad model to check proportionality assumption
parameterEstimates(fit_quad2) %>% filter((lhs == "s" | lhs == "q") & op == "~~")


## ----parallel1-----------------------------------------------------------------------------------------------------
lgm_parallel <- '
issa =~ 1*ssa1 + 1*ssa2 + 1*ssa3 + 1*ssa4 + 1*ssa5
sssa =~ 0*ssa1 + ssa2 + ssa3 + ssa4 + 1*ssa5

ipsp =~ 1*psp1 + 1*psp2 + 1*psp3 + 1*psp4 + 1*psp5
spsp =~ 0*psp1 + psp2 + psp3 + psp4 + 1*psp5

# covariances between pairs of intercept and slope
issa ~~ ipsp
sssa ~~ spsp

# regression paths
sssa ~ ipsp
spsp ~ issa
'

# Fit the basis LGM using the growth() function
fit_parallel <- growth(lgm_parallel, data = diary, 
                    estimator = "mlr", missing = "fiml")

summary(fit_parallel, fit.measures = T, std = T, rsquare = T)


## ----parallellocal-------------------------------------------------------------------------------------------------
residuals(fit_parallel, type = "cor.bollen")

