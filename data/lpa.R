## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(tidySEM)
library(dplyr)
library(tidyr)
library(ggplot2)


## ----loaddata, message = FALSE, warning = FALSE--------------------------------------------------------------------
kids <- read.csv("data/projectkids.csv")

# data frame with auxiliary outcome included
kids_aux <- kids %>% select(chaos1:chaos6, hpc_mean) %>%
  drop_na()

# data frame with just PLA indicators
kids_chaos <- kids_aux %>% select(chaos1:chaos6) %>%
  mutate(chaos1 = 6 - chaos1,
         chaos4 = 6 - chaos4,
         chaos6 = 6 - chaos6)

colnames(kids_chaos) <- c("bedtimeroutine", "hearthink", "zoo", "stayontop", "tvon", "calm")
colnames(kids_aux) <- c("bedtimeroutine", "hearthink", "zoo", "stayontop", "tvon", "calm", "hpc_mean")


## ----desc----------------------------------------------------------------------------------------------------------
desc <- descriptives(kids_chaos)
desc <- desc[, c("name", "n", "missing", "unique", "mean", "median",
    "sd", "min", "max")]
desc


## ----descplot------------------------------------------------------------------------------------------------------
chaos_plot <- kids_chaos
names(chaos_plot) <- paste0("Value.", names(chaos_plot))
chaos_plot <- reshape(chaos_plot, 
                      varying = names(chaos_plot), 
                      direction = "long", 
                      timevar = "Variable")

ggplot(chaos_plot, aes(x = Value)) + 
  geom_histogram() + 
  facet_wrap(~Variable) +
  theme_bw()


## ----est, eval = F-------------------------------------------------------------------------------------------------
## set.seed(9710)
## res <- mx_profiles(data = kids_chaos, classes = 1:4)
## saveRDS(res, file = "data/LPA_results.RDS")


## ----loadfit-------------------------------------------------------------------------------------------------------
res <- readRDS("data/LPA_results.RDS")


## ----fit-----------------------------------------------------------------------------------------------------------
fit <- table_fit(res)  # model fit table
fit[, c("Name", "LL", "Parameters", "n", "BIC", "Entropy", "prob_min",
    "prob_max", "n_min", "n_max", "np_ratio", "np_local")]


## ----plotfit-------------------------------------------------------------------------------------------------------
plot(fit)


## ----lrlmr---------------------------------------------------------------------------------------------------------
lr_lmr(res)


## ----blrt, eval = F------------------------------------------------------------------------------------------------
## set.seed(1)
## res_blrt <- BLRT(res, replications = 5)


## ----altmod--------------------------------------------------------------------------------------------------------
res_alt <- mx_profiles(kids_chaos, classes = 4, variances = "varying")

# The example below shows you how to have varying variances + covariances within classes
#res_alt <- mx_profiles(kids_chaos, classes = 4, variances = "varying", covariances = "varying")

compare <- list(res[[4]], res_alt)
table_fit(compare)


## ----switchlabels--------------------------------------------------------------------------------------------------
res_final <- mx_switch_labels(res[[4]])


## ----classprob1----------------------------------------------------------------------------------------------------
class_prob(res_final, type = "sum.mostlikely") # assumes no classification error


## ----classprob2----------------------------------------------------------------------------------------------------
class_prob(res_final, type = "sum.posterior") # includes classification error

#class_prob(res_final, type = "individual") # classification probabilities for each individual


## ----classprob3----------------------------------------------------------------------------------------------------
class_prob(res_final, type = "mostlikely.class") # values on diagonal should be > .7


## ----classprob4----------------------------------------------------------------------------------------------------
class_prob(res_final, type = "avg.mostlikely") # values on diagonal should be > .7


## ----tableresult---------------------------------------------------------------------------------------------------
table_results(res_final, columns = c("label", "est", "se", "confint",
    "class")) %>% pivot_wider(id_cols = label, names_from = class, values_from = est:confint, names_vary = "slowest")


## ----tableresult2--------------------------------------------------------------------------------------------------
table_results(res_final, columns = c("label", "confint", "class")) %>% 
  filter(stringr::str_detect(label, "Means")) %>%
  pivot_wider(id_cols = label, 
              names_from = class, 
              values_from = confint)


## ----plotprofile---------------------------------------------------------------------------------------------------
p <- plot_profiles(res_final) 

p + facet_wrap(~Class)


## ----auxmod--------------------------------------------------------------------------------------------------------
aux_hpc <- BCH(res_final, data = kids_aux$hpc_mean)
summary(aux_hpc)


## ----auxmod2-------------------------------------------------------------------------------------------------------
lr_test(aux_hpc, compare = "M")


## ----newcase-------------------------------------------------------------------------------------------------------
kids_chaos_new <- data.frame(bedtimeroutine = 1, 
                             hearthink = 4, 
                             zoo = 3, 
                             stayontop = 4, 
                             tvon = 1, 
                             calm = 1)

predict(res_final, newdata = kids_chaos_new)

