## ----loadpack, message = FALSE, warning = FALSE--------------------------------------------------------------------
library(tidySEM)
library(dplyr)
library(tidyr)
library(ggplot2)


## ----loaddata, message = FALSE, warning = FALSE--------------------------------------------------------------------
plbs <- read.csv("data/preschool.csv")

plbs <- plbs %>% mutate(across(everything(), ~if_else(.x == 1.5, 1, .x)),
                across(everything(), ~ factor(.x, 
                                              labels = c("Does not Apply", 
                                                         "Sometimes Applies", 
                                                         "Most Often Applies"),
                                              ordered = T
                                              )
                       )
                )

colnames(plbs) <- c("dontcare", "cooperates", "distracted", "willinghelp", "silly", "acceptnew")


## ----desc----------------------------------------------------------------------------------------------------------
desc <- descriptives(plbs)
desc <- desc[, c("name", "n", "missing", "unique", "mode", "mode_value")]
desc


## ----histplot, message = FALSE, warning = FALSE--------------------------------------------------------------------
plbs_plot <- plbs
plbs_plot <- pivot_longer(plbs_plot, everything())

ggplot(plbs_plot, aes(x = value)) + 
  geom_histogram(stat="count") + 
  facet_wrap(~name) +
  theme_bw()


## ----est, eval = F-------------------------------------------------------------------------------------------------
## set.seed(9710)
## res <- mx_lca(data = plbs, classes = 1:6)
## saveRDS(res, file = "data/LCA_results.RDS")


## ----loadres-------------------------------------------------------------------------------------------------------
res <- readRDS("data/LCA_results.RDS")


## ----fit-----------------------------------------------------------------------------------------------------------
fit <- table_fit(res)  # model fit table
fit[, c("Name", "LL", "Parameters", "n","AIC", "BIC", "saBIC", "Entropy", "prob_min",
    "prob_max", "n_min", "n_max", "np_ratio", "np_local")]


## ----plotfit-------------------------------------------------------------------------------------------------------
plot(fit)


## ----lmrtest, message = FALSE, warning = FALSE---------------------------------------------------------------------
lr_lmr(res)


## ----blrt, eval = F, message = FALSE, warning = FALSE--------------------------------------------------------------
## set.seed(1)
## res_blrt <- BLRT(res, replications = 5)


## ----switchlabels--------------------------------------------------------------------------------------------------
res_final <- mx_switch_labels(res[[2]])


## ----classprob1----------------------------------------------------------------------------------------------------
class_prob(res_final, type = "sum.mostlikely") # assumes no classification error


## ----classprob2----------------------------------------------------------------------------------------------------
class_prob(res_final, type = "sum.posterior") # includes classification error

#class_prob(res_final, type = "individual") # classification probabilities for each individual


## ----classprob3----------------------------------------------------------------------------------------------------
class_prob(res_final, type = "mostlikely.class") # values on diagonal should be > .7


## ----classprob4----------------------------------------------------------------------------------------------------
class_prob(res_final, type = "avg.mostlikely") # values on diagonal should be > .7


## ----tableres------------------------------------------------------------------------------------------------------
table_results(res_final, columns = c("label", "est", "confint",
    "class")) %>% filter(str_detect(label, "Thresholds")) %>% 
  mutate(label = str_split_i(label, "\\.", 2)) %>%
  pivot_wider(id_cols = label, names_from = class, values_from = est:confint, names_vary = "slowest")


## ----tableprob-----------------------------------------------------------------------------------------------------
table_prob(res_final) %>% 
  pivot_wider(names_from = group, 
              values_from = Probability)


## ----plotprob------------------------------------------------------------------------------------------------------
plot_prob(res_final) +
  theme(axis.text.x = element_text(angle=45, vjust = 0.5))


## ----newcase-------------------------------------------------------------------------------------------------------
plbs_new <- plbs[10,]
plbs_new

predict(res_final, newdata = plbs_new)

