################################################################################
#' 
#' Run code for NIP retreat workshop on information theory and metacognition.
#' 
#' Dr. Sascha Meyen, saschameyen@gmail.com
#' 2024-03-01 10:03
#' 
################################################################################

rm(list = ls()) # Clear workspace
source("load_relevant_scripts.R")

# Standard example -------------------------------------------------------------
show_sdt_setting(d = 1,
                 c = 0)
show_meta_sdt_setting(d  = 1           , 
                      cs = c(-1, 0, +1))

# Sample -----------------------------------------------------------------------
set.seed(2024-0301-1045)
n <- 50
sample_tab <- sample_from_meta_sdt_setting(d  = 1           , 
                                           cs = c(-1, 0, +1),
                                           n  = n           )
sample_tab

sample_tab <- get_modified_sample_normal_case()
sample_tab




# Fit --------------------------------------------------------------------------

# Data ----------------------------------------------------
sample_tab

# Load package --------------------------------------------
# install.packages("devtools")
# devtools::install_github("craddm/metaSDT")
library(metaSDT)

# Meta-d' fit from Matt Craddock --------------------------
meta_fit <- fit_meta_d_MLE(sample_tab[2,], sample_tab[1,])
meta_fit <- round(meta_fit[, 1:9], 2)

meta_fit


estimate_meta_sensitivity(sample_tab[2:1,])

show_meta_sdt_setting(d  = meta_fit$meta_da    , 
                      cs = c(meta_fit$t2ca_rS1, 
                             meta_fit$meta_ca ,
                             meta_fit$t2ca_rS2))

# Modify sample: worst case ----------------------------------------------------
sample_tab_worst_case <- get_modified_sample_worst_case()
meta_fit <- demonstrate_meta_sdt_fit(sample_tab_worst_case)
meta_fit[, 1:10]

show_sdt_setting(d = meta_fit$da  ,
                 c = meta_fit$t1c1)
show_meta_sdt_setting(d  = meta_fit$meta_da    , 
                      cs = c(meta_fit$t2ca_rS1, 
                             meta_fit$meta_ca ,
                             meta_fit$t2ca_rS2))

# Modify sample: best case -----------------------------------------------------
sample_tab_best_case <- get_modified_sample_best_case()
meta_fit <- demonstrate_meta_sdt_fit(sample_tab_best_case)
meta_fit[, 1:10]

show_sdt_setting(d = meta_fit$da  ,
                 c = meta_fit$t1c1)
show_meta_sdt_setting(d  = meta_fit$meta_da    , 
                      cs = c(meta_fit$t2ca_rS1, 
                             meta_fit$meta_ca ,
                             meta_fit$t2ca_rS2))

# Information measures ---------------------------------------------------------

sapply(1:ncol(sample_tab), \(i) {
       pmi2(sample_tab[1,i]/sum(sample_tab[,i]))
})

sample_tab
sample_tab_worst_case
sample_tab_best_case

estimate_meta_sensitivity(sample_tab[2:1,])
estimate_meta_I(sample_tab)
estimate_meta_Ir1(sample_tab)
estimate_meta_Ir2(sample_tab)
estimate_RMI(sample_tab)
estimate_RMI_with_bias_reduction(sample_tab)

estimate_meta_sensitivity(sample_tab_worst_case)
estimate_meta_I(sample_tab_worst_case)
estimate_meta_Ir1(sample_tab_worst_case)
estimate_meta_Ir2(sample_tab_worst_case)
estimate_RMI(sample_tab_worst_case)
estimate_RMI_with_bias_reduction(sample_tab_worst_case)

estimate_meta_sensitivity(sample_tab_best_case)
estimate_meta_I(sample_tab_best_case)
estimate_meta_Ir1(sample_tab_best_case)
estimate_meta_Ir2(sample_tab_best_case)
estimate_RMI(sample_tab_best_case)
estimate_RMI_with_bias_reduction(sample_tab_best_case)


# Best and worst cases ---------------------------------------------------------

worst_case_low_type_1 <- matrix(c(17.5, 17.5,  7.5,  7.5,
                                   7.5,  7.5, 17.5, 17.5), nrow = 2, byrow = TRUE)
estimate_meta_sensitivity(worst_case_low_type_1)

best_case_low_type_1 <- matrix(c( 0, 7.5, 7.5, 35,
                                 35, 7.5, 7.5, 0 ), nrow = 2, byrow = TRUE)
estimate_meta_sensitivity(best_case_low_type_1)
estimate_meta_sensitivity(best_case_low_type_1*2)

best_case_high_type_1 <- matrix(c( 0, 2.5, 2.5, 45,
                                  45, 2.5, 2.5, 0 ), nrow = 2, byrow = TRUE)
estimate_meta_sensitivity(best_case_high_type_1)
estimate_meta_sensitivity(best_case_high_type_1*2)
