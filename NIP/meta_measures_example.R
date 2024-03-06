# Clear workspace
rm(list = ls())

# Load meta-measures

# install.packages("devtools")
# devtools::install_github("craddm/metaSDT")

library(metaSDT)

source("meta_measures_library.R")

# Example table
tab <- matrix(scan(text = "
    16 17 14 03
    02 11 19 18
"), nrow = 2, byrow = TRUE)

# Evaluate meta-d' -------------------------------------------------------------
fit_meta_d_MLE(tab[1,], tab[2,])

estimate_sensitivity(tab)
estimate_meta_sensitivity(tab)
estimate_M_ratio(tab)

# Evaluate information-theoretic measures --------------------------------------
estimate_meta_I(tab)
estimate_meta_Ir1(tab)
estimate_meta_Ir2(tab)

estimate_RMI(tab)
estimate_RMI_with_bias_reduction(tab)

