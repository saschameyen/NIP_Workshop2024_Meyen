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

# Alternative meta-d'fits  -----------------------------------------------------

library(statConfR)
stimulus <- rep(c(-1, +1), rowSums(tab))
rating <- c(rep(1:4, tab[1,]), rep(1:4, tab[2,]))
correct <- rep(c(1, 0, 0, 1), c(sum(tab[1, 1:2]), 
                                sum(tab[1, 3:4]), 
                                sum(tab[2, 1:2]), 
                                sum(tab[2, 3:4])))

dat <- data.frame(stimulus = factor(stimulus),
                  rating = factor(rating),
                  correct = correct,
                  participant = 1)

table(dat$correct, dat$rating)

fitMetaDprime(dat)

# ------------------------------------------------------------------------------
library(stats)
# install.packages("optimx")
library(optimx)
source("translated_meta_fit.R")

fit_meta_d_MLE_Meyen(tab[1,], tab[2,])

#  -----------------------------------------------------------------------------

output = system(paste('python adapt_fit_meta_d_MLE.py ', 
                paste0(tab[1, ], collapse = " "),
                paste0(tab[2, ], collapse = " ")),
                intern = TRUE)
l <- length(output)
sensitivity <- output[l-2]
meta_sensitivity <- output[l-1]
m_ratio <- output[l]