demonstrate_meta_sdt_fit <- function(sample_tab)
{
  # Data -------------------------------------------------------------------------
  sample_tab

  # Load package -----------------------------------------------------------------
  # install.packages("devtools")
  # devtools::install_github("craddm/metaSDT")
  library(metaSDT)

  # Meta-d' fit from Matt Craddock -----------------------------------------------
  meta_fit <- fit_meta_d_MLE(sample_tab[2,], sample_tab[1,])
  meta_fit <- round(meta_fit, 2)

  meta_fit
}