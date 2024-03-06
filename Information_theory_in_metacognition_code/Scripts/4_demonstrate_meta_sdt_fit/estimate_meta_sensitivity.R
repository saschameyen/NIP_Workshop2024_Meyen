estimate_meta_sensitivity <- function(tab)
{
  meta_fit <- demonstrate_meta_sdt_fit(tab)
  meta_sensitivity <- meta_fit$meta_da
  meta_sensitivity
}