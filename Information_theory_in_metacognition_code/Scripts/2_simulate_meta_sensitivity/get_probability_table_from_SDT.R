get_probability_table_from_SDT <- function(d, cs)
{
  cs <- c(cs, Inf)
  tab <- matrix(NA, nrow = 2, ncol = length(cs))
  
  tab[1, ] <- diff(c(0, pnorm(cs, mean = +d/2))) / 2
  tab[2, ] <- diff(c(0, pnorm(cs, mean = -d/2))) / 2

  tab <- round(tab, 2)

  tab
}