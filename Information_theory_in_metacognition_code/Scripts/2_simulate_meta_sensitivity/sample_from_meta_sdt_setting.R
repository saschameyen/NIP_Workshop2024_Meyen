sample_from_meta_sdt_setting <- function(d, cs, n)
{
  tab <- get_probability_table_from_SDT(d, cs)

  n_responses <- length(cs) + 1

  r2 <- sample(n_responses:1, n, prob = tab[1,], replace = TRUE)
  r1 <- sample(n_responses:1, n, prob = tab[2,], replace = TRUE)

  sample_tab <- rbind(y0 = table(r1), y1 = table(r2))

  rownames(sample_tab) = c("Y=+1", "Y=-1")
  if (n_responses == 4)
    colnames(sample_tab) = c("-1, high", "-1, low", "+1, low", "+1, high") 

  sample_tab
}