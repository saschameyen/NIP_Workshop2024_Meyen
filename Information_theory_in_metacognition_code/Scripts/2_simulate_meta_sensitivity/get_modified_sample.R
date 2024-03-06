get_modified_sample_normal_case <- function()
{
  new_tab2 <- c( 2, 11, 19, 18)
  new_tab1 <- c(16, 17, 14,  3)

  new_tab <- rbind(r2 = new_tab2, r1 = new_tab1)
  new_tab
}   
   

get_modified_sample_worst_case <- function()
{
  new_tab2 <- c( 6,  7, 19, 18)
  new_tab1 <- c(16, 17,  9,  8)

  new_tab <- rbind(r2 = new_tab2, r1 = new_tab1)
  new_tab
}

get_modified_sample_best_case <- function()
{
  new_tab2 <- c( 0, 13, 17, 20)
  new_tab1 <- c(20, 13, 17,  0)

  new_tab <- rbind(r2 = new_tab2, r1 = new_tab1)
  new_tab
}