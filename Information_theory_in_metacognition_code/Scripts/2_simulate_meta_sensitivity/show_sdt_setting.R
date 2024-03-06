show_sdt_setting <- function(d, c)
{
  tab <- get_probability_table_from_SDT(d = d, 
                                        c = c)
  visualize_sdt_setting(d = d, 
                        c = c)
  tab
}

show_meta_sdt_setting <- function(d, cs)
{
  tab <- get_probability_table_from_SDT(d  = d , 
                                        cs = cs)
  visualize_meta_sdt_setting(d  = d , 
                             cs = cs)

  tab
}