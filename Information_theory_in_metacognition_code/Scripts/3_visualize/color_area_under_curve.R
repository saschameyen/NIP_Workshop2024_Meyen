color_area_under_curve <- function(x, y, col)
{
  polygon(c(x  , rev(x)),
          c(0*x, rev(y)),
          col = col)  
}