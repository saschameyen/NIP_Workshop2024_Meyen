
visualize_meta_sdt_setting <- function(d, cs)
{
  if (dev.cur() != 1) dev.off()
  dev.new(width = 8, height = 5)
  par(mar = c(5, 5, 0.5, 0.5))

  dx <- 0.01
  xs <- seq(-3.5, +3.5, dx)
  f1 <- dnorm(xs, mean = -d/2)
  f2 <- dnorm(xs, mean = +d/2)

  plot(xs, f1, type = "l",
       xlab = "Evidence",
       ylab = "Density",
       cex.lab = 2,
       cex.axis = 1.5,
       lwd = 3,
       ylim = c(0, 0.5))
  points(xs, f2, type = "l", lwd = 3)
  abline(v = cs[2], lwd = 3)
  abline(v = cs[1], lwd = 3)
  abline(v = +cs[3], lwd = 3)

  s <- xs > cs[2]  & xs < cs[3]
  color_area_under_curve(xs[s], f2[s], sky_blue)
  s <- xs > cs[3]  & xs < 4
  color_area_under_curve(xs[s], f2[s], bluish_green)
  s <- xs > cs[2]  & xs < cs[3]
  color_area_under_curve(xs[s], f1[s], orange)
  s <- xs > cs[3]  & xs < 4
  color_area_under_curve(xs[s], f1[s], reddish_purple)

}