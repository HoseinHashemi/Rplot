## Author: Hosein Hashemizadeh

DensityPlot <- function(x, shadeCol = 'cyan', lwd = 3, shadeLo, shadeHi , xlim = NULL, ylim = NULL, main = "", log = '', xlab = '', add = FALSE, ...) {

  dens = density(x, ...)
  if ( add ) {
    par(new=TRUE)
    plot(dens, xlim = xlim, lwd = lwd, xaxt = 'n', yaxt = 'n', bty = 'n', xlab = xlab, main = main, log = log)
  } else {
    plot(dens, xlim = xlim, lwd = lwd, xaxt = 'n', bty = 'n', xlab = xlab, main = main, log = log)
    axis(1, pos = 0)
  }
  
  if (!missing(shadeLo) & !missing(shadeHi) ) {
    x=dens$x ; y=dens$y
    with(dens, polygon(c(shadeLo, x[x > shadeLo & x < shadeHi],shadeHi), c(0, y[x > shadeLo & x < shadeHi],0), col = shadeCol ) )
  } else {
    with(dens, polygon(dens, col = shadeCol ))
  }
  
}

# END