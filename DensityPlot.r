## Author: Hosein Hashemizadeh

DensityPlot <- function(x, 
                        bw = "nrd0", 
                        lineCol = 'black', 
                        shadeCol, 
                        lwd = 3, 
                        lty = 1,
                        shadeLo, 
                        shadeHi , 
                        xlim = NULL, 
                        ylim = NULL, 
                        main = "", 
                        log = '', 
                        bty = 'n',
                        cex.axis = 1, 
                        cex.lab = 1,
                        xlab = '', 
                        ylab = "Density", 
                        add = FALSE, ...) {

  dens = density(x, na.rm = TRUE, bw = bw, ...)
  
  if ( add ) {
    par(new=TRUE)
    plot(dens, 
         xlim = xlim, 
         ylim = ylim, 
         lwd = lwd, 
         lty = lty,
         xaxt = 'n', 
         yaxt = 'n', 
         bty = 'n', 
         xlab = '', 
         ylab = '',
         main = main, 
         log = log, 
         col = lineCol)
  } else {
    
    par(cex.axis = cex.axis, cex.lab = cex.lab)
    
    plot(dens, 
         xlim = xlim, 
         ylim = ylim, 
         lwd = lwd, 
         lty = lty,
         xaxt = 'n', 
         bty = bty,
         xlab = xlab,
         ylab = ylab,
         main = main, 
         log = log, 
         col = lineCol)
    if (bty == 'n') {
      axis(1, pos = 0)
    } else {
      axis(1, pos = -.045)  
    }
    
  }
  
  if (!missing(shadeCol) & !missing(shadeLo) & !missing(shadeHi) ) {
    
    x=dens$x ; y=dens$y
    with(dens, polygon(c(shadeLo, x[x > shadeLo & x < shadeHi],shadeHi), c(0, y[x > shadeLo & x < shadeHi],0), col = shadeCol, border = shadeCol ) )
    
  } else if (!missing(shadeCol)) {
    
    with(dens, polygon(dens, col = shadeCol ))
    
  } 
  
}

# END