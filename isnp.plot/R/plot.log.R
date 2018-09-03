# Draw a plot with 2 logarithmic axes
plot.log <-
  function(x, y, type = 'b', pch = 20, xlab = NA, ylab = NA, main = NA, y.min = NA, y.max = NA, col = NA, ylim = NA) {
    
    if (is.na(min(y))) {
      return()
    }
    
    if (is.na(col)) {
      col <- plot.colors()[1]
    }
    
    xx <- x[which(x > 0)]
    y.indices <- which(y > 0)
    yy <- y[y.indices]

    if (missing(ylim)) {
      if (!missing(y.min) && !missing(y.max)) {
        ylim = c(min(y.min), max(y.max))
      } else {
        ylim = c(min(y), max(y))
      }
    }
    
    ticks.major.lim <- c(floor(log10(min(xx))) - 2, floor(log10(max(xx))) + 2)
    ticks.major.x <- 10^(ticks.major.lim[1] : ticks.major.lim[2])
    ticks.minor.x <- rep(1:9, length(ticks.major.x)) * (10^rep(ticks.major.lim[1] : ticks.major.lim[2], each=9))
    
    ticks.major.lim <- c(floor(log10(ylim[1])) - 2, floor(log10(ylim[2])) + 2)
    ticks.major.y <- 10^(ticks.major.lim[1] : ticks.major.lim[2])
    ticks.minor.y <- rep(1:9, length(ticks.major.y)) * (10^rep(ticks.major.lim[1] : ticks.major.lim[2], each=9))
    
    plot(
      x = xx,
      y = yy,
      log = "xy",
      type = type,
      pch = pch,
      col = col,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      main = main,
      axes = F
    )
    
    axis(side = 1, at = ticks.minor.x, labels=NA, tcl = par("tcl") * 0.5)
    axis(side = 2, at = ticks.minor.y, labels=NA, tcl = par("tcl") * 0.5)
    
    axis(side = 1, at = ticks.major.x)
    axis(side = 2, at = ticks.major.y)
    
    grid(NULL, NULL, lty = 1, col = "cornsilk2")
    
    if (!missing(y.min) && !missing(y.max)) {
      arrows(
        xx, 
        y.min[y.indices],
        xx, 
        y.max[y.indices],
        length=0.02, 
        angle=90, 
        code=3,
        col = col
      )
    }
  }
