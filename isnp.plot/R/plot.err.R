plot.err <- function(x, y, y.min = NA, y.max = NA, type = 'b', pch = 20, xlab = NULL, ylab = NULL, main = NULL, 
                     sub = NULL, log = "", xaxt = NULL, yaxt = NULL, col = NA) {

  has.err <- !is.na(y.min) && !is.na(y.max)
  
  if (has.err) {
    ylim = c(min(y, y.min), max(y, y.max))
  } else {
    ylim = c(min(y), max(y))
  }

  if (is.na(col)) {
    col <- plot.colors()[1]
  }
  
  plot(
    x = x,
    y = y,
    ylim = ylim,
    type = type,
    pch = pch,
    xlab = xlab,
    ylab = ylab,
    main = main,
    sub = sub,
    log = log,
    xaxt = xaxt,
    yaxt = yaxt,
    col = col
  )
  
  if (has.err) {
    arrows(
      x,
      y.min,
      x,
      y.max,
      length=0.02,
      angle=90,
      code=3,
      col = col
    )
    
  }
  
}