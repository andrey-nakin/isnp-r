run.plot <-
  function(exp.data, legendprop, xprop, mapper, plotter = NA,
           xlab = NA, ylab = NA, main = NA, col = NA, legend.position = "topright") {
    
    if (is.na(col)) {
      col = plot.colors();
    }
    
    data <- exp.data$data
    runProps <- exp.data$runProps
    seriesProps <- exp.data$seriesProps
    
    d <- lapply(
      seq(from = 1, to = length(data), by = 1),
      function(runNo) {
        runData <- data[[runNo]]
        
        yData <- lapply(
          seq(from = 1, to = length(runData), by = 1),
          function(seriesNo) {
            seriesData <- runData[[seriesNo]]
            if (is.null(seriesData)) {
              return (list(value = NA, min = NA, max = NA))
            } else {
              v <- mapper(seriesData, series.prop = seriesProps[[runNo]][[seriesNo]], rp = runProps[[runNo]])
              if (is.list(v)) {
                return (v)
              } else if (is.numeric(v)) {
                return (list(value = v, min = NA, max = NA))
              } else {
                return (list(value = NA, min = NA, max = NA))
              }
            }
          }
        )
        
        xData <- sapply(
          seq(from = 1, to = length(runData), by = 1),
          function(seriesNo) {
            rp <- seriesProps[[runNo]]
            props <- seriesProps[[runNo]][[seriesNo]]
            if (is.null(props)) {
              return (NA)
            } else {
              v <- as.numeric(props[[xprop]])
              if (is.numeric(v)) {
                return (v)
              } else {
                return (NA)
              }
            }
          }
        )

        return (list(
          x = xData,
          y = sapply(yData, function(v) v$value),
          y.min = sapply(yData, function(v) v$min),
          y.max = sapply(yData, function(v) v$max)
        ))
      }
    )
    
    y.min = suppressWarnings(Reduce(x = d, init = NA, f = function(x, l) {
      res <- min(c(x, min(l$y, na.rm = T)), na.rm = T)
      return (res)
    }))
    y.min = suppressWarnings(Reduce(x = d, init = y.min, f = function(x, l) {
      res <- min(c(x, min(l$y.min, na.rm = T)), na.rm = T)
      return (res)
    }))

    y.max = suppressWarnings(Reduce(x = d, init = NA, f = function(x, l) {
      res <- max(c(x, max(l$y, na.rm = T)), na.rm = T)
      return (res)
    }))
    y.max = suppressWarnings(Reduce(x = d, init = y.max, f = function(x, l) {
      res <- max(c(x, max(l$y.max, na.rm = T)), na.rm = T)
      return (res)
    }))
    
    plot(
      xlab = xlab, ylab = ylab, main = main,
      x = d[[1]]$x,
      y = d[[1]]$y,
      type = 'b', 
      pch=20,
      ylim = c(y.min, y.max),
      col = col[1]
    )
    arrows(
      d[[1]]$x, 
      d[[1]]$y.min, 
      d[[1]]$x, 
      d[[1]]$y.max, 
      length=0.03, 
      angle=90, 
      code=3,
      col = col[1]
    )
    
    for (seriesNo in seq(1, length(d), 1)) {
      lines(
        x = d[[seriesNo]]$x,
        y = d[[seriesNo]]$y,
        type = 'b', 
        pch=20,
        col = col[seriesNo]
      )
      arrows(
        d[[seriesNo]]$x, 
        d[[seriesNo]]$y.min, 
        d[[seriesNo]]$x, 
        d[[seriesNo]]$y.max, 
        length=0.03, 
        angle=90, 
        code=3,
        col = col[seriesNo]
      )
    }
    
    leg <- sapply(runProps, function(props) {
      if (is.null(props)) {
        return (NA)
      } else {
        v <- props[[legendprop]]
        if (is.null(v)) {
          return (NA)
        } else {
          return (v)
        }
      }
    })
    
    legend(
      legend.position, 
      legend = leg, 
      pch=20,
      col = col
    )
    
    if (is.function(plotter)) {
      plotter(d[[1]], runData, sp = seriesProps[[runNo]], rp = runProps[[runNo]])
    }
    
  }
