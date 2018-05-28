run.plot <-
  function(data, runProps, seriesProps, legendprop, xprop, mapper, 
           xlab = NA, ylab = NA, main = NA) {
    
    all.values <- vector(mode="numeric", length=0)
    d <- lapply(
      seq(from = 1, to = length(data), by = 1),
      function(runNo) {
        runData <- d[[runNo]]
        
        yData <- lapply(
          seq(from = 1, to = length(runData), by = 1),
          function(seriesNo) {
            seriesData <- runData[[seriesNo]]
            if (is.null(seriesData)) {
              return (list(value = NA, min = NA, max = NA))
            } else {
              v <- mapper(seriesData, sp = seriesProps[[runNo]][[seriesNo]], rp = runProps[[runNo]])
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
      ylim = c(y.min, y.max)
    )
    arrows(
      d[[1]]$x, 
      d[[1]]$y.min, 
      d[[1]]$x, 
      d[[1]]$y.max, 
      length=0.03, 
      angle=90, 
      code=3
    )
    
  }
