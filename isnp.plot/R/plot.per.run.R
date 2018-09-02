# Builds experiment plots, one per run, every plot displays multiple graphs, one per series
plot.per.run <-
  function(exp.data, data.func, legend.prop = NA, main.func = NULL, plotter.func = NULL, xlab = NA, ylab = NA, log = NA, col = NA, type = 'b', pch = 20, legend.position = "topright") {
    
    if (is.na(col)) {
      col = plot.colors();
    }
    
    do.per.run(exp.data, function(runNo, numOfSeries, props, exp.data, ...) {
      
      data <- lapply(1 : numOfSeries, function(seriesNo) {
        res <- data.func(
          runNo = runNo, 
          seriesNo = seriesNo
        )
      })

      x.min <- Reduce(function(a, b) {
        if (is.null(b)) {
          return (a)
        } else {
          m <- min(b$x)
          if (is.na(a)) {
            return (m)
          } else {
            return (min(c(a, m)))
          }
        }
      }, data, NA)

      x.max <- Reduce(function(a, b) {
        if (is.null(b)) {
          return (a)
        } else {
          m <- max(b$x)
          if (is.na(a)) {
            return (m)
          } else {
            return (max(c(a, m)))
          }
        }
      }, data, NA)
      
      y.min <- Reduce(function(a, b) {
        if (is.null(b)) {
          return (a)
        } else {
          m <- min(b$y)
          if (is.na(a)) {
            return (m)
          } else {
            return (min(c(a, m)))
          }
        }
      }, data, NA)
      
      y.max <- Reduce(function(a, b) {
        if (is.null(b)) {
          return (a)
        } else {
          m <- max(b$y)
          if (is.na(a)) {
            return (m)
          } else {
            return (max(c(a, m)))
          }
        }
      }, data, NA)
      
      if (is.null(main.func)) {
        main = ""
      } else {
        main = main.func(props)
      }
      
      if (!is.null(data[[1]])) {
        x <- data[[1]]$x
        y <- data[[1]]$y
      } else {
        x <- c()
        y <- c()
      }
      
      xlim <- c(x.min, x.max)
      ylim <- c(y.min, y.max)
      
      plot(
        x = x,
        y = y,
        xlim = xlim,
        ylim = ylim,
        log = log,
        xlab = xlab,
        ylab = ylab,
        main = main,
        type = type,
        pch = pch,
        col = col[1]
      )
      
      nv <- lapply(2 : numOfSeries, function(seriesNo) {
        if (!is.null(data[[seriesNo]])) {
          lines(
            x = data[[seriesNo]]$x,
            y = data[[seriesNo]]$y,
            type = type,
            pch = pch,
            col = col[seriesNo]
          )
        }
      })

      if (!is.null(plotter.func)) {
        plotter.func(
          runNo = runNo,
          xlim = xlim,
          ylim = ylim
        )
      }
            
      grid(NULL, NULL, lty = 1, col = "cornsilk2")
      
      if (!is.na(legend.prop)) {
        
        leg <- sapply(1 : numOfSeries, function(seriesNo) {
          props <- exp.data$seriesProps[[runNo]][[seriesNo]]
          if (is.null(props)) {
            return (NA)
          } else {
            v <- props[[legend.prop]]
            if (is.null(v)) {
              return (NA)
            } else {
              return (paste(legend.prop, v, sep = " = "))
            }
          }
        })

        legend(
          legend.position, 
          legend = leg, 
          pch = pch,
          col = col
        )
      }
      
    })
    
  }
