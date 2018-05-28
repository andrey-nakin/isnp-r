series.hist.plot <-
  function(data, seriesProps, legendprop, xprop, mapper, breaks, conf.level = 0.999, log = NA,
           xlab = NA, ylab = NA, main = NA, col = NA) {
    
    if (is.na(col)) {
      col = plot.colors();
    }
    
    lapply(
      seq(from = 1, to = length(data), by = 1),
      function(runNo) {
        runData <- d[[runNo]]
        d <- lapply(
          seq(from = 1, to = length(runData), by = 1),
          function(seriesNo) {
            seriesData <- runData[[seriesNo]]
            if (is.null(seriesData)) {
              return (list(x = NA, y = NA))
            } else {
              sample <- mapper(seriesData, sp = seriesProps[[runNo]][[seriesNo]])
              sample.hist <- hist(x = sample, plot = F, breaks = breaks)
              sample.len <- length(sample)
              y = sample.hist$density[which(sample.hist$counts > 0)]
              c = sample.hist$counts[which(sample.hist$counts > 0)]
              y.min = sapply(c, function(x) stats::binom.test(x, sample.len, conf.level = conf.level)$conf.int[1])
              y.max = sapply(c, function(x) stats::binom.test(x, sample.len, conf.level = conf.level)$conf.int[2])
              x <- tail(sample.hist$breaks, n = -1)[which(sample.hist$counts > 0)]
              return (list(
                x = x, 
                y = y,
                y.min = y.min, 
                y.max = y.max
              ))
            }
          }
        )
        
        dd <- d[[1]]
        plot(
          x = dd$x,
          y = dd$y,
          log = log,
          pch=20,
          type = 'b',
          col = col[1],
          xlab = xlab, ylab = ylab, main = main
        )
        arrows(dd$x, dd$y.min, dd$x, dd$y.max, length=0.03, angle=90, code=3, col = col[1])

        for (seriesNo in seq(from = 2, to = length(runData), by = 1)) {
          dd <- d[[seriesNo]]
          
          lines(
            x = dd$x,
            y = dd$y,
            type = 'b', 
            col = col[seriesNo], 
            pch=20
          )
          
          arrows(dd$x, dd$y.min, dd$x, dd$y.max, length=0.03, angle=90, code=3, col = col[seriesNo])
        }
        
        leg <- sapply(seriesProps[[runNo]], function(props) {
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
          "topright", 
          legend = leg, 
          pch=20,
          col = col
        )
        
      }
    )
    
    
  }