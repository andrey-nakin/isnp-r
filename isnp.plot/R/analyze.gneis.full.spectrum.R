# Analyze neutron spectrum, compare it to GNEIS approximation function
analyze.gneis.full.spectrum <- 
  function(exp.data, breakNum, conf.level, series.prop = NULL, series.prop.title = NA, detector.square = NA, adjust.range = T) {
    
    range <- c(1, 1000)
    fit.range <- c(1, 800)
    col <- plot.colors()
    
    calc.error <- function(y, approx) {
      return (
        y / approx - 1
      )
    }
    
    if (is.na(series.prop.title)) {
      series.prop.title <- series.prop
    }
    
    my.spectra.10 <- calc.kinetic.spectrum(exp.data, range = c(0, 10.0), breakNum = breakNum, adjust.range = adjust.range, conf.level = conf.level, detector.square = detector.square)
    my.spectra.100 <- calc.kinetic.spectrum(exp.data, range = c(0, 100.0), breakNum = breakNum, adjust.range = adjust.range, conf.level = conf.level, detector.square = detector.square)
    my.spectra.1000 <- calc.kinetic.spectrum(exp.data, range = c(0, 1000.0), breakNum = breakNum, adjust.range = adjust.range, conf.level = conf.level, detector.square = detector.square)
    d <- dim(my.spectra.10)
    my.spectra <- array(NA, dim = c(d[1], d[2], d[3], 271))
    my.spectra[, , , 1:90] <- my.spectra.10[, , , 10:99]
    my.spectra[, , , 91:180] <- my.spectra.100[, , , 10:99]
    my.spectra[, , , 181:271] <- my.spectra.1000[, , , 10:100]

    my.approx.a <- calc.kinetic.spectrum.fit(exp.data, my.spectra)
    
    my.plotter <- function(runNo, seriesNo, props, ...) {
      x <- my.spectra[runNo, seriesNo, 1, ]
      y <- my.spectra[runNo, seriesNo, 2, ]
      if (is.na(min(y))) {
        return()
      }
      
      plot.log(
        x = x,
        y = y,
        y.min = my.spectra[runNo, seriesNo, 3, ],
        y.max = my.spectra[runNo, seriesNo, 4, ],
        xlab = "Energy, MeV",
        ylab = "Flux, n/(cm^2*s*MeV)",
        main = paste(
          range.to.string(range),
          paste("PL", props$PhysicsList, sep="="),
          paste(series.prop.title, props[[series.prop]], sep="="),
          sep = ", "
        )
      )
      
      f.approx <- function(e) calc.gneis.spectrum.approx(e, a = my.approx.a[runNo, seriesNo])
      idx <- which(x >= fit.range[1] & x < fit.range[2] & y > 0)
      fx <- x[idx]
      fy <- y[idx]
      e <- calc.error(fy, f.approx(fx))
      e.rms <- calc.rmsd(e)
      
      ax <- gen.x.values.log(fx, n = 91)
      lines(
        x = ax,
        y = f.approx(ax),
        type = 'l',
        pch = 20,
        col = plot.colors()[2]
      )
      
      legend(
        "topright", 
        legend = c("Experiment", "Approximation"),
        lty = 1,
        pch = c(20, NA),
        col = c(plot.colors()[1], plot.colors()[2])
      )
      
      mtext(
        paste(
          paste("a", format(my.approx.a[runNo, seriesNo], digits = 3), sep="="), 
          paste("RMS(delta)", format(e.rms, digits = 2), sep="="), 
          sep = ", "
        ),
        side = 3
      )
    }
    
    do.per.series(exp.data, my.plotter)
    
    plot.per.run(
      exp.data,
      legend.prop = series.prop,
      legend.prop.title = series.prop.title,
      log = "x",
      xlab = "Energy, MeV",
      ylab = "Relative Approximation Error",
      main.func = function(props) {
        res <- paste(
          "Approx. Error",
          range.to.string(range),
          paste("PL", props$PhysicsList, sep="="),
          sep = ", "
        )
        return (res)
      },
      data.func = function(runNo, seriesNo, ...) {
        x <- my.spectra[runNo, seriesNo, 1, ]
        y <- my.spectra[runNo, seriesNo, 2, ]
        f.approx <- function(e) calc.gneis.spectrum.approx(e, a = my.approx.a[runNo, seriesNo])
        idx <- which(x >= fit.range[1] & x < fit.range[2] & y > 0)
        fx <- x[idx]
        fy <- y[idx]
        delta <- calc.error(fy, f.approx(fx))
        return (list(
          x = fx,
          y = delta
        ))
      },
      plotter.func = function(xlim, ...) {
        lines(
          x = xlim,
          y = c(0, 0),
          type = 'l',
          col = 'red'
        )
      }
    )

    if (!is.null(series.prop) && !is.na(as.numeric(exp.data$seriesProps[[1]][[1]][[series.prop]]))) {
      do.per.run(exp.data, function(runNo, numOfSeries, props, series.props.list, ...) {
        
        x <- lapply(series.props.list, function(props) as.numeric(props[[series.prop]]))
        y <- lapply(1:numOfSeries, function(seriesNo) {
          x <- my.spectra[runNo, seriesNo, 1, ]
          y <- my.spectra[runNo, seriesNo, 2, ]
          f.approx <- function(e) calc.gneis.spectrum.approx(e, a = my.approx.a[runNo, seriesNo])
          idx <- which(x >= fit.range[1] & x < fit.range[2] & y > 0)
          fx <- x[idx]
          fy <- y[idx]
          delta <- calc.error(fy, f.approx(fx))
          return (calc.rmsd(delta))
        })
        
        plot(
          x = x,
          y = y,
          type = 'b',
          pch = 20,
          col = col[1],
          main = paste(
            "RMS Error",
            range.to.string(range),
            paste("PL", props$PhysicsList, sep="="),
            sep = ", "
          ),
          xlab = series.prop,
          ylab = "RMS Error"
        )
      })
    }

  }