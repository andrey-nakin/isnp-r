# Analyze neutron spectrum, compare it to GNEIS approximation function
analyze.gneis.spectrum <- 
  function(exp.data, range, breakNum, conf.level, series.prop, series.prop.title = NA, detector.square = NA, adjust.range = T, fit.range) {
    
    if (is.na(series.prop.title)) {
      series.prop.title <- series.prop
    }
    
    my.spectra <- calc.kinetic.spectrum(exp.data, range = range, breakNum = breakNum, adjust.range = adjust.range, conf.level = conf.level, detector.square = detector.square)
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
      fx <- x[which(x >= fit.range[1] & x < fit.range[2] & y > 0)]
      fy <- y[which(x >= fit.range[1] & x < fit.range[2] & y > 0)]
      e <- log10(fy) - log10(f.approx(fx))
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
          paste("RMS(delta)", format(10^e.rms - 1, digits = 2), sep="="), 
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
        fx <- x[which(x >= fit.range[1] & x < fit.range[2] & y > 0)]
        fy <- y[which(x >= fit.range[1] & x < fit.range[2] & y > 0)]
        delta <- fy / f.approx(fx) - 1
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
    
  }