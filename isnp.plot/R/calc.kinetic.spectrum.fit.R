# Calculates fitting parameter <a> of Gneis neutron spectrum approximation function
# exp.data - experiment raw data parsed
# spectrum - spectrum calculated by calc.kinetic.spectrum function
# Returns: 2d array with <a> values, row - run index, column - series index
calc.kinetic.spectrum.fit <-
  function(exp.data, spectrum) {
    res <- exp.array.2d(exp.data, filler = function(series.data, runNo, seriesNo, ...) {
      x <- spectrum[runNo, seriesNo, 1, ]
      y <- spectrum[runNo, seriesNo, 2, ]
      if (is.nan(min(y))) {
        return (NA)
      }
      
      xx <- x[which(x >= 1)]
      yy <- y[which(x >= 1)]
      
      df <- data.frame(x = xx, y = log10(yy))
      fit <- nls(y ~ calc.gneis.spectrum.log.approx(x, a), data = df, start = list(a = 0.1))
      
      a <- coef(fit)[1]
      return (10^a)
    })
    
    return(res)
  }