calc.kinetic.spectrum <- function(exp.data, range, breakNum, adjust.range = T, conf.level = 0.95, detector.square = NA) {

  estep <- (range[2] - range[1]) / breakNum
  breaks.nonadjusted = seq(range[1], range[2], length.out = breakNum + 1)
  if (adjust.range) {
    breaks = breaks.nonadjusted + estep / 2
    range.adjusted = range + estep / 2
  } else {
    breaks = breaks.nonadjusted
    range.adjusted = range
  }
  exp.prop <- exp.data$expProps

  if (is.na(detector.square)) {
    detector.square <- as.numeric(exp.prop$DetectorWidth) * as.numeric(exp.prop$DetectorHeight)
  }
  
  count.to.flux <- function(count, series.prop, energy) {
    res <- count / as.numeric(series.prop$PrimaryProtons) / detector.square * as.numeric(exp.prop$IntensityOfProtons) / energy
    return (res)
  }
  
  res <- exp.array.4d(exp.data, d3 = 4, d4 = breakNum, filler = function(series.data, series.prop, dim, ...) {
    res <- array(NA, dim = dim)
    population <- series.data$KineticEnergy
    n <- length(population)
    data <- population[population >= range.adjusted[1] & population < range.adjusted[2]]
    h <- hist(data, breaks = breaks, plot = F)
    res[1, ] <- tail(breaks.nonadjusted, n = -1)
    res[2, ] <- count.to.flux(h$counts, series.prop, energy = estep)
    
    for (i in seq(1, breakNum, 1)) {
      ci <- stats::binom.test(x = h$counts[i], n = n, conf.level = conf.level)$conf.int
      res[3, i] <- count.to.flux(ci[1] * n, series.prop, energy = estep)
      res[4, i] <- count.to.flux(ci[2] * n, series.prop, energy = estep)
    }
    
    return (res)
  })
  
  return (res)
  
}
