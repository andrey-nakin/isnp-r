run.plot.mean <-
  function(data, runProps, seriesProps, legendprop, xprop, mapper, conf.level = 0.999, xlab = NA, ylab = NA, main = NA) {
    run.plot(data, runProps = runProps, seriesProps = seriesProps, legendprop = legendprop, xprop = xprop, xlab = xlab, ylab = ylab, main = main, mapper = function(df) {
      sample <- mapper(df)
      sample.len <- length(sample)
      sample.mean <- mean(sample)
      sample.mean.sd <- sd(sample) / sqrt(sample.len)
      return (list(
        value = sample.mean,
        min = sample.mean + sample.mean.sd * qt((1 - conf.level) / 2, df = sample.len),
        max = sample.mean + sample.mean.sd * qt((1 + conf.level) / 2, df = sample.len)
      ))
    })
  }
