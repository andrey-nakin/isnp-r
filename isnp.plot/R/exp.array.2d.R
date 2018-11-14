# return a 2d-array with experiment data processed
# the <filler> function should return a scalar
exp.array.2d <- function(exp.data, filler) {
  res <- array(NA, dim = c(exp.data$numOfRuns, exp.data$numOfSeries))
  
  for (runNo in seq(1, exp.data$numOfRuns)) {
    run.data <- exp.data$data[[runNo]]
    numOfSeries <- length(run.data)
    
    if (numOfSeries > 0) {
      for (seriesNo in seq(1, numOfSeries, 1)) {
        series.data <- run.data[[seriesNo]]
        if (!is.null(series.data)) {
          res[runNo, seriesNo] <- filler(
            series.data = series.data,
            exp.prop = exp.data$expProps,
            run.prop = exp.data$runProps[[runNo]],
            series.prop = exp.data$seriesProps[[runNo]][[seriesNo]],
            runNo = runNo,
            seriesNo = seriesNo
          )
        }
      }
    }
  }
  
  return (res)
}
