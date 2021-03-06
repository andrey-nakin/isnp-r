# return a 3d-array with experiment data processed
# the <filler> function should return 1d array
exp.array.3d <- function(exp.data, d3, filler) {
  res <- array(NA, dim = c(exp.data$numOfRuns, exp.data$numOfSeries, d3))
  
  for (runNo in seq(1, exp.data$numOfRuns)) {
    run.data <- exp.data$data[[runNo]]
    numOfSeries <- length(run.data)
    
    if (numOfSeries > 0) {
      for (seriesNo in seq(1, numOfSeries, 1)) {
        series.data <- run.data[[seriesNo]]
        if (!is.null(series.data)) {
          res[runNo, seriesNo,] <- filler(
            series.data = series.data,
            exp.prop = exp.data$expProps,
            run.prop = exp.data$runProps[[runNo]],
            series.prop = exp.data$seriesProps[[runNo]][[seriesNo]],
            dim = c(d3),
            runNo = runNo,
            seriesNo = seriesNo
          )
        }
      }
    }
  }
  
  return (res)
}
