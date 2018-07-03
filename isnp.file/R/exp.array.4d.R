# return a 4d-array with experiment data processed
# the <filler> function should return 2d array
exp.array.4d <- function(exp.data, d3, d4, filler) {
  res <- array(NA, dim = c(exp.data$numOfRuns, exp.data$numOfSeries, d3, d4))
  
  for (runNo in seq(1, exp.data$numOfRuns)) {
    run.data <- exp.data$data[[runNo]]
    numOfSeries <- length(run.data)
    
    if (numOfSeries > 0) {
      for (seriesNo in seq(1, numOfSeries, 1)) {
        res[runNo, seriesNo,,] <- filler(
          series.data = run.data[[seriesNo]],
          exp.prop = exp.data$expProps,
          run.prop = exp.data$runProps[[runNo]],
          series.prop = exp.data$seriesProps[[runNo]][[seriesNo]],
          dim = c(d3, d4)
        )
      }
    }
  }
  
  return (res)
}
