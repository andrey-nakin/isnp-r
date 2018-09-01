# Perform the given action per series
# exp.data - experiment data parsed by exp.data function
# closure - function performing the action
do.per.series <-
  function(exp.data, closure) {
    for (runNo in seq(1, exp.data$numOfRuns, 1)) {
      for (seriesNo in seq(1, exp.data$numOfSeries, 1)) {
        closure(
          runNo = runNo, 
          seriesNo = seriesNo, 
          exp.data = exp.data,
          run.props = exp.data$runProps[[runNo]], 
          series.props = exp.data$seriesProps[[runNo]][[seriesNo]]
        )
      }
    }
  }