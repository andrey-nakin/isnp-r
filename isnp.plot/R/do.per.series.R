# Perform the given action per series
# exp.data - experiment data parsed by exp.data function
# closure - function performing the action
do.per.series <-
  function(exp.data, closure) {
    for (runNo in seq(1, exp.data$numOfRuns, 1)) {
      for (seriesNo in seq(1, exp.data$numOfSeries, 1)) {
        props <- exp.data$expProps
        props <- append(props, exp.data$runProps[[runNo]])
        props <- append(props, exp.data$seriesProps[[runNo]][[seriesNo]])
        
        closure(
          runNo = runNo, 
          seriesNo = seriesNo,
          props <- props
        )
      }
    }
  }