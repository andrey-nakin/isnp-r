# Perform the given action per series
# exp.data - experiment data parsed by exp.data function
# closure - function performing the action
do.per.run <-
  function(exp.data, closure) {
    for (runNo in seq(1, exp.data$numOfRuns, 1)) {
      props <- exp.data$expProps
      props <- append(props, exp.data$runProps[[runNo]])
      
      closure(
        runNo = runNo, 
        numOfSeries = exp.data$numOfSeries,
        props = props,
        exp.data = exp.data,
        series.props.list = exp.data$seriesProps[[runNo]]
      )
    }
  }
