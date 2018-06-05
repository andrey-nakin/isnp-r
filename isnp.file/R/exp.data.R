# return a list that encapsulates given data frame with experiment properties
exp.data <-
  function(basedir = ".", data) {
    data <- list(
      data = data, 
      expProps = exp.props(basedir), 
      runProps = run.props(basedir), 
      seriesProps = series.props(basedir)
    )
    return (data)
  }
