# return a list that encapsulates given data frame with experiment properties
exp.data <-
  function(basedir = ".", data) {
    
    get.value <- function(props, propname) {
      v <- props[[propname]]
      if (is.null(v)) {
        return (NA)
      } else {
        num <- suppressWarnings(as.numeric(v))
        if (is.na(num)) {
          return (v)
        } else {
          return (num)
        }
      }
    }

    data <- list(
      data = data, 
      expProps = exp.props(basedir), 
      runProps = run.props(basedir), 
      seriesProps = series.props(basedir),
      numOfRuns = length(data),
      numOfSeries = max(sapply(data, function(run.data) length(run.data)))
    )
    
    # collect experiment-level props
    if (!is.null(data$expProps)) {
      for (i in names(data$expProps)) {
        data[[i]] <- get.value(data$expProps, i)
      }
        
    }
    
    # collect run-level props
    if (!is.null(data$runProps) && data$numOfRuns > 0) {
      props <- list()
      for (runNo in seq(1, data$numOfRuns, 1)) {
        
      }
    }
    
    return (data)
  }
