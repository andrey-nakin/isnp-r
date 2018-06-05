# build a data frame where every run represents a single column
run.column.table <- 
  function(exp.data, rows, cell.builder, header.prop, row.names) {
    df <- data.frame(row.names = row.names)
    exp.props <- expData$expProps
    
    for (runNo in seq(1, length(exp.data$data), 1)) {
      run.props <- expData$runProps[[runNo]]
      series.props <- expData$seriesProps[[runNo]]
      
      column <- sapply(
        seq(1, nrow(rows), 1),
        function(rowNo) {
          return (cell.builder(
            row = rows[rowNo,],
            run.data = exp.data$data[[runNo]],
            exp.props = exp.props,
            run.props = run.props,
            series.props = series.props
          ))
        }
      )
      
      prop <- run.props[[header.prop]]
      df[[prop]] = column
    }
    
    return (df)
  }
