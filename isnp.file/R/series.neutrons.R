# reads 2d list of data frames with neutron data
series.neutrons <- 
  function(basedir = ".", colClasses = NA) {
    res <- series.tables("neutrons.txt", basedir, colClasses = colClasses)
    return (res)
  }
