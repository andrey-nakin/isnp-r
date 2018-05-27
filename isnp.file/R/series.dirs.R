# returns vector of series directories
series.dirs <-
  function(basedir) {
    rundirs <- run.dirs(basedir)
    maxseries <- max(sapply(rundirs, function(rundir) {
      d <- dir(rundir, full.names = TRUE, recursive = FALSE, pattern="series[0-9]{3}")
      return (length(d))
    }))
    
    res <- array(dim = c(length(rundirs), maxseries))
    
    for (i in seq(from = 1, to = length(rundirs), by = 1)) {
      d <- dir(rundirs[i], full.names = TRUE, recursive = FALSE, pattern="series[0-9]{3}")
      sort(d)
      if (length(d) > 0) {
        for (j in seq(from = 1, to = length(d), by = 1)) {
          res[i, j] = d[j]
        }
      }
    }
    
    return(res)
  }
