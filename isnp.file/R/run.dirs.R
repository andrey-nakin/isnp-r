# returns vector of run directories
run.dirs <-
  function(basedir = ".") {
    dirs <- dir(basedir, full.names = TRUE, recursive = FALSE, pattern="run[0-9]{2}")
    sort(dirs)
    return(dirs)
  }
