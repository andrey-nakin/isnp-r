# read 2d array of a single series properties
series.prop <- 
  function(basedir, propname) {
    dirs <- series.dirs(basedir)
    res <- apply(dirs, c(1, 2), function(seriesdir) {
      filename <- paste(seriesdir, "series.properties", sep = "/")
      if (file.exists(filename)) {
        props <- properties::read.properties(filename)
        p <- props[[propname]]
        if (is.null(p)) {
          return (NA)
        } else {
          return (p)
        }
      } else {
        return (NA)
      }
    })
    
    return (res)
  }
