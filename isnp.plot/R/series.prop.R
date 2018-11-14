# read 2d array of a single series properties
series.prop <- 
  function(basedir, propname) {
    dirs <- series.dirs(basedir)
    res <- apply(dirs, c(1, 2), function(seriesdir) {
      filename <- paste(seriesdir, "series.properties", sep = "/")
      if (file.exists(filename)) {
        props <- properties::read.properties(filename)
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
      } else {
        return (NA)
      }
    })
    
    return (res)
  }
