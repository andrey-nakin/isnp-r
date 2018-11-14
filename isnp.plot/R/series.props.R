# reads 2d list of series properties
series.props <- 
  function(basedir = ".") {
    dirs <- series.dirs(basedir)
    
    res <- lapply(
      seq(from = 1, to = dim(dirs)[1], by = 1),
      function(runNo) {
        res <- res <- lapply(
          seq(from = 1, to = dim(dirs)[2], by = 1),
          function(seriesNo) {
            dirname <- dirs[runNo, seriesNo]
            if (is.na(dirname)) {
              return (NULL)
            } else {
              filename <- paste(dirname, "series.properties", sep = "/")
              if (file.exists(filename)) {
                res <- properties::read.properties(filename)
                return (res)
              } else {
                return (NULL)
              }
            }
          }
        )
        
        return (res)
      }
    )
    
    return (res)
  }
