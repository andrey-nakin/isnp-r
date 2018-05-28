# reads 2d list of data frames
series.tables <- 
  function(filename, basedir = ".", header = T, sep = "\t", quote = "\"", dec = ".", colClasses = NA) {
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
              filename <- paste(dirname, filename, sep = "/")
              if (file.exists(filename)) {
                res <- read.table(filename, header = header, sep = sep, quote = quote, dec = dec, colClasses = colClasses)
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
