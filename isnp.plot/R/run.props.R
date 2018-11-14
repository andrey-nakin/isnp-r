# read list of property lists for every run
run.props <-
  function(basedir = ".") {
    dirs <- run.dirs(basedir)

    props <- lapply(
      as.list(dirs),
      function(dir) {
        filename <- paste(dir, "run.properties", sep = "/")
        
        if (file.exists(filename)) {
          p <- properties::read.properties(filename)
        } else {
          p <- NULL
        }
        
        return (p)
      }
    )
    
    return(props)
  }
