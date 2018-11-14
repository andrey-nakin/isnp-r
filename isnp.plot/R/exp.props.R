# read experiment property list
exp.props <-
  function(basedir = ".") {
    filename <- paste(basedir, "exp.properties", sep = "/")
    
    if (file.exists(filename)) {
      p <- properties::read.properties(filename)
    } else {
      p <- NULL
    }
    
    return (p)
  }
