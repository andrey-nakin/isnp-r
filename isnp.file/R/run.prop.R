# read a vector of single properties for every run
run.prop <-
  function(basedir, propname) {
    res <- sapply(
      run.props(basedir),
      function (props) {
        if (is.list(props)) {
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
      }
    )
    return (res)
  }
