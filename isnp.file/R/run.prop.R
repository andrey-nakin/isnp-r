# read a vector of single properties for every run
run.prop <-
  function(basedir, propname) {
    res <- sapply(
      run.props(basedir),
      function (props) {
        if (is.list(props)) {
          p <- props[[propname]]
          if (is.null(p)) {
            return (NA)
          } else {
            return (p)
          }
        } else {
          return (NA)
        }
      }
    )
    return (res)
  }
