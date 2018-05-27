# read a vector of single properties for every run
run.prop <-
  function(basedir, propname) {
    res <- sapply(
      run.props(basedir),
      function (props) {
        if (is.list(props)) {
          return (props[[propname]])
        } else {
          return (NA)
        }
      }
    )
    return (res)
  }
