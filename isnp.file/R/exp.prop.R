# read a single experiment property
exp.prop <-
  function(basedir, propname) {
    props <- exp.props(basedir)
    if (is.null(props)) {
      return (NA)
    } else {
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
    }
  }
