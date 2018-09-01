# Calculates root-mean-square deviation
# e - vector of errors
# Returns: root-mean-square deviation
calc.rmsd <- 
  function(e) {
    res <- sqrt(mean(e^2))
    return (res)
  }
