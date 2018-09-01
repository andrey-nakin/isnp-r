# Generate X values given the range of values intended for logarithmic plots
# x - base values to determine the valid range
# n - max number of values per decimal order of magnitude
# Returns - vector of X values
gen.x.values.log <-
  function(x, n = 100) {
    x.min <- min(x)
    x.max <- max(x)
    
    dec.min <- floor(log10(x.min))
    dec.max <- floor(log10(x.max))
    dec.num <- dec.max - dec.min + 1
    
    v <- head(seq(from = 1, to = 10, length.out = n), n = -1)
    res <- rep(v, dec.num) * (10 ^ rep(dec.min : dec.max, each = n - 1))
    
    return (res[which(res >= x.min & res <= x.max)])
  }
