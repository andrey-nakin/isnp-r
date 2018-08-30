# Convert energy range to string
# range - list of 2 numbers with low and high limits
# sep - separator string
# nounits - when TRUE units are not displayed
# unitname - name of units
# Result - string with textual representation of the given range
range.to.string <- 
  function(range, sep = "-", nounits = F, unitname = "MeV") {
    if (nounits) {
      s <- sprintf("%g%s%g", range[1], sep, range[2])
    } else {
      s <- sprintf("%g%s%g %s", range[1], sep, range[2], unitname)
    }
      
    return (s)
  }
