# reads 2d list of data frames with neutron kinetic energies
series.neutrons.kinetic <- 
  function(basedir = ".") {
    res <- series.tables("neutrons.txt", basedir, colClasses = c(rep("NULL", 2), "numeric", rep("NULL", 7)))
    return (res)
  }
