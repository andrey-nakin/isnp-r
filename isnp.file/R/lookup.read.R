library(properties)

# lookp data files through the runs/series an read them
lookup.read <- 
  function(filename, basedir, header, sep, quote, dec, col.names) {
  res <- array(dim = c(length(PLS), length(DS)))
  
  lookup(filename = filename, basedir = basedir, 
                      header = header, sep = sep, quote = quote, dec = dec, col.names = col.names, closure = function(df) {
                        
                      }
  )
 }
