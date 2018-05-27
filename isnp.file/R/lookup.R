# lookp data files through the runs/series an read them
lookup <-
  function(
    filename, basedir = ".", closure, closureparam,
    header = TRUE, sep = "\t", quote = "\"'", dec = ".", col.names
  )
  {
    for (i in seq(from = 1, to = 1, by = 1)) {
      my.dir.run = paste(basedir, sprintf("run%02d", i - 1), sep="/")
      for (j in seq(from = 1, to = 10, by = 1)) {
        my.dir.series = paste(my.dir.run, sprintf("series%03d", j - 1), sep="/")
        my.df <- read.table(
          file = paste(my.dir.series, filename, sep="/"), 
          header = header, 
          sep = sep,
          quote = quote,
          dec = dec,
          col.names = col.names
        )
        
        closure(my.df, param = closureparam, runNo = i, seriesNo = j);
      }
    }
  }
