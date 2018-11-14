hist.freq <- function(x, breaks = "Sturges", plot = T, xlab = NULL, ylab = NULL, ylim = NULL) {
  h <- hist(x, breaks = breaks, plot = plot)
  n <- length(x)
  bp <- barplot(h$counts / n, col = "white", space = 0, ylim = ylim, xpd = F)
  axis(1, at = bp, labels = h$mids)
  title(ylab = ylab, xlab = xlab)
  return (h)
}