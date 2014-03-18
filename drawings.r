baseplot <- function() {
  plot(Petal.Length ~ Petal.Width, data = iris, type = 'n', bty = 'n')
}

#pdf('covariance.pdf')
baseplot()
#dev.off()
