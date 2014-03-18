baseplot <- function(main) {
  plot(Petal.Length ~ Petal.Width, data = iris, type = 'p', bty = 'n',
    main = main, pch = 21, bg = 'grey', col = NULL,
    col.axis = 'grey', col.lab = 'grey', col.main = 'grey', fg = 'grey')
}

#pdf('covariance.pdf')
baseplot('Two iris variables')

#baseplot('')
#points(Petal.Length ~ Petal.Width, data = iris)
#dev.off()
