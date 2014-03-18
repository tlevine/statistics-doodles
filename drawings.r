baseplot <- function(main, formula = (Petal.Length ~ Petal.Width), data = iris) {
  plot(formula = formula, data = data, type = 'p', bty = 'n',
    main = main, pch = 21, bg = 'grey', col = NULL,
    col.axis = 'grey', col.lab = 'grey', col.main = 'grey', fg = 'grey')
}

#pdf('covariance.pdf')
baseplot('Two iris variables')

baseplot('How')
#points(Petal.Length ~ Petal.Width, data = iris)
#dev.off()
