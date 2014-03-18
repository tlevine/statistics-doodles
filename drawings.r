baseplot <- function(main, formula = (Petal.Length ~ Petal.Width), data = iris) {
  plot(formula = formula, data = data, type = 'p', bty = 'n',
    main = main, pch = 21, bg = 'grey', col = NULL,
    col.axis = 'grey', col.lab = 'grey', col.main = 'grey', fg = 'grey')
}

interjection <- function(main) {
  plot.new()
  text(0.5, 0.5, cex = 2, label = main, font = 2)
}

#pdf('covariance.pdf')
interjection('What is a statistic?')
baseplot('Two iris variables that move together')
rand <- data.frame(x = rnorm(100), y = rnorm(100))
baseplot('Normal random noise', formula = y ~ x, data = rand)
baseplot('')
interjection('We want a number\nthat describes\nwhether two variables\nmove together.')

#baseplot('How')
#points(Petal.Length ~ Petal.Width, data = iris)
#dev.off()
