baseplot <- function(main, formula = (Petal.Length ~ Petal.Width), data = iris) {
  plot(formula = formula, data = data, type = 'p', bty = 'n',
    main = main, pch = 21, bg = 'grey', col = NULL,
    col.axis = 'grey', col.lab = 'grey', col.main = 'grey', fg = 'grey')
}

interjection <- function(main) {
  plot.new()
  text(0.5, 0.5, cex = 2, label = main, font = 2)
}

interjection('What is a statistic?')
interjection('A number that describes\nlots of other numbers')
interjection('Here are some numbers:\n1 2.2 pi 4 5 7 7\n\nWhat are some statistics?')
interjection('min, max,\nmode, median, mean,\n range, variance')
interjection('how many integers,\nwhether the numbers are sorted\n&c.')
interjection('')
interjection('Measuring linear relationships')

baseplot('Two iris variables that move together')

rand <- data.frame(x = rnorm(100), y = rnorm(100))
baseplot('Two air quality variables that move oppositely',
  formula = Ozone ~ Wind, data = airquality) 

baseplot('Normal random noise', formula = y ~ x, data = rand)

interjection('We want a number\nthat describes\nwhether two variables\nmove together.')

#baseplot('How')
#pdf('doodles.pdf', width = 11, height = 8.5)
#dev.off()
