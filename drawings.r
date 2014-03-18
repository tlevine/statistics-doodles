baseplot <- function(main, formula, data) {
  plot(formula = formula, data = data, type = 'p', bty = 'n',
    main = main, pch = 21, bg = 'grey', col = NULL,
    col.axis = 'grey', col.lab = 'grey', col.main = 'grey', fg = 'grey')
}
irisplot <- function(main) {
  baseplot(main, (Petal.Length ~ Petal.Width), iris)
}

meanplot <- function(main) {
  irisplot(main)
  abline(h = mean(iris$Petal.Length), lty = 2)
  abline(v = mean(iris$Petal.Width), lty = 2)
}

interjection <- function(main) {
  plot.new()
  text(0.5, 0.5, cex = 2, label = main, font = 2)
}


slides <- function() {
# Introduction
interjection('What is a statistic?')
interjection('A number that describes\nlots of other numbers')
interjection('Here are some numbers:\n1 2.2 pi 4 5 7 7\n\nWhat are some statistics?')
interjection('min, max,\nmode, median, mean,\n range, variance')
interjection('how many integers,\nwhether the numbers are sorted\n&c.')
interjection('')
interjection('Measuring linear relationships')

# Three datasets
irisplot('Two iris variables that move together')

rand <- data.frame(x = rnorm(100), y = rnorm(100))
baseplot('Two air quality variables that move oppositely',
  formula = Ozone ~ Wind, data = airquality) 

baseplot('Normal random noise', formula = y ~ x, data = rand)

# Motivation for covariance
interjection('We want a number\nthat describes\nwhether two variables\nmove together.')

irisplot('It should be high for these variables')

rand <- data.frame(x = rnorm(100), y = rnorm(100))
baseplot('It should be low for these variables',
  formula = Ozone ~ Wind, data = airquality) 

baseplot('It should be near zero for these variables', formula = y ~ x, data = rand)

# Computing covariance
interjection('Covariance')

irisplot('The iris variables')

meanplot('Find the means')

meanplot('Draw a rectangle')
rect(xleft = mean(iris$Petal.Width),
     ybottom = mean(iris$Petal.Length),
     xright = iris$Petal.Width[c(44,72)],
     ytop = iris$Petal.Length[c(44,72)])

irisplot('Draw all the rectangles')
valence <- (iris$Petal.Length - mean(iris$Petal.Length)) *
           (iris$Petal.Width - mean(iris$Petal.Width)) > 0
rect(xleft = mean(iris$Petal.Width),
     ybottom = mean(iris$Petal.Length),
     xright = iris$Petal.Width,
     ytop = iris$Petal.Length,
     col = rgb(1-valence, 0, valence,.1),
     lwd = 0)

plot(c(-1,1),c(-1,1),main = 'Add the blues together. (This is at a different scale.)',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lwd = 0, col = 'blue')

plot(c(-1,1),c(-1,1),main = 'Add the reds together.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lwd = 0, col = 'blue')
rect(xleft = .8, ybottom = -1, xright = 1, ytop = -.8, lwd = 0, col = 'red')

plot(c(-1,1),c(-1,1),main = 'Subtract the reds.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lwd = 0, col = 'blue')
rect(xleft = .8, ybottom = -1, xright = 1, ytop = -.8, lwd = 0, col = 'white')

plot(c(-1,1),c(-1,1),main = 'Divide into as many equal pieces as we have irises (n).',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lwd = 0, col = 'blue')
rect(xleft = .8, ybottom = -1, xright = 1, ytop = -.8, lwd = 0, col = 'white')
abline(v = 2 * (-.5 + ((1:nrow(iris))/nrow(iris))))

plot(c(-1,1),c(-1,1),main = 'This blue sliver is the covariance.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.3, xright = -.3 + (.5/nrow(iris)), ybottom = -1, ytop = 1,
  col = 'blue', lwd = 0)

interjection('Let\'s review the previous slides quickly.')
interjection('')

# Computing variance
interjection('Variance')
interjection('Variance tells us\nhow spread out\nsome numbers are.')
interjection('(Add examples here.)')
interjection('The variance of a variable is\nthe covariance of the variable\nwith itself.')

irisplot('Our two iris variables from before')
interjection('Let\'s look at just one of them.')
baseplot('The points all fall along the same line.', (Petal.Length ~ Petal.Length), iris)
interjection('Let\'s find the variance of Petal.Length')

baseplot('Draw all the rectangles', (Petal.Length ~ Petal.Length), iris)
rect(xleft = mean(iris$Petal.Length),
     ybottom = mean(iris$Petal.Length),
     xright = iris$Petal.Length,
     ytop = iris$Petal.Length,
     col = rgb(0, 0, 1,.1),
     lwd = 0)

baseplot('Why no red rectangles?', (Petal.Length ~ Petal.Length), iris)
rect(xleft = mean(iris$Petal.Length),
     ybottom = mean(iris$Petal.Length),
     xright = iris$Petal.Length,
     ytop = iris$Petal.Length,
     col = rgb(0, 0, 1,.1),
     lwd = 0)

plot(c(-1,1),c(-1,1),main = 'Add the blues together. (This is at a different scale.)',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lwd = 0, col = 'blue')

plot(c(-1,1),c(-1,1),main = 'We have no reds to subtract.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lwd = 0, col = 'blue')

plot(c(-1,1),c(-1,1),main = 'Divide into as many equal pieces as we have irises (n).',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lwd = 0, col = 'blue')
abline(v = 2 * (-.5 + ((1:nrow(iris))/nrow(iris))))

plot(c(-1,1),c(-1,1),main = 'This blue sliver is the variance.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.3, xright = -.3 + (.5/nrow(iris)), ybottom = -1, ytop = 1,
  col = 'blue', lwd = 0)

interjection('Let\'s review again.')

interjection('')

# Correlation
interjection('A problem with covariance')
interjection('Covariance has units!\n\n(x-unit times y-unit)')
interjection('Which relationship is stronger\n(more linear)?')

par(mfrow = 1:2)
.cov <- round(cov(iris[c('Sepal.Length','Sepal.Width')])[1,2], 2)
irisplot(paste0('Irises (cov = ', .cov, ' cm^2)'))
.cov <- round(cov(cars[c('speed','dist')])[1,2], 2)
baseplot(paste0('Cars (cov =', .cov, ' mph*ft)'), (speed ~ dist), cars)

par(mfrow = c(1,1))
}


#pdf('doodles.pdf', width = 11, height = 8.5)
slides()
#dev.off()
