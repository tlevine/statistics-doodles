attach(iris)
attach(na.omit(airquality))
attach(cars)

baseplot <- function(main, y, x) {
  plot(y ~ x, type = 'p', bty = 'n',
    main = main, pch = 21, bg = 'grey', col = NULL,
    col.axis = 'grey', col.lab = 'grey', col.main = 'grey', fg = 'grey')
}
irisplot <- function(main) {
  baseplot(main, Petal.Length, Petal.Width)
}

meanplot <- function(main) {
  irisplot(main)
  abline(h = mean(Petal.Length), lty = 2)
  abline(v = mean(Petal.Width), lty = 2)
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
baseplot('Two air quality variables that move oppositely', Ozone, Wind) 

baseplot('Normal random noise', rand$y, rand$x)

# Motivation for covariance
interjection('We want a number\nthat describes\nwhether two variables\nmove together.')

irisplot('It should be high for these variables')

rand <- data.frame(x = rnorm(100), y = rnorm(100))
baseplot('It should be low for these variables', Ozone, Wind) 

baseplot('It should be near zero for these variables', rand$y, rand$x)

# Computing covariance
interjection('Covariance')

irisplot('The iris variables')

meanplot('Find the means')

meanplot('Draw a rectangle')
rect(xleft = mean(Petal.Width),
     ybottom = mean(Petal.Length),
     xright = Petal.Width[c(44,72)],
     ytop = Petal.Length[c(44,72)])

irisplot('Draw all the rectangles')
valence <- (Petal.Length - mean(Petal.Length)) *
           (Petal.Width - mean(Petal.Width)) > 0
rect(xleft = mean(Petal.Width),
     ybottom = mean(Petal.Length),
     xright = Petal.Width,
     ytop = Petal.Length,
     col = rgb(1-valence, 0, valence,.1),
     lwd = 0)

irisplot('Why did I color them blue and red?')
valence <- (Petal.Length - mean(Petal.Length)) *
           (Petal.Width - mean(Petal.Width)) > 0
rect(xleft = mean(Petal.Width),
     ybottom = mean(Petal.Length),
     xright = Petal.Width,
     ytop = Petal.Length,
     col = rgb(1-valence, 0, valence,.1),
     lwd = 0)

irisplot('Why did I color them blue and red?')
valence <- (Petal.Length - mean(Petal.Length)) *
           (Petal.Width - mean(Petal.Width)) > 0
rect(xleft = mean(Petal.Width),
     ybottom = mean(Petal.Length),
     xright = Petal.Width,
     ytop = Petal.Length,
     col = rgb(1-valence, 0, valence,.1),
     lwd = 0)
text(x = max(Petal.Width), y = max(Petal.Length), pos = 2, label = 'Evidence of\nmovement together')
text(x = min(Petal.Width), y = min(Petal.Length), pos = 4, label = 'Evidence of\nmovement together')
text(x = max(Petal.Width), y = min(Petal.Length), pos = 2, label = 'Evidence of\nmovement oppositely')
text(x = min(Petal.Width), y = max(Petal.Length), pos = 4, label = 'Evidence of\nmovement oppositely')

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

irisplot('What if we have more red than blue?')
valence <- (Ozone - mean(Ozone)) *
           (Wind - mean(Wind)) > 0
rect(xleft = mean(Wind),
     ybottom = mean(Ozone),
     xright = Wind,
     ytop = Ozone,
     col = rgb(1-valence, 0, valence,.1),
     lwd = 0)

plot(c(-1,1),c(-1,1),main = 'Add the blues together. (This is at a different scale.)',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = .8, xright = -.8, ytop = 1, lwd = 0, col = 'blue')

plot(c(-1,1),c(-1,1),main = 'Add the reds together.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lwd = 0, col = 'red')
rect(xleft = -1, ybottom = .8, xright = -.8, ytop = 1, lwd = 0, col = 'blue')

plot(c(-1,1),c(-1,1),main = 'Subtract the reds.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lwd = 0, col = 'red')
rect(xleft = -1, ybottom = .8, xright = -.8, ytop = 1, lwd = 0, col = 'white')

plot(c(-1,1),c(-1,1),main = 'Divide into as many equal pieces as we have irises (n).',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lwd = 0, col = 'red')
rect(xleft = -1, ybottom = .8, xright = -.8, ytop = 1, lwd = 0, col = 'white')
abline(v = 2 * (-.5 + ((1:nrow(iris))/nrow(iris))))

plot(c(-1,1),c(-1,1),main = 'This red sliver is the covariance.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.3, xright = -.3 + (.5/nrow(iris)), ybottom = -1, ytop = 1,
  col = 'red', lwd = 0)

plot(c(-1,1),c(-1,1),main = 'This red sliver is the covariance.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.3, xright = -.3 + (.5/nrow(iris)), ybottom = -1, ytop = 1,
  col = 'red', lwd = 0)
text(.3, 0, 'But it\'s negative!')

irisplot('What if we have as much red as blue?')
valence <- (rand$y - mean(rand$y)) *
           (rand$x - mean(rand$x)) > 0
rect(xleft = mean(rand$x),
     ybottom = mean(rand$y),
     xright = rand$x,
     ytop = rand$y,
     col = rgb(1-valence, 0, valence,.1),
     lwd = 0)

plot(c(-1,1),c(-1,1),main = 'Add the blues together. (This is at a different scale.)',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.45, ybottom = -.45, xright = .55, ytop = .55, lwd = 0, col = 'blue')

plot(c(-1,1),c(-1,1),main = 'Add the reds together.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.55, ybottom = -.55, xright = .45, ytop = .45, lwd = 0, col = 'red')

plot(c(-1,1),c(-1,1),main = 'Subtract the reds.',
  type = 'n', axes = F, xlab = '', ylab = '')
points(0,0)
text(0,-.5, '(Covariance is zero.)')


# Computing variance
interjection('Variance')
interjection('Variance tells us\nhow spread out\nsome numbers are.')
interjection('(Add examples here.)')
interjection('The variance of a variable is\nthe covariance of the variable\nwith itself.')

irisplot('Our two iris variables from before')
interjection('Let\'s look at just one of them.')
baseplot('The points all fall along the same line.', Petal.Length, Petal.Length)
interjection('Let\'s find the variance of Petal.Length')

baseplot('Draw all the rectangles', Petal.Length, Petal.Length)
rect(xleft = mean(Petal.Length),
     ybottom = mean(Petal.Length),
     xright = Petal.Length,
     ytop = Petal.Length,
     col = rgb(0, 0, 1,.1),
     lwd = 0)

baseplot('Why no red rectangles?', Petal.Length, Petal.Length)
rect(xleft = mean(Petal.Length),
     ybottom = mean(Petal.Length),
     xright = Petal.Length,
     ytop = Petal.Length,
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
.cov <- round(cov(Sepal.Length,Sepal.Width), 2)
irisplot(paste0('Irises (cov = ', .cov, ' cm^2)'))
.cov <- round(cov(speed,dist), 2)
baseplot(paste0('Cars (cov =', .cov, ' mph*ft)'), speed, dist)
par(mfrow = c(1,1))

interjection('Oh noes!')
interjection('Let\'s divide\ncovariance by the variances\nto standardize it.')

a <- sd(Sepal.Length)
b <- sd(Sepal.Width)
ab <- max(a,b)

corbase <- function(main = '', low = -1) {
  plot(c(low * ab,ab),c(low * ab,ab),main = main, type = 'n', axes = F, xlab = '', ylab = '')
  rect(xright = 0, ybottom = 0, xleft = -a, ytop = a, col = 'grey', lwd = 0)
  text(-a/2,a/2,'var(Sepal.Width)')
  rect(xleft = 0, ytop = 0, xright = b, ybottom = -b, col = 'grey', lwd = 0)
  text(b/2,-b/2,'var(Sepal.Length)')
}

corbase()
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lwd = 0)
text(b/2,a/2,'sd(Sepal.Width)*\nsd(Sepal.Length)', col = 'white')

corbase()
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lwd = 0)
text(b/2,a/2,'sd(Sepal.Width)*\nsd(Sepal.Length)', col = 'white')
text(-a/2,-b/2,'cov(Sepal.Width,Sepal.Length)\ncannot be bigger than\nblack rectangle.')

interjection('Why?')

baseplot('Covariance has red rectangles.', Petal.Length, Petal.Length)
rect(xleft = mean(Petal.Length),
     ybottom = mean(Petal.Length),
     xright = Petal.Length,
     ytop = Petal.Length,
     col = rgb(0, 0, 1,.1),
     lwd = 0)

baseplot('Variance doesn\'t have red rectangles.', Petal.Length, Petal.Length)
rect(xleft = mean(Petal.Length),
     ybottom = mean(Petal.Length),
     xright = Petal.Length,
     ytop = Petal.Length,
     col = rgb(0, 0, 1,.1),
     lwd = 0)

corbase()
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lwd = 0)
text(b/2,a/2,'sd(Sepal.Width)*\nsd(Sepal.Length)', col = 'white')
text(-a/2,-b/2,'cov(Sepal.Width,Sepal.Length)\ncannot be bigger than\nblack rectangle.')

corbase('Let\'s zoom in.', low = -.1)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lwd = 0)

r <-  cor(Sepal.Length,Sepal.Width)
corbase('Squish covariance vertically into the rectangle.', low = -.1)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lwd = 0)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a * r, col = 'blue', lwd = 0)
text(b/2,r*a/2,'cov(Sepal.Width, Sepal.Length)', col = 'blue')
text(0, r*a/2, 'r * sd(Sepal.Length')

corbase('Squish covariance horizontally into the rectangle.', low = -.1)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lwd = 0)
rect(xleft = 0, ybottom = 0, xright = b * r, ytop = a, col = 'blue', lwd = 0)
text(r*b/2,a/2,'cov(Sepal.Width, Sepal.Length)', col = 'blue')
text(r*b/2, 0, 'r * sd(Sepal.Width', pos = 2)

corbase('People like to talk about R-squared.', low = -.1)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lwd = 0)
rect(xleft = 0, ybottom = 0, xright = b * r, ytop = a * r, col = 'blue', lwd = 0)
text(r*b/2,a/2,'cov(Sepal.Width, Sepal.Length)', col = 'blue')
text(0, r*a/2, 'r ^ 2 * sd(Sepal.Length')
text(r*b/2, 0, 'r ^ 2 * sd(Sepal.Width', pos = 2)

interjection('What if covariance is negative (red)?')

# XXX
corbase('R is the same, just negative.', low = -.1)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lwd = 0)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a * r, col = 'red', lwd = 0)
text(b/2,r*a/2,'cov(Sepal.Width, Sepal.Length)', col = 'red')
text(0, r*a/2, 'r * sd(Sepal.Length')

# warning here!

corbase()
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lwd = 0)
text(b/2,a/2,'sd(Sepal.Width)*\nsd(Sepal.Length)', col = 'white')
text(-a/2,-b/2,'cov(Sepal.Width,Sepal.Length)\ncannot be bigger than\nblack rectangle.')
text(0,0,'(Add covariance on top in red)')

interjection('Correlation, review')

interjection('')
interjection('If we transform the covariance a bit,\nwe can make predictions.')
interjection('y = b0  + b1 * x')
interjection('Let\'s  find b1.')

corbase()
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lwd = 0)
text(b/2,a/2,'sd(Sepal.Width)*\nsd(Sepal.Length)', col = 'white')
text(-a/2,-b/2,'cov(Sepal.Width,Sepal.Length)\ncannot be bigger than\nblack rectangle.')
text(0,0,'(Add the thing on top)')


}


pdf('doodles.pdf', width = 11, height = 8.5)
slides()
dev.off()
