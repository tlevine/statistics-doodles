attach(iris)
attach(na.omit(airquality))
attach(cars)

baseplot <- function(main, ...) {
  plot(..., type = 'p', bty = 'n',
    main = main, pch = 21, bg = 'grey', col = NULL,
    col.axis = 'grey', col.lab = 'grey', col.main = 'grey', fg = 'grey')
}
irisplot <- function(main) {
  baseplot(main, Petal.Length ~ Petal.Width)
}

meanplot <- function(main) {
  irisplot(main)
  abline(h = mean(Petal.Length), lty = 2)
  abline(v = mean(Petal.Width), lty = 2)
}

interjection <- function(main) {
  plot.new()
  text(0.5, 0.5, cex = 3, label = main, font = 2)
}

blue <-  rgb(0,0,1,.5)

rand <- data.frame(x = rnorm(100), y = rnorm(100))

# ----------------

statistics <- function() {
# Introduction
interjection('Why we have statistics')

plot(c(-1,1),c(-1,1),xlab='',ylab='',type='n',axes=F,main='Lots of numbers')
text(rep(seq(-1,1,.1),each=21),rep(seq(-1,1,.1),21),round(100*runif(400)))


plot(c(-1,1),c(-1,1),xlab='',ylab='',type='n',axes=F,main='It\'s hard to fit lots of numbers into our brains all at once.')
text(rep(seq(-1,1,.1),each=21),rep(seq(-1,1,.1),21),round(100*runif(400)))

interjection('So we invent numbers\nthat describe\nlots of other numbers\n\n')
interjection('So we invent numbers\nthat describe\nlots of other numbers\n\n(statistics)')

interjection('Here are some numbers:\n1 2.2 pi 4 5 7 7\n\nWhat are some statistics?')
interjection('min, max,\nmode, median, mean,\n range, variance')
interjection('how many integers,\nwhether the numbers are sorted\n&c.')
interjection('')
interjection('Measuring linear relationships')
}

linear.relationships <- function() {
# Three datasets
irisplot('Two iris variables that move together')

baseplot('Two air quality variables that move oppositely', Ozone ~ Wind) 

baseplot('Normal random noise', rand$y ~ rand$x)
}

covariance <- function() {
# Motivation for covariance
interjection('We want a number\nthat describes\nwhether two variables\nmove together.')

irisplot('It should be high for these variables')

rand <- data.frame(x = rnorm(100), y = rnorm(100))
baseplot('It should be low for these variables', Ozone ~ Wind) 

baseplot('It should be near zero for these variables', rand$y ~ rand$x)

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
     lty = 'blank')

irisplot('Why did I color them blue and red?')
valence <- (Petal.Length - mean(Petal.Length)) *
           (Petal.Width - mean(Petal.Width)) > 0
rect(xleft = mean(Petal.Width),
     ybottom = mean(Petal.Length),
     xright = Petal.Width,
     ytop = Petal.Length,
     col = rgb(1-valence, 0, valence,.1),
     lty = 'blank')

irisplot('Why did I color them blue and red?')
valence <- (Petal.Length - mean(Petal.Length)) *
           (Petal.Width - mean(Petal.Width)) > 0
rect(xleft = mean(Petal.Width),
     ybottom = mean(Petal.Length),
     xright = Petal.Width,
     ytop = Petal.Length,
     col = rgb(1-valence, 0, valence,.1),
     lty = 'blank')
text(x = max(Petal.Width), y = max(Petal.Length), pos = 2, label = 'Evidence of\nmovement together')
text(x = min(Petal.Width), y = min(Petal.Length), pos = 4, label = 'Evidence of\nmovement together')
text(x = max(Petal.Width), y = min(Petal.Length), pos = 2, label = 'Evidence of\nmovement oppositely')
text(x = min(Petal.Width), y = max(Petal.Length), pos = 4, label = 'Evidence of\nmovement oppositely')

plot(c(-1,1),c(-1,1),main = 'Add the blues together. (This is at a different scale.)',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lty = 'blank', col = 'blue')

plot(c(-1,1),c(-1,1),main = 'Add the reds together.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lty = 'blank', col = 'blue')
rect(xleft = .8, ybottom = -1, xright = 1, ytop = -.8, lty = 'blank', col = 'red')

plot(c(-1,1),c(-1,1),main = 'Subtract the reds.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lty = 'blank', col = 'blue')
rect(xleft = .8, ybottom = -1, xright = 1, ytop = -.8, lty = 'blank', col = 'white')

plot(c(-1,1),c(-1,1),main = 'Divide into as many equal pieces as we have irises (n).',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lty = 'blank', col = 'blue')
rect(xleft = .8, ybottom = -1, xright = 1, ytop = -.8, lty = 'blank', col = 'white')
abline(v = 2 * (-.5 + ((1:nrow(iris))/nrow(iris))))

plot(c(-1,1),c(-1,1),main = 'This blue sliver is the covariance.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.3, xright = -.3 + (.5/nrow(iris)), ybottom = -1, ytop = 1,
  col = 'blue', lty = 'blank')
}

covariance.notpositive <- function() {
irisplot('That was for this sort of relationship.')
valence <- (Petal.Length - mean(Petal.Length)) *
           (Petal.Width - mean(Petal.Width)) > 0
rect(xleft = mean(Petal.Width),
     ybottom = mean(Petal.Length),
     xright = Petal.Width,
     ytop = Petal.Length,
     col = rgb(1-valence, 0, valence,.1),
     lty = 'blank')

baseplot('What if we have more red than blue?', Ozone ~ Wind)
valence <- (Ozone - mean(Ozone)) *
           (Wind - mean(Wind)) > 0
rect(xleft = mean(Wind),
     ybottom = mean(Ozone),
     xright = Wind,
     ytop = Ozone,
     col = rgb(1-valence, 0, valence,.1),
     lty = 'blank')

plot(c(-1,1),c(-1,1),main = 'Add the blues together. (This is at a different scale.)',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = .8, xright = -.8, ytop = 1, lty = 'blank', col = 'blue')

plot(c(-1,1),c(-1,1),main = 'Add the reds together.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lty = 'blank', col = 'red')
rect(xleft = -1, ybottom = .8, xright = -.8, ytop = 1, lty = 'blank', col = 'blue')

plot(c(-1,1),c(-1,1),main = 'Subtract the reds.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lty = 'blank', col = 'red')
rect(xleft = -1, ybottom = .8, xright = -.8, ytop = 1, lty = 'blank', col = 'white')

plot(c(-1,1),c(-1,1),main = 'Divide into as many equal pieces as we have irises (n).',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lty = 'blank', col = 'red')
rect(xleft = -1, ybottom = .8, xright = -.8, ytop = 1, lty = 'blank', col = 'white')
abline(v = 2 * (-.5 + ((1:nrow(iris))/nrow(iris))))

plot(c(-1,1),c(-1,1),main = 'This red sliver is the covariance.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.3, xright = -.3 + (.5/nrow(iris)), ybottom = -1, ytop = 1,
  col = 'red', lty = 'blank')

plot(c(-1,1),c(-1,1),main = 'This red sliver is the covariance.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.3, xright = -.3 + (.5/nrow(iris)), ybottom = -1, ytop = 1,
  col = 'red', lty = 'blank')
text(.3, 0, 'But it\'s\nnegative!', cex = 3, font = 2)

baseplot('What if we have as much red as blue?', rand$y ~ rand$x)
valence <- (rand$y - mean(rand$y)) *
           (rand$x - mean(rand$x)) > 0
rect(xleft = mean(rand$x),
     ybottom = mean(rand$y),
     xright = rand$x,
     ytop = rand$y,
     col = rgb(1-valence, 0, valence,.1),
     lty = 'blank')

plot(c(-1,1),c(-1,1),main = 'Add the blues together. (This is at a different scale.)',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.45, ybottom = -.45, xright = .55, ytop = .55, lty = 'blank', col = 'blue')

plot(c(-1,1),c(-1,1),main = 'Add the reds together.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.55, ybottom = -.55, xright = .45, ytop = .45, lty = 'blank', col = 'red')

plot(c(-1,1),c(-1,1),main = 'Subtract the reds.',
  type = 'n', axes = F, xlab = '', ylab = '')
points(0,0)
text(0,-.5, '(Covariance is zero.)')
}

variance <- function() {
# Computing variance
interjection('Variance')
interjection('Variance tells us\nhow spread out\nsome numbers are.')
interjection('1, 4, 8, 10\nvs\n4, 4, 5, 6')
interjection('The variance of a variable is\nthe covariance of the variable\nwith itself.')

irisplot('Our two iris variables from before')
interjection('Let\'s look at just one of them.')
baseplot('The points all fall along the same line.', Petal.Length, Petal.Length, asp = 1)
interjection('Let\'s find the variance of Petal.Length')

baseplot('Draw all the rectangles', Petal.Length, Petal.Length, asp = 1)
rect(xleft = mean(Petal.Length),
     ybottom = mean(Petal.Length),
     xright = Petal.Length,
     ytop = Petal.Length,
     col = rgb(0, 0, 1,.1),
     lty = 'blank')

baseplot('Why no red rectangles?', Petal.Length, Petal.Length, asp = 1)
rect(xleft = mean(Petal.Length),
     ybottom = mean(Petal.Length),
     xright = Petal.Length,
     ytop = Petal.Length,
     col = rgb(0, 0, 1,.1),
     lty = 'blank')

plot(c(-1,1),c(-1,1),main = 'Add the blues together. (This is at a different scale.)',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lty = 'blank', col = 'blue')

plot(c(-1,1),c(-1,1),main = 'We have no reds to subtract.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lty = 'blank', col = 'blue')

plot(c(-1,1),c(-1,1),main = 'Divide into as many equal pieces as we have irises (n).',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -1, ybottom = -1, xright = 1, ytop = 1, lty = 'blank', col = 'blue')
abline(v = 2 * (-.5 + ((1:nrow(iris))/nrow(iris))))

plot(c(-1,1),c(-1,1),main = 'This blue sliver is the variance.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.3, xright = -.3 + (.5/nrow(iris)), ybottom = -1, ytop = 1,
  col = 'blue', lty = 'blank')
}

correlation <- function() {
# Correlation
interjection('A problem with covariance')
interjection('Covariance has units!\n\n(x-unit times y-unit)')
interjection('Which relationship is stronger\n(more linear)?')

par(mfrow = 1:2)
.cov <- round(cov(Petal.Length,Petal.Width), 2)
irisplot(paste0('Irises (cov = ', .cov, ' cm^2)'))
.cov <- round(cov(speed,dist), 2)
baseplot(paste0('Cars (cov =', .cov, ' mph*ft)'), speed ~ dist)
par(mfrow = c(1,1))

interjection('Oh noes!')
interjection('We can divide\nthe covariance\nby the variances\nto standardize it.')

irisplot('We\'re using these data again.')

a <- sd(Petal.Length)
b <- sd(Petal.Width)
ab <- max(a,b)
corbase <- function(main = '', low = -1) {
  plot(c(low * ab,ab),c(low * ab,ab),main = main, type = 'n', axes = F, xlab = '', ylab = '', asp = 1)
  rect(xright = 0, ybottom = 0, xleft = -a, ytop = a, col = 'grey', lty = 'blank')
  text(-a/2,a/2,'var(Petal.Width)')
  rect(xleft = 0, ytop = 0, xright = b, ybottom = -b, col = 'grey', lty = 'blank')
  text(b/2,-b/2,'var(Petal.Length)')
}

corbase()
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
text(b/2,a/2,'sd(Petal.Width)*\nsd(Petal.Length)', col = 'white')

corbase()
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
text(b/2,a/2,'sd(Petal.Width)*\nsd(Petal.Length)', col = 'white')
text(-a/2,-b/2,'The black rectangle is\nlike an average variance.')

corbase()
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
text(b/2,a/2,'sd(Petal.Width)*\nsd(Petal.Length)', col = 'white')
text(-a/2,-b/2,'cov(Petal.Width,Petal.Length)\ncannot be bigger than\nblack rectangle.')

interjection('Why?')

irisplot('Covariance has red rectangles.')
valence <- (Petal.Length - mean(Petal.Length)) *
           (Petal.Width - mean(Petal.Width)) > 0
rect(xleft = mean(Petal.Width),
     ybottom = mean(Petal.Length),
     xright = Petal.Width,
     ytop = Petal.Length,
     col = rgb(1-valence, 0, valence,.1),
     lty = 'blank')

baseplot('Variance doesn\'t have red rectangles.', Petal.Length, Petal.Length, asp = 1)
rect(xleft = mean(Petal.Length),
     ybottom = mean(Petal.Length),
     xright = Petal.Length,
     ytop = Petal.Length,
     col = rgb(0, 0, 1,.1),
     lty = 'blank')

corbase()
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
text(b/2,a/2,'sd(Petal.Width)*\nsd(Petal.Length)', col = 'white')
text(-a/2,-b/2,'cov(Petal.Width,Petal.Length)\ncannot be bigger than\nblack rectangle.')

corbase('Let\'s zoom in.', low = -.05)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
text(b/2,a/2,'sd(Petal.Width)*\nsd(Petal.Length)', col = 'white')

a <- sd(Petal.Length)
b <- sd(Petal.Width)
ab <- max(a,b)
corbase <- function(main = '', low = -1) {
  plot(c(low * ab,ab),c(low * ab,ab),main = main, type = 'n', axes = F, xlab = '', ylab = '', asp = 1)
  rect(xright = 0, ybottom = 0, xleft = -a, ytop = a, col = 'grey', lty = 'blank')
  text(-a/2,a/2,'var(Petal.Width)')
  rect(xleft = 0, ytop = 0, xright = b, ybottom = -b, col = 'grey', lty = 'blank')
  text(b/2,-b/2,'var(Petal.Length)')
}

r <-  cor(Petal.Length,Petal.Width)
corbase('Squish covariance vertically into the rectangle.', low = -.05)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
rect(xleft = 0, ybottom = 0, xright = b, ytop = a * r, col = 'blue', lty = 'blank')

interjection('Correlation (R)\nis the ratio of\nthe small rectangle\nto the big rectangle.')

corbase('Squish covariance vertically into the rectangle.', low = -.05)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
rect(xleft = 0, ybottom = 0, xright = b, ytop = a * r, col = 'blue', lty = 'blank')
text(b/2,r*a/2,'cov(Petal.Width,\nPetal.Length)', col = 'white', font = 2)
text(0, r*a/2, 'R * sd(Petal.Length)', col = 'blue', font = 2, srt = 90, adj = c(0.5,-1))

corbase('Squish covariance horizontally into the rectangle.', low = -.05)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', font = 2, lty = 'blank')
rect(xleft = 0, ybottom = 0, xright = b * r, ytop = a, col = 'blue', lty = 'blank')
text(r*b/2,a/2,'cov(Petal.Width,\nPetal.Length)', col = 'white', font = 2)
text(b/2, 0, 'R * sd(Petal.Width', adj = c(0.5,1.5), col = 'blue', font = 2)

interjection('People like to\ntalk about R-squared.')

corbase('Intersect the two squished covariance rectangles.', low = -.05)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
rect(xleft = 0, ybottom = 0, xright = b * r, ytop = a * r, col = 'purple', lty = 'blank')
text(r*b/2,a/2,'cov(Petal.Width, Petal.Length)', col = 'purple')
text(0, r*a/2, 'R ^ 2 * sd(Petal.Length)',srt = 90, pos = 2, col = 'purple', font = 2)
text(r*b/2, 0, 'R ^ 2 * sd(Petal.Width)', adj = c(0.5,1.5), col = 'purple', font = 2)

interjection('That was for very\npositive (blue) covariances.')
interjection('What if covariance is negative (red)?')

irisplot('We were just using these data.')
baseplot('What if we had these data?', Ozone ~ Wind) 
baseplot('What if we had these data?', Ozone ~ Wind) 
valence <- (Ozone - mean(Ozone)) *
           (Wind - mean(Wind)) > 0
rect(xleft = mean(Wind),
     ybottom = mean(Ozone),
     xright = Wind,
     ytop = Ozone,
     col = rgb(1-valence, 0, valence,.1),
     lty = 'blank')

a <- sd(Ozone)
b <- sd(Wind)
ab <- max(a,b)
corbase <- function(main = '', low = -1) {
  plot(c(low * ab,ab),c(low * ab,ab),main = main, type = 'n', axes = F, xlab = '', ylab = '', asp = 1)
  rect(xright = 0, ybottom = 0, xleft = -a, ytop = a, col = 'grey', lty = 'blank')
  text(-a/2,a/2,'var(Wind)')
  rect(xleft = 0, ytop = 0, xright = b, ybottom = -b, col = 'grey', lty = 'blank')
  text(b/2,-b/2,'var(Ozone)')
}

r <-  -cor(Ozone,Wind)
corbase('R is the same, just negative.', low = -.05)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
rect(xleft = 0, ybottom = 0, xright = b, ytop = a * r, col = 'red', lty = 'blank')

corbase('R-squared is the same, and it is always positive.', low = -.05)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
rect(xleft = 0, ybottom = 0, xright = b * r, ytop = a * r, col = 'purple', lty = 'blank')

corbase('Zoom back out.')
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
rect(xleft = 0, ybottom = 0, xright = b, ytop = a * r, col = 'red', lty = 'blank')
}

comparing.correlations <- function() {
  interjection('And now we can compare correlations.')
  
}

least.squares.regression <- function() {
interjection('If we transform the covariance a bit,\nwe can also make predictions.')
interjection('Let\'s use x to predict y.')
interjection('y = b0  + b1 * x')
interjection('Let\'s invent b1.')
interjection('')
interjection('What values should it have?')
interjection('If covariance is high\nand x is high,\ny should be high.\n\n(b1 is very positive.)')
interjection('If covariance is high\nand x is low,\ny should be low.\n\n(b1 is very negative.)')
interjection('If covariance is low,\nwe have no idea what y is.\n\n(b1 is around zero.)')
interjection('')
interjection('Let\'s think about units again.')

plot(c(-1,1),c(-1,1),main = 'Covariance is an area; its unit is the product of the x and y units.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.3, xright = -.3 + (.5/nrow(iris)), ybottom = -1, ytop = 1,
  col = 'blue', lty = 'blank')

plot(c(-1,1),c(-1,1),main = 'Variance is a special covariance; its unit is the square of the x unit.',
  type = 'n', axes = F, xlab = '', ylab = '')
rect(xleft = -.3, xright = -.3 + (.5/nrow(iris)), ybottom = -1, ytop = 1,
  col = 'blue', lty = 'blank')

a <- sd(Ozone)
b <- sd(Wind)
ab <- max(a,b)
corbase <- function(main = '', low = -1) {
  plot(c(low * ab,ab),c(low * ab,ab),main = main, type = 'n', axes = F, xlab = '', ylab = '', asp = 1)
  rect(xright = 0, ybottom = 0, xleft = -a, ytop = a, col = 'grey', lty = 'blank')
  text(-a/2,a/2,'var(Wind)')
  rect(xleft = 0, ytop = 0, xright = b, ybottom = -b, col = 'grey', lty = 'blank')
  text(b/2,-b/2,'var(Ozone)')
}

r <-  cor(Petal.Width,Petal.Length)
corbase('Correlation is a ratio of areas with the same units.', low = -.05)
rect(xleft = 0, ybottom = 0, xright = b, ytop = a, col = 'black', lty = 'blank')
rect(xleft = 0, ybottom = 0, xright = b, ytop = a * r, col = 'blue', lty = 'blank')

interjection('The unit of b1 must be y-unit/x-unit.')

corbase('Our covariance picture')
rect(xleft = 0, ybottom = 0, xright = b, ytop = a * r, col = 'blue', lty = 'blank')
text(b,r*a/2,'cov(Petal.Width,\nPetal.Length)', col = 'white', font = 2, pos = 2)
text(0, a*2/3, 'R * sd(Petal.Length)', pos = 2, col = 'black', font = 1, srt = 90)

.adj <- cov(Petal.Width,Petal.Length)/sd(Petal.Length)
corbase('Lay the covariance over one of the variances instead.')
rect(xleft = -a, xright = 0, ybottom = a * (1 - .adj), ytop = a, col = 'blue', lty = 'blank')
text(-a/2,a/2,'cov(Petal.Width, Petal.Length)', col = 'white', font = 2)

corbase('Petal.Width = b0 + b1 * Petal.Length')
rect(xleft = -a, xright = 0, ybottom = a * (1 - .adj), ytop = a, col = 'blue', lty = 'blank')
text(-a/2,a/2,'cov(Petal.Width, Petal.Length)', col = 'white', font = 2)
text(0,a/2,'b1 * sd(Petal.Length)', col = 'blue', pos = 4)

.adj <- cov(Petal.Width,Petal.Length)/sd(Petal.Width)
corbase('Lay the covariance over the other variance.')
rect(xleft = 0, xright = b, ybottom = -b, ytop = -b  * (1 - .adj), col = blue, lty = 'blank')
text(0,0,'cov(Petal.Width,\nPetal.Length)', col = 'white', font = 2, pos = 4)

corbase('Petal.Length = b0 + b1 * Petal.Width')
rect(xleft = 0, xright = b, ybottom = -b, ytop = -b  * (1 - .adj), col = blue, lty = 'blank')
text(a/2,0,'b1 * sd(Petal.Length)', col = 'blue', font = 2, srt = -90)
}


thoughtful.thoughts <- function() {
  interjection('A statistic is a number\nthat describes a lot\nof other numbers.')
  interjection('Covariance describes\nthe strength of\nlinear relationships.')
  interjection('Variance describes\nhow spread-out\nsome numbers are.')
  interjection('Correlation is a\nstandardized version\nof covariance.')
  interjection('(Beta coefficients for)\nleast-squares regression\npredict one variable\nbased on another.')
  interjection('')
  interjection('You can pretty much always draw math.')
}

slides <- function() {
  interjection('Statistics with doodles\nThomas Levine\nthomaslevine.com')
  statistics()
  linear.relationships()
  covariance()
  covariance.notpositive()
  interjection('Let\'s review the previous slides quickly.')
  linear.relationships()
  covariance()
  covariance.notpositive()
  interjection('')
  variance()
  interjection('')
  correlation()
  interjection('Remember how this fits in.')
  covariance()
  correlation()
  interjection('')
  least.squares.regression()
  interjection('Let\'s go over that again.')
  linear.relationships()
  covariance()
  covariance.notpositive()
  variance()
  correlation()
  least.squares.regression()
  interjection('')
  thoughtful.thoughts()
}

pdf('doodles.pdf', width = 11, height = 8.5)
slides()
dev.off()
