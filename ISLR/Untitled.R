library(ISLR)

pairs(Auto)

cor(Auto[, -9])
lm.fit=lm(mpg ~.-name, Auto)
summary(lm.fit)


lm.fit.interaction=lm(mpg ~.-name-acceleration+weight:horsepower+horsepower:cylinders+horsepower:displacement+displacement:cylinders, Auto)
summary(lm.fit.interaction)

set.seed(3)
y <- rnorm(100)
mean(y)
sd(y)
x

library(dplyr)
tbl_df(iris)
View(iris)

iris %>% group_by(Species) %>% 
         summarise(avg = mean(Sepal.Width)) %>% 
         arrange(avg)

iris %>% filter(Sepal.Length > 7)

iris %>% slice(10:15)

iris %>% select(Sepal.Length, Petal.Length, Species)
iris %>% nrow()
iris %>% count(Species, wt=Sepal.Length)

iris %>% group_by(Species) %>% summarize(med = median(Sepal.Length))

x <- rnorm(100)
y <- rnorm(100)
plot(x,y)
™
x <- seq(-pi, pi, length= 50)
y <- x
f <- outer(x, y, function(x,y) cos(y) / (1 + x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
summary(iris)

library(MASS)
library(ISLR2)

lm.fit <- Boston %>% lm(medv ~ lstat, data = .)

lm.fit %>% predict(data.frame(lstat = c(5, 10, 15)), interval = 'confidence' )
attach(Boston)
plot(lstat, medv)
abline(lm.fit, lwd = 3, col = "red")

par(mfrow = c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

-------------

lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit <- lm(medv ~ . - age, data = Boston)
summary(lm.fit)

lm.fit <- lm(medv ~ lstat)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
anova(lm.fit, lm.fit2)

par(mfrow = c(2, 2))
plot(lm.fit2)
plot(lm.fit)

lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

library(jtools)
summ(lm.fit)

summary(lm(medv ~ lstat * age, data = Boston))
-----------
  
head(Carseats)

library(ISLR2)
Yesnames(Smarket)

dim(Smarket)
summary(Smarket)

pairs(Smarket)
cor(Smarket[, -9])
attach(Smarket)
plot(Volume)

glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef

glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]

contrasts(Direction)

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs>.5] <- "Up"
head(glm.pred)

table(glm.pred, Direction)
mean(glm.pred == Direction)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5 + Volume, data = Smarket, 
               family = binomial, subset = train)

glm.probs <- glm.fit %>% predict(Smarket.2005, type = "response")
head(glm.probs)
glm.pred <- rep("Down", 252)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket, 
                family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", dim(Smarket.2005)[1])
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

predict(glm.fits, newdata = data.frame(Lag1 = 1.2, Lag2 = 1.1), type = "response")

########
library(ggplot2)
library(manipulate)
k <- 1000
xvals <- seq(-5, 5, length = k)

myplot <- function(df) {
  d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
                  x = xvals, 
                  dist = factor(rep(c("Normal", "T"), c(k,k))
                                )
                  )
  g <- ggplot(d, aes(x = x, y = y))
  g <- g + geom_line(size = 2, aes(colour = dist))
  g
}
manipulate(myplot(mu), mu = slider(1, 10, step =1))


pvals <- seq(.5, .99, by = .01)

myplot2 <- function(df) {
  d <- data.frame(n = qnorm(pvals), t = qt(pvals, df),
                  p = pvals)
  #plot_ly(data=d, x = n, y = t)
  g <- ggplot(d, aes(x = n, y = t))
  g <- g + geom_line(col="black")
  g
}

manipulate(myplot2(df), df = slider(1, 50, step =1))

####
data("sleep")
g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
mn <- mean(difference); s <- sd(difference); n <- 10

mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)

t.test(g2, g1, paired = T)

#
mu0 = 30
mua = 32
sigma = 4
n = 16
alpha = .05
z= qnorm(1-alpha)
power <- pnorm(mu0 + z*sigma/sqrt(n), mean = mua, sd = sigma/sqrt(n), lower.tail = F)
sd = sigma/sqrt(n)
power.t.test(n=n, delta = (mua-mu0)/sigma, sd=sd, type = "one.sample", alt = "one.sided")$power


library(manipulate)
mu0 = 30
myplot <- function(sigma, mua, n, alpha) {
  g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
  g = g + stat_function(fun = dnorm, geom = "line", args = list(mean = mu0,
                                                                sd = sigma/sqrt(n)), size = 2, col = "red")
  g = g + stat_function(fun = dnorm, geom = "line", args = list(mean = mua,
                                                                sd = sigma/sqrt(n)), size = 2, col = "blue")
  xitc = mu0 + qnorm(1 - alpha) * sigma/sqrt(n)
  g = g + geom_vline(xintercept = xitc, size = 3)
  g
}
manipulate(myplot(sigma, mua, n, alpha), sigma = slider(1, 10, step = 1, initial = 4),
           mua = slider(30, 35, step = 1, initial = 32), n = slider(1, 50, step = 1,
                                                                    initial = 16), alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05))
#
set.seed(1010093)

pValues <- rep(NA, 1000)
for (i in 1:1000) {
  y <- rnorm(20)
  x <- rnorm(20)
  pValues[i] <- summary(lm(y~x))$coeff[2,4]
}

sum(pValues <.05)

# Controls Family-wise error rate (FWER)
sum(p.adjust(pValues, method = "bonferroni") < .05)

set.seed(1010093)
pValues <- rep(NA, 1000)
for (i in 1:1000) {
  x <- rnorm(20)
  # First 500 beta=0, last 500 beta=2
  if (i <= 500) {
    y <- rnorm(20)
  } else {
    y <- rnorm(20, mean = 2 * x)
  }
  pValues[i] <- summary(lm(y ~ x))$coeff[2, 4]
}
trueStatus <- rep(c("zero", "not zero"), each = 500)
table(pValues < 0.05, trueStatus)

# Control FWER
table(p.adjust(pValues, method = "bonferroni") < .05, trueStatus)

# Control FDR
table(p.adjust(pValues, method = "BH") < 0.05, trueStatus)
p.adjust(pValues, method = "BH") < 0.05

par(mfrow = c(1, 2))
plot(pValues, p.adjust(pValues, method = "bonferroni"), pch = 19)
plot(pValues, p.adjust(pValues, method = "BH"), pch = 19)

# jacknife
library(UsingR)
data("father.son")
x <- father.son$sheight
n <- length(x)
theta <- median(x)
jk <- sapply(1:n, function(i) median(x[-i]))
thetaBar <- mean(jk)
biasEst <- (n-1)*(thetaBar-theta)
seEst <- sqrt((n-1)*mean((jk-thetaBar)^2))
c(biasEst, seEst)

library(bootstrap)
temp <- jackknife(x, median)
c(temp$jack.bias, temp$jack.se)

# bootstrap
B <- 1000
resamples <- matrix(sample(x, n*B, replace = T), B, n)
medians <- apply(resamples,1, median)
sd(medians)

quantile(medians, c(0.025, 0.975))

plot_ly(data = as.data.frame( medians), x= ~medians, type = "histogram")

data("InsectSprays")
plot_ly(InsectSprays, x = ~spray, y = ~count, name = "Insect sprays data", type = "box")

subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"), ]
y <- subdata$count
group <- as.character(subdata$spray)

testStat <- function(w, g) mean(w[g =="B"] - mean(w[g == "C"])) 
observedStat <- testStat(y, group)

permutations <- sapply(1:10000, function(i) testStat(y, sample(group)))
observedStat

mean(permutations > observedStat)
as.data.frame( permutations) %>% plot_ly( x= ~permutations, type = "histogram", nbinsx = 30, bargroupgap=0.1)


