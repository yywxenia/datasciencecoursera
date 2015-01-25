
rm(list=ls())
library(knitr)
library(plotrix)

# Set up the experiment values
set.seed(1230)
par(xpd=NA)
lambda <- 0.2
n <- 40

# Generate the random variables for these n values
par(mfrow = c(2,2))

for (no_sim in c(10, 100, 1000, 10000)){
  meanValue <- NULL
  meanSD <- NULL  
  for (i in 1:no_sim){
    values <- rexp(n, lambda)
    means <- mean(values)
    sds <- sd(values)
    meanValue  <- c(meanValue, means)
    meanSD <- c(meanSD, sds)
  }
  
  myhist <- hist(meanValue, freq = TRUE, xlim = c(2, 8), xlab = "Values",
                 main = paste(no_sim, "simulations"),col="light yellow")
}
# no_sim = 10,000 the histogram of probability density
par(mfrow = c(1,1))
myhist <- hist(meanValue, freq = FALSE, xlim = c(2, 8), xlab = "Values", ylim = c(0, .55), 
               breaks = 25, col="light yellow",
               main = paste("Probability Density Function for", no_sim, "Simulations"))
# Total mean and sd of the aggregated samples
avg <- mean(meanValue)
s <- sd(meanValue)

# Average value from the data set
abline(v = avg , col = "red", lwd = 3, lty = 2)

# Expected value of an exponential distribution
abline(v = 5, col = "purple", lwd = 3, lty = 9)

# Theoretical normal distribution for the data set
x <- seq(min(meanValue), max(meanValue), length = 100) 
y <- dnorm(x, mean = avg, sd = s)
curve(dnorm(x, mean = avg, sd = s), 
      col = "gray", lwd = 3, lty = 3, add = TRUE)

legend('topright', c("Expected value", "Actual mean", "Normal distrubution"), 
       lty=1, col=c('purple', 'red', "grey"), bty='n', cex=.75)

sd(meanValue)
qqnorm(meanValue, col = "purple")
qqline(meanValue)

# For no_sim <- 100
no_sim <- 100
meanValue<- NULL; meanSD <- NULL
for (i in 1:no_sim){
  values <- rexp(n, lambda)
  means <- mean(values)
  sds <- sd(values)
  meanValue <- c(meanValue, means)
  meanSD <- c(meanSD, sds)
}

# 95% confidence interval for each simulation
upper <- meanValue +  1.96 * (meanSD/sqrt(n))
lower <- meanValue -  1.96 * (meanSD/sqrt(n))
sum(lower < 5 & 5 < upper)/no_sim * 100
index <- c(1:no_sim)


plot(index, upper, ylim = c(0, 10), type = "n", xlab = "Index", ylab = "Mean", 
     main = "Plot of confidence interval coverage for 100 simulations",col="purple")

segments(index, upper, index, lower, col = "purple", lwd = 3)
text(-8, 5, expression(paste("", mu, "")), cex = 1.5)
ablineclip(h=5, x1 = -2.5, lty = 2, col="red")