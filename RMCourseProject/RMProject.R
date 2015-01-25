##==========================================================================
#Author: Yiwei Yan

##--------------------------------------------------------------------------
rm(list=ls())
data(mtcars)
str(mtcars)

#Convert qualitative data
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual")) ## am   Transmission (0 = automatic, 1 = manual
mtcars$gear <- factor(mtcars$gear)

##--------------------------------------------------------------------------
# Use all variables in the dataset as predictors in the first model (Model1)
Model1 = lm(mpg ~ ., data = mtcars)
s1<-summary(Model1)$coef
Model2 <- step(Model1, trace = 0, direction="both")
summary(Model2)
ModelB<-lm(mpg~am, data= mtcars)

##--------------------------------------------------------------------------
# Compare Model2 with ModelB
anova(ModelB,Model2)
# draw a boxplot figure to show what results of ModelB looks like
Figure1<- boxplot(mpg ~ am, data = mtcars,
xlab = "Transmission Types", 
ylab = "MPG",
main = "MPG versus Transmission", col = c("light blue","dark blue"))

par(mfrow=c(2,2))
plot(Model2, col = "Blue", main = "Residual Plots")
tail(sort(hatvalues(Model2)),3)
tail(sort(dfbetas(Model2)[,6]))
# t test
t.test(mpg ~ am, data = mtcars)

##--------------------------------------------------------------------------
## Graph 1: Pairwise plot of mtcars
# In order to get a greater intuition of what variables may be of interest
pairs(mtcars, panel = panel.smooth, main = "Pairwise plot of mtcars", col="blue")
