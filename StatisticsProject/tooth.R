# Remove all current data in workspace:
rm(list=ls())
library(ggplot2)

# data(ToothGrowth)
summary(ToothGrowth)
Str(ToothGrowth)
dose <- ToothGrowth$dose
supp <- ToothGrowth$supp
len <- ToothGrowth$len


par(mfrow = c(1,2))
b1<-ggplot(ToothGrowth, aes(x = factor(dose), y = len, fill = factor(dose)))+
  geom_boxplot() + 
  guides(fill=FALSE) + 
  facet_grid(. ~ supp)+
  theme_classic()
b2<-ggplot(ToothGrowth, aes(x = factor(supp), y = len, fill = factor(supp)))+
  geom_boxplot() + 
  guides(fill=FALSE) + 
  facet_grid(. ~ dose)+
  theme_classic()


# Calculate the mean and sd of each dosage and supplement
Dmeans <- aggregate(ToothGrowth$len, by = list(ToothGrowth$dose), FUN = mean)
Dsds <- aggregate(ToothGrowth$len, by = list(ToothGrowth$dose), FUN = sd)
Smeans <- aggregate(ToothGrowth$len, by = list(ToothGrowth$supp), FUN = mean)
Ssds <- aggregate(ToothGrowth$len, by = list(ToothGrowth$supp), FUN = sd)

# Split data by dosages
d0.5 <- subset(ToothGrowth, dose == 0.5)
d1.0 <- subset(ToothGrowth, dose == 1.0)
d2.0 <- subset(ToothGrowth, dose == 2.0)

# Conduct a t test between supplements
test0.5 <- t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = d0.5)
test0.5$p.value; test0.5$conf
test1.0 <- t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = d1.0)
test1.0$p.value; test1.0$conf
test2.0 <- t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = d2.0)
test2.0$p.value; test2.0$conf
