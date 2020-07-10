
library(readr)
library(ggplot2)
library(dplyr)

LungCapData <- read.delim(file.choose(),header=T,sep="\t")
attach(LungCapData)

#linear model age and lung capacity

#examine plot without ggplot

plot(Age, LungCap, main="Simple scatterplot")

#pearsons correlation
cor(Age,LungCap)

mod <- lm(LungCap ~ Age)
summary(mod)

#examine attributes stored
attributes(mod)


plot(Age, LungCap, main="Simple scatterplot",
     xlab= "Age", ylab= "Lung Capacity",
     las=1)

abline(mod, col=2, lwd=2.5)

#check confidence intervals for coefficients
confint(mod)

confint(mod, level = 0.99)

#examine anova table for regression
anova_table <- anova(mod)

#assumptions to check include that:
#the y values are independent
#the y values can be expressed as a linear function of the X variable
#variation of observations around regression line is constant(homoscedasticity)
#for given value of X, Y values are normally distributed


#view residual plots and examine linearity
plot(mod)

#produce all four plots on one graph 
par(mfrow=c(2,2))
plot(mod)

#examine how non constant variance shows up in residual plot
par(mfrow=c(1,1))
plot(x,y)
#second regression model
mod2 <- lm(y~x)
abline(mod2,  col=2)




