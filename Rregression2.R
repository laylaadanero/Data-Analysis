#logistic regression

library(readr)
library(dplyr)

#read.delim(file.choose(), header=T,sep="\t")

install.packages("mlbench")
library(mlbench)

library(help = "mlbench")

# Boston Housing Data
data(BostonHousing)
dim(BostonHousing)
head(BostonHousing)

attach(BostonHousing)

#variables of dataset 

#CRIM - per capita crime rate by town
#ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
#INDUS - proportion of non-retail business acres per town.
#CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
#NOX - nitric oxides concentration (parts per 10 million)
#RM - average number of rooms per dwelling
#AGE - proportion of owner-occupied units built prior to 1940
#DIS - weighted distances to five Boston employment centres
#RAD - index of accessibility to radial highways
#TAX - full-value property-tax rate per $10,000
#PTRATIO - pupil-teacher ratio by town
#B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#LSTAT - % lower status of the population
#MEDV - Median value of owner-occupied homes in $1000's

#lets check if crime rate is dependent on MEDV and or other variables
model1 <- lm(crim ~ b)

# null hypothesis is that the two crime rate and house value does not affect each other
# alternative hypothesis is that the two do 

sumamry(model1)
# p value was 2.2e-16 and F statistic 89.49 on 1 and 504 DF 
#residual error was 7.934 on 504 degrees of freedom and multiple R squared= 0.1508

cor(crim, b, method="pearson")
#negative pearson coeff means two variables are going in opposite directions so no linearity here
par(mfrow=c(2,2))
plot(model1)


#checking to see if we have to data clean or sort through data