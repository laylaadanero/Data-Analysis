#exploring multivariate linear regression 

LungCapData<- read.delim(file.choose(),sep="\t",header = T)
attach(LungCapData)

#fit linear model to examine plot

model_1 <- lm(LungCap ~ Age + Height)

summary(model_1)
#result R squared = 0.843
# 84% of variation in Lung Capacity can be explained by our model

cor(Age, Height, method="pearson")

#confidence intervals for model coefficients
confint(model_1,conf.level=0.95)

#fit linear model for all X variables

model_2 <- lm(LungCap ~ Age + Height + Smoke + Gender + Caesarean)

summary(model_2)

plot(model_2)  # view residual plots to examine linearity

#changing height into categorical variable
CatHeight <- cut(Height, breaks=c(0,50,55,60,65,70,100),
                 labels=c("A", "B", "C", "D","E","F"),
                 right=FALSE)