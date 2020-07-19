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


# examine full and reduced models using partial F test
# full model --> μ(lungcap) = bo + b1(age) + b2(gender) + b3(smoke) + b4(height)
# reduced model --> μ(lungcap) = bo + b1(age) + b2(gender) + b3(smoke) 
# reduced model is nested within full model

# check if removing b4(height) results in statistical increase in sum of squared errors/residual sum of squares
# will removing height result in increased error in model and decrease in predictive power

Full.Model <- lm(LungCap ~ Age + I(Age^2))

Reduced.Model <- lm(LungCap ~ Age)

# get summary for both models to examine R sqaured and residual standard error

summary(Full.Model)

summary(Reduced.Model)


#partial f-test
anova(Reduced.Model, Full.Model) 

# p value shows we failed to reject null hypothesis

model1 <- lm(LungCap ~ Age + Gender + Smoke + Height)
model2 <- lm(LungCap ~ Age + Gender + Smoke)

#partial test 
anova(model2, model1)


