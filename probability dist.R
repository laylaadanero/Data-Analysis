LungCapData <- read.delim(file.choose(), header=T, sep="\t")
attach(LungCapData)

#probability distribution
# p(X=3)

dbinom(x=3, size=20, prob=1/6)

#probability for x of 0, 1, 2, 3
dbinom(x=0:3, size=20, prob=1/6)

#probability of less than x=3 
pbinom(q=3, size=20, prob=1/6, lower.tail=T)

#focusing on known rate of lamda = 7
dpois(x=4, lamda=7)

dpois(x=0:4, lambda=7)

sum(dpois(x=0:4, lambda=7))

# P(X<=4)

ppois(q=4, lambda=7, lower.tail=T)

#Normal distribution, Z scores, and normal probabilities 
# P(X <= 70)
pnorm(q=70, mean=75, sd=5, lower.tail=T)

#P(X >= 85)
pnorm(q=85, mean=75, sd=5, lower.tail=F)

# P(Z >= 1)
pnorm(q=1, mean=0, sd=1, lower.tail=F)

#find Q1
qnorm(p=0.25, mean=75, sd=5, lower.tail =T)

x <- seq(from=55, to=95, by=0.25)

dens <- dnorm(x, mean=75, sd=5)
plot(x, dens, type="l")

#Bivariate Analysis
#one and two sample t test in R 

boxplot(LungCap)

# Ho: mu < 8 
# one-sided 95% confidence interval for mu
#null hypothesis = mean is 8 
t.test(LungCap, mu=8, alternative="less",
       conf.level=0.95)

#for two sided 
t.test(LungCap, mu=8, alternative="two.sided",
       conf.level=0.95)

attributes(TEST)
TEST$p.value
#will give the value of any specific attribute