library(readr)
library(ggplot2)
library(dplyr)

#linear regression tutorial 

LungCapData <- read.delim(file.choose(), header = T, sep="\t")
attach(LungCapData)

#frequency table for gender
table(Gender)
#store in object
count <- table(Gender)
#relative frequency 
percent <-table(Gender)/725

#produce bar plot for count and relative frequency
percentbar <- 
  barplot(percent,main="Relative Frequency",xlab="%",
          ylab="Gender",las=1,names.arg=c("Female","Male"),
          horiz=TRUE)
#to get bars horizontally set horiz to TRUE

#add pie chart and add a box around it 
pie(count,main="Relative Frequency of Lungs")
box()


#Boxplots
boxplot(LungCap, mains="Boxplot", 
        ylab="Lung Capacity", ylim=c(0,16),
        las=1)
#quartiles
quantile(LungCap,probs =c(0,0.25,0.5,0.75,1))

#produces box plot of lung capacity for 
#each group formed by variable Gender
boxplot(LungCap ~ Gender, main="Boxplot by Gender")




#histogram of Lung Capcity Density
LungCapHist <- hist(LungCap, prob=T, ylim=c(0,0.2), breaks=seq(from=0, to=16, by=2), 
                    xlab = "Lung Capacity", las=1, col=c(7))

lines(density(LungCap), lwd=2, col=c(2))

#stemandleafplots 

stem(femaleLungCap,scale=1)

#stacked and group bar charts and mosaic plots
#need contingency table for this
table(Smoke,Gender)
barplot(Table1,beside=TRUE, legend.text = T, las=1)
#Can change the table for conditional probabilites if needed

#mosaic plot to examine the relationship between 2 categorical variables
mosaicplot(Table1)

#scatter plots examine 2 numeric variables
class(Height)
summary(Height)

#need to calc pearsons correlation to examine strength of linear relationship
#between the 2 numeric variables
cor(Age,Height)

plot(Age,Height, main="Scatterplot", xlab="AGE", ylab="HEIGHT",
     las=1, pch=8) #can use cex or pch
#add linear line then smoother

abline(lm(Height~Age), col=4)
lines(smooth.spline(Age, Height), lty=2, lwd=5, col=2)


#categ variables are summarized using freq or proportion
table(Smoke)/length(Smoke) #create a freq table

#trimmed mean 
mean(LungCap, trim=0.10)

#spearman calculated by 
cor(LungCap, Age, method="spearman")

#covariance calculated 
cov(LungCap,Age)

summary(LungCap) #both categ and numerical variables

#do scatterplot of both male and female height v age

plot(Age[Gender=="male"],Height[Gender=="male"], pch="x",cex=0.7,
     xlab="Age",ylab="Height", las=1, col=2)
points(Age[Gender=="female"],Height[Gender=="female"], pch=1)

#splits plot screen for two sep plots 
par(mfrow=c(1,2))

plot(Age[Gender=="male"],Height[Gender=="male"],
     xlab="Age",ylab="Height",
     las=1, xlim=c(0,20),ylim=c(45,85), 
     main="Height vs Age for Males")

plot(Age[Gender=="female"],Height[Gender=="female"],
     xlab="Age",ylab="Height",
     las=1, xlim=c(0,20),ylim=c(45,85),
     main="Height vs Age for Females")

#changing the axis 
#so first remove axes and then add them 


#only comparing age and lung capacity
par(mfrow=c(1,1))
plot(Age, Height, main="TITLE", axes=F)
axis(side=1, at=c(7,12.3,15), labels=c("sev", "mean", "15"))

plot(Age,LungCap, main="Scatterplot of Lung Capacity with Age",las=1, xlab="Lung Capacity")
text(x=5, y=13, label="r=0.82", adj=1, cex=1,
     col=4,font=4)

quantile(LungCap,probs =c(0,0.25,0.5,0.75,1))

abline(h=mean(LungCap), col=2)
text(x=2.5, y=8.5, adj=0, label="Mean Lung Cap",
     cex=0.65, col=2)

#stratified boxplots- explore relationship between
#smoking and lung capacity within age strata
AgeGroups <- cut(Age, breaks=c(0,13,15,17,25),
                 labels=c("<13","14/15","16/17","18+"))
LungCapbox <- boxplot(LungCap~Smoke*AgeGroups, 
                      main="LungCap vs Smoke by Age Groups", 
                      las=2, ylab="Lung Capacity",col=c(4,2),axes=FALSE)
#modifications
box()
axis(2, at=seq(0,20,2), seq(0,20,2), las=1)
axis(1, at=c(1.5,3.5,5.5,7.5), 
     label=c("<13","14-15","16-17","18+"))

legend(x=5.5,y=4.5, legend=c("Non-smoke","Smoke"),
       col=c(4,2))

#the lung capacity affecting non smokers to smokers 

plot(Age[Smoke=="no"],LungCap[Smoke=="no"],
     main="Lung Capacity with Age",
     xlab="Age",ylab="Lung Capacity",
     las=1,col="blue",cex=1.25, pch=16)

points(Age[Smoke=="yes"],LungCap[Smoke=="yes"],
       col="red",cex=1.25,pch=17)

legend(x=3.5,y=13.5, legend=c("Non-smoke","Smoke"),
       col=c("blue","red"), lty=c(2,3), lwd =3, cex=0.75, bty="n")

lines(smooth.spline(Age[Smoke=="no"], LungCap[Smoke=="no"]),
      col="blue",lwd=2, lty=2)
lines(smooth.spline(Age[Smoke=="yes"], LungCap[Smoke=="yes"]),
      col="red",lwd=2, lty=3)