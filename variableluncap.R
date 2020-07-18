#variables

#making a dummy variable

mean(LungCap[CatHeight=="A"])
mean(LungCap[CatHeight=="B"])
mean(LungCap[CatHeight=="C"])
mean(LungCap[CatHeight=="D"])
mean(LungCap[CatHeight=="E"])
mean(LungCap[CatHeight=="F"])

mod <- lm(LungCap ~ CatHeight)
summary(mod)

#change ref group (from no to yes) to reparamaterize model  

Smoke <- relevel(Smoke, "yes")

mod1 <- lm(LungCap ~ Age + Smoke)
summary(mod1)

plot(Age[Smoke=="no"], LungCap[Smoke=="no"], col="blue",
     ylim=c(0,15), xlab="Age", ylab="LungCap", main="LungCap vs Age,Smoke",las=1)

points(Age[Smoke=="yes"],LungCap[Smoke=="yes"],col="red",pch=16)

legend(x=3.5,y=13.5, legend=c("Non-smoke","Smoke"),
       col=c("blue","red"), pch=c(1,16), bty="n") 
# add lty=c(2,3) + lwd =3 after regression lines added


#add regression lines 
abline(a=1.08, b=0.555, col="blue", lwd=3)

abline(a=0.431, b=0.555, col="red",lwd=3)

# we see no effect modification 

#regression equation is -> meuw = 1.08 + (0.555xAge)-(0.649xSmoke)