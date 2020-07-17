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

#change ref point from no to yes

Smoke <- relevel(Smoke, ref="yes")