library(readr)
BodyFat <- read_csv("C:/Documents/My Excel/BodyFat.csv")
BodyFat$X18<-NULL #This drops the unknown column called "X18"
View(BodyFat)
print(str(BodyFat))
library(ggplot2)
library(RColorBrewer)
length(unique(BodyFat$AGE))
unique(BodyFat$AGE)
summary(BodyFat)

#Plotting age vs bodyfat
ggplot(BodyFat,aes(AGE,BODYFAT))+geom_point(aes(x=AGE,y=BODYFAT,color=BODYFAT,size=AGE))
ggplot(BodyFat,aes(AGE,BODYFAT))+geom_jitter() #There is some correlation b/t age and bodyfat

#Plotting age vs weight
ggplot(BodyFat,aes(AGE,WEIGHT))+geom_jitter(aes(X=AGE,y=WEIGHT,color=WEIGHT,size=AGE))
ggplot(BodyFat,aes(AGE,WEIGHT))+geom_jitter() #No correlation b/t age and weight

#Plotting weight vs height
ggplot(BodyFat,aes(WEIGHT,HEIGHT))+geom_jitter(aes(X=WEIGHT,y=HEIGHT,color=WEIGHT,size=HEIGHT))
ggplot(BodyFat,aes(WEIGHT,HEIGHT))+geom_jitter() #No correlation b/t age and weight

#Which patients have the highest and lowest bodyfat. Top5/bottom5
BodyFat[order(BodyFat$BODYFAT,decreasing=T)[1:5],] #What can I say about this?
BodyFat[order(BodyFat$BODYFAT,decreasing=F)[1:5],] #What can I say about this?

#Calculationg the Regression lines
BfvsAge<-lm(BODYFAT~AGE,data=BodyFat)
summary(BfvsAge) #Does the R2 support and conclude the graph above

BfvsWeight<-lm(BODYFAT~WEIGHT,data=BodyFat)
summary(BfvsWeight)

BfvsHeight<-lm(BODYFAT~HEIGHT,data=BodyFat)
summary(BfvsHeight) #Explain why the R2 is too low

#Calculating Bodyfat vs all independant variables
BfvsALL<-lm(BODYFAT~DENSITY+AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+
              KNEE+ANKLE+BICEPS+FOREARM+WRIST,data=BodyFat)
summary(BfvsALL) #Explain why the R2 is very high. What is this telling me


