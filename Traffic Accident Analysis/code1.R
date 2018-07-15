install.packages("caTools")
install.packages("rpart")
install.packages("MASS")
install.packages("caTools")
library(MASS)
library(party)
library(rpart)
library(caTools)
set.seed(2)
data <- read.csv("final.csv")
View(data)
str(data)
apply(data,2,function(x) round(length(unique(x))/nrow(data),3)*100)
cols<-c("Day_of_Week","Accident_Severity","Number_of_Vehicles","Number_of_Casualties","Road_Type","Speed_limit","Pedestrian_Crossing.Human_Control","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Urban_or_Rural_Area","Casualty_Class","Sex_of_Casualty","Casualty_Severity","Vehicle_Type","Sex_of_Driver")
for(i in cols)
{
  data[,i]=as.factor(data[,i])
}
summary(data)

ind<-sample.split(Y=data$Accident_Severity,SplitRatio = 0.8)
traindf<-data[ind,]
testdf<-data[!ind,]

pct<-ctree(Accident_Severity ~ .,data=traindf,controls=ctree_control(mincriterion=0.99,minsplit=200000))
plot(pct)

pwc<-predict(pct,testdf)
pwc
t<-table(predictions=pwc,actual=testdf$Accident_Severity)
t
sum(diag(t))/sum(t)


dtm<-rpart(Accident_Severity ~.,data=traindf)
library(rpart.plot)
rpart.plot(dtm)
text(dtm,pretty=0)
summary(dtm)

