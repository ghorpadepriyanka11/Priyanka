ea=c(101,102,103)
ln=c("Vaibhav","Priyanka","sujipt")
sal=c(2000,3000,4000)
a=data.frame(ea,ln,sal)
a
#--------------------------------------

i=12
if (i %% 2==1) {
  print("Number is odd")
  print("If part")
}else
{
  print("Number is even")
  print("Else part")
  cat("Number is",+i)
}
#----------------------------------------
if(a>b & a>c) print("A is greater") else
  if(b>a & b>c) print("B is Greater") else
    if(c>a & c>b) print("C is Greater")else
      if(a==b & b==c) print("All are equal")
#--------------------------------------------
ifelse(a>b,"a is greate","b is greater")
#-----------------Functions---------------------------
a=read.csv(file.choose())
a
sf=sample(2,nrow(a),replace = TRUE,prob = c(0.6,0.4))
set.seed(1122)
training=a[sf==1,]
test=a[sf==2,]
training
test
table(a$STATE)
table(test$STATE)
table(training$STATE)
#HW :::Read what is sampling with replacement
#--------------------------------------------
A=read.csv(file.choose())
#--------OR---------------------------------
A=read.csv('C:/Users/user/Downloads/EMP.csv')
A
install.packages("sqldf")
install.packages("dbly")
install.packages("caret")
install.packages("ggplot")
install.packages("xlsx")
install.packages("tree")
install.packages("arules")
install.packages("e1071")
library(sqldf)
installed.packages()
install.packages("ISLR")
install.packages("MASS")
install.packages("datasets")
library(ISLR)
ISLR::Credit
A=data.frame(Credit)
str(A)
sqldf("select * from A")
sqldf("select Income,Student from A where Student='Yes'and Income > 100")
sqldf("select Income,Student,Ethnicity from A where Ethnicity like 'A%'")
#------------------Group Level---------------------------------------
sqldf("select Ethnicity,count() from A group by Ethnicity ")
sqldf("select AVG(Income) from A")
sqldf("select ethnicity,sum(income) from A GROUP BY ethnicity")
sqldf("select ethnicity,max(income) from A GROUP BY ethnicity")
#------------------Table Level-----------------------------------------
table(A$Ethnicity)

table(A$Ethnicity,A$Married)

#-------------------------------------------


library(MASS)

A=data.frame(Cars93)
hist(A$Manufacturer)
A1=A[1:20,]
hist(A1$RPM)
table(A1$RPM)
hist(A1$RPM,col = 2)
hist(A1$RPM,col = 2:10)
hist(A1$RPM,col = c(2,2,2,2,3))
par(mfrow=c(2,2))
hist(A1$RPM,col = c(2,2,2,2,3))
hist(A1$RPM,col = c(2,2,2,2,3))
hist(A1$RPM,col = c(2,2,2,2,3))
par(mfrow=c(1,1))
hist(A1$RPM,col = 3,main = "Histogram of RPM's with repeat frequency")
hist(A1$RPM,col = 3,main = "Histogram of RPM's with repeat frequency")
hist(A1$RPM,col = "blue",main = "Histogram of RPM's with repeat frequency",xlab = "RPM of engines",ylab = "Repetation frequency")
hist(A1$RPM,col = "blue",main = "Histogram of RPM's with repeat frequency",xlab = "RPM of engines",ylab = "Repetation frequency",labels = TRUE,breaks = seq(4000,7000,200))

age=c(20,22,24,26,28,30,32,34,36)
height=c(150,152,154,156,157,158,159,162,163)
A=data.frame(age,height)
plot(age,height,col=2,pch=7,cex=0.8,xlim=c(15,60),ylim=c(150,180))
age1=c(20,38,35,39,35,34,37,39,37)
height1=c(160,162,160,158,159,152,166,165,159)
points(age1,height1,pch=8,col=4)
points(age1+4,height1+5,pch=9,col=5)

plot(A,col=4,pch=9)
library(ISLR)
A=data.frame(Credit)
head(A)
A$Limit
n=fivenum(A$Limit)
n
newcol=c(1:nrow(A))
A1=data.frame(A,newcol)
if(A$limit<n[1])A1$newcol=1 else
  if(A$limit>n[1] && A$limit<n[2])A1$newcol else
    if(A$limit>n[2] && A$limit<n[3])A1$newcol=3 else
      if(A$limit>n[3] && A$limit<n[4])A1$newcol=4 else
        A1$newcoll=5
#newcol = ifelse(A$Limit<3087,2,ifelse(A$Limit<4622.5,3,ifelse(A$Limit<5876.5,4,5)))
#A1=data.frame(A,newcol)
#A1

plot(A1$Age,A1$Limit,col=A1$newcol,pch=9)
#-------------Regression--------------------------------------
install.packages("psych")
library(psych)#find alteranative for this library

A=read.csv(file.choose())
A
A=na.omit(A)
A
sf=sample(2,nrow(A),replace=TRUE,prob=c(0.8,0.2))
trd=A[sf==1,]
tsd=A[sf==2,]
cor(A[,c(1,2,3,5)])
pairs.panels(A)
model1=lm(PROFIT~RND,data=trd)
pred=predict(model1,tsd)
pred
cbind(pred,tsd$PROFIT)

#------------------Correlation,Plotting------------
library(ISLR)
library(psych)
A=data.frame(Credit)
str(A)
head(A)
numcols = unlist(lapply(A,is.numeric))
B = A[,numcols]
pairs.panels(B)
cor(B)
sf=sample(2,nrow(A),replace=TRUE,prob=c(0.7,0.3))
trd=A[sf==1,]
trd=A[sf==2,]
modelInc=lm(Income~Limit,data=trd)
modelLimit=lm(Limit~Rating,data=trd)
modelRating=lm(Rating~Balance,data=trd)
modelBal =lm(Balance ~ Rating,data=trd)


pred1=predict(modelInc,tsd)
#------------------------------OR---------
library(ISLR)
library(psych)
A = data.frame(Credit)
str(A)
head(A)
pairs.panels(A)
numcols = unlist(lapply(A,is.numeric))
B = A[,numcols]
pairs.panels(B)
cor(B)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model1_Inc = lm(Income ~ Limit,data=trd)
#model2_Inc = lm(Income ~ Rating,data=trd)
#model3_Inc = lm(Income ~ Balance,data=trd)

#model1_Limit = lm(Limit ~ Balance,data=trd)
model2_Limit = lm(Limit ~ Rating,data=trd)
#model3_Limit = lm(Limit ~ Income,data=trd)

model1_Rating = lm(Rating ~ Balance,data=trd)

#model1_Bal = lm(Balance ~ Income,data=trd)
#model2_Bal = lm(Balance ~ Limit,data=trd)
model3_Bal = lm(Balance ~ Rating,data=trd)

pred_Inc = predict(model1_Inc,tsd)
cbind(tsd$Limit,pred_Inc,tsd$Income)
pred_Rating = predict(model1_Rating,tsd)
cbind(tsd$Rating,pred_Inc,tsd$Balace)
pred_Bal = predict(model3_Bal,tsd)
cbind(tsd$Balance,pred_Inc,tsd$Rating)
pred_Lim = predict(model2_Limit,tsd)
cbind(tsd$Limit,pred_Inc,tsd$Rating)

#--------------------Global Environment----------

a <- 2
b <- 5
f <- function(x) x<-0
 ls()
environment()
.GlobalEnv
#----------------Cascading Environment-------------

f=function(fx){
  g=function(gx){
    print("Inside g")
    print(environment())
    print(ls())
  }
  g(5)
  print("Inside f")
  print(environment())
  print(ls())
}

#-------------------Regression--------------------
A=data.frame(ISLR::Wage)
str(A)

numcols = unlist(lapply(A,is.numeric))
B = A[,numcols]
pairs.panels(B)
cor(B)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

#model1=lm(wage~year,data=trd)#0.0057
#model2=lm(wage~age, data=trd)#0.04258
#model3=lm(wage~logwage, data=trd)#0.04258
#model4=lm(wage~year+age, data=trd)#0.04748
model5=lm(wage~year+logwage,data=trd)#0.9032
model6=lm(wage~age+logwage,data=trd)#0.9035
model7=lm(wage~age+year+logwage,data=trd)#0.9035

pred5=predict(model5,tsd)
cbind(tsd$year,tsd$logwage,tsd$wage,pred5)
pred6=predict(model6,tsd)
cbind(tsd$age,tsd$logwage,tsd$wage,pred6)

#------------------------------------------
A = read.csv(file.choose())
str(A)
cor(A[,c(1,2,3,5)])
set.seed(123)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf==1,]
tsd = A[sf==2,]
nrow(A)
nrow(trd)
nrow(tsd)
model1 = lm(PROFIT ~ RND,data = trd)
model2 = lm(PROFIT ~ RND+MKT,data = trd)
model3 = lm(PROFIT ~ RND+MKT+ADMIN,data = trd)
model4 = lm(PROFIT ~ RND+ADMIN,data = trd)

pred1 = predict(model1,tsd)
pred2 = predict(model2,tsd)
pred3 = predict(model3,tsd)
pred4 = predict(model4,tsd)


cbind(tsd,pred1,pred2,pred3,pred4)


model5=lm(PROFIT ~ RND+MKT+STATE,data = trd)
pred5=predict(model5,tsd)

RND=c(160000,170000)
MKT=c(450000,500000)
STATE=c("Florida","New York")
w=data.frame(RND,MKT,STATE)
predw=predict(model5,w)
cbind(w,predw)

#------------------Polynomial Regression------------------

install.packages("ggplot2")
library("ggplot2")
a=c(21,19,39,50,60,55,78,69,90,110,111,120,130,141)

b = c(32,46,38,47,40,48,67,50,40,52,65,74,85,79)

f = data.frame(a,b)

ggplot(f, aes(y=b, x=a)) +
  geom_point(alpha = .9) +
  stat_smooth(method = "lm", formula = y ~ I(x^2))

#------------------Ridge and lasso -------------------
#------------------Classification ---------------Logisic regression-----
A = read.csv(file.choose())
str(A)
fivenum(A$PROFIT)
A$PR0FIT_TYPE=ifelse(A$PROFIT<=107978,0,1)
A$PR0FIT_TYPE = as.factor(A$PR0FIT_TYPE)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.8,0.2))
trd = A[sf==1,]
tsd = A[sf==2,]
model1 = glm(PR0FIT_TYPE ~ RND+MKT+ADMIN,data = trd,family = "binomial")
pred = predict(model1,tsd)
w = ifelse(pred <= 0.5,0,1)
cm = table(predicted = w,actual = tsd$PR0FIT_TYPE)
cbind(tsd$PR0FIT_TYPE,w)
#this will percentage of missclassified elements,u can also find correctly classified element
misclassifiedPercentage=(nrow(tsd)-sum(diag(cm)))/nrow(tsd)*100
#----------------Naive Bayes classification---------------
#In this we have linearity between predictors

A=data.frame(iris)
head(A)
View(A)
A=na.omit(A)
sf=sample(2,nrow(A),replace=TRUE,prob=c(0.8,0.2))
trd=A[sf==1,]
tsd=A[sf==2,]
install.packages("naivebayes")

library(naivebayes)
model1=naive_bayes(Species~.,trd)
pred=predict(model1,tsd)
cbind(pred,tsd$Species)
w=table(pred,tsd$Species)
misclassified=(1-(sum(diag(w))/nrow(tsd)))*100
model1$data
#-------------sampling with repitation read

#Classification-nonlinear decision tree-------------
install.packages("rpart")
install.packages("tree")
library(tree)
library(rpart)
A=data.frame(iris)
psych::pairs.panels(A[,1:4])
sf=sample(2,nrow(A),replace = TRUE,prob=c(0.7,0.3))
trd=A[sf==1,]
tsd=A[sf==2,]

model_tree=tree(Species ~ .,data=trd)#you can use rpart function instead of tree
pred_tree=predict(model_tree,tsd)
library(naivebayes)
model_nb=naive_bayes(Species~.,data=trd)
pred_nb=predict(model_nb,tsd)

model_rpart=rpart(Species~.,data=trd)
pred_rpart=predict(model_rpart,tsd)

plot(model1)
text(model1)

pred_t=ifelse(pred_tree[,1]>0.5,"setosa",ifelse(pred_tree[,2]>0.5,"versicolor","verginica"))
#pred_rpart=predict(model_rpart,Species)
w=table(pred_t,tsd$Species)
x=table(pred_nb,tsd$Species)
#-----------------------Decision Tree/Random forest-------------------------------------------
#------------------------------------------------------------new comment-----







