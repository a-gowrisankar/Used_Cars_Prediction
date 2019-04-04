library(ggplot2)                                   #library for ggplot
library(dplyr)
library(data.table)  
library(lubridate)                                 #for converting timestampt to date format
library(MASS)
library(Metrics)                                   #for rmse function
library(car)  
autos <- fread("./autos.csv")
nrow(autos)

autos$dateCrawled="null"                           #"dateCrawled" Attribute value is set to null


autos$seller="null"                                #"seller" Attribute value is set to null



autos$offerType="null"                             #"offerType" Attribute value is set to null



summary(autos$price)                               #summary of the attributes values


quantile(autos$price)                              #Function to find the distibution

quantile(autos$price, probs = c(0.05, 0.95))       #Find the distribution of values from 5% to 95%

autos = autos[(price > quantile(autos$price, 0.05)) & (price < quantile(autos$price, 0.95))]    #Limit the values of the attribute within the 5 to 95 percentage of overall values
ggplot(aes(x="price", y=price), data = autos) + geom_boxplot()                                  #Boxplot to depict the values after cleansing
nrow(autos)           #Number of rows in the dataset after cleansing the values

autos$abtest="null"                                #Set the value of the attribute"abtest" to null


autos= autos[autos$vehicleType != ""]              #Removing records with missing values for the attribute
nrow(autos)




autos= autos[autos$yearOfRegistration != ""]        #Removing records without year of registration
autos$age=2016-autos$yearOfRegistration             #Calculating the age of the vehicle
summary(autos$age)                                  #summary of the age values

quantile(auots$age)                                 #Find the distribution of the values
Error in quantile(auots$age) : object 'auots' not found
quantile(autos$age)                                 #Find the distribution of the values

autos = autos[(age > quantile(autos$age, 0.05)) & (age < quantile(autos$age, 0.95))]   #Restricting the values within a limit
nrow(autos)

autos= autos[autos$gearbox != ""]                           #Removing of records with missing values
nrow(autos)

quantile(autos$powerPS)                                     #Find the distribution of values

quantile(autos$powerPS, probs = c(0.05, 0.95))              #Find the distribution of values within 5 to 95%

autos = autos[(powerPS > quantile(autos$powerPS, 0.05)) & (powerPS < quantile(autos$powerPS, 0.95))]    #Limiting the values and drop the remaining
nrow(autos)



autos$model ="null"                              #Setting the value for the attribute as null
autos= autos[autos$fuelType != ""]               #Removing the records with missing values
autos= autos[autos$brand != ""]                  #Removing of records with missing values
autos= autos[autos$notRepairedDamage != ""]      #Removing of records with missing values
autos$dateCreated = ymd_hms(autos$dateCreated)   #formatting to date from timestamp
autos$lastSeen = ymd_hms(autos$lastSeen)         #formatting to date from timestamp
autos$sellingTime = as.integer(as.Date(autos$lastSeen) - as.Date(autos$dateCreated))  #Calculating the selling time
autos = autos[(sellingTime  > quantile(autos$sellingTime, 0.05)) & (sellingTime < quantile(autos$sellingTime, 0.95))]  #limiting the selling time values 
nrow(autos)


autosclean=autos                                 #copying the values to a new table

#Plot the numerical data vs the Price 

price=autosclean$price                              #Creating variable to hold the price value of the vehicle
powerPS=autosclean$powerPS                          #Creating variable to hold the Engine power value of the vehicle
kilometer=autosclean$kilometer                      #Creating variable to hold the Kilometer value of the vehicle
sellingTime=autosclean$sellingTime                  #Creating variable to hold the selling time  of the vehicle
age=autosclean$age                                  #Creating variable to hold the age  of the vehicle


plot(~price+powerPS+kilometer+sellingTime+age,main="Simple scatterplot")

#Studying the effect of Vehicle type and fuel type combination over the reselling price of the used car


bus = ifelse(autosclean$vehicleType=="bus",1,0)                          #Creating variable for vehicle type bus
cabrio = ifelse(autosclean$vehicleType=="cabrio",1,0)                    #Creating variable for vehicle type cabrio
coupe = ifelse(autosclean$vehicleType=="coupe",1,0)                      #Creating variable for vehicle type coupe
kleinwagen = ifelse(autosclean$vehicleType=="kleinwagen",1,0)            #Creating variable for vehicle type klienwagen
kombi = ifelse(autosclean$vehicleType=="kombi",1,0)                      #Creating variable for vehicle type kombi
limousine = ifelse(autosclean$vehicleType=="limousine",1,0)              #Creating variable for vehicle type limousine
suv = ifelse(autosclean$vehicleType=="suv",1,0)                          #Creating variable for vehicle type suv
benzin = ifelse(autosclean$fuelType=="benzin",1,0)                       #Creating variable for fuel type benzin
diesel = ifelse(autosclean$fuelType=="diesel",1,0)                       #Creating variable for fuel type diesel
lpg = ifelse(autosclean$fuelType=="lpg",1,0)                             #Creating variable for fuel type lpg
andere = ifelse(autosclean$fuelType=="andere",1,0)                       #Creating variable for fuel type andere
cng = ifelse(autosclean$fuelType=="cng",1,0)                             #Creating variable for fuel type cng
elektro = ifelse(autosclean$fuelType=="elektro",1,0)                     #Creating variable for fuel type elektro
hybrid = ifelse(autosclean$fuelType=="hybrid",1,0)                       #Creating variable for fuel type hybrid

model=lm(price ~ bus+cabrio+coupe+kleinwagen+kombi+limousine+suv+benzin+diesel+lpg+andere+cng+elektro+hybrid, data = autosclean)
summary(model)

m1=stepAIC(model,direction=c("forward"),scope=list(upper=model ,lower=~1),trace=F)          #Forward Selection
summary(m1) 
pr1=predict(m1,newdata=autosclean)
r1=rmse(pr1, autosclean$price)
r1

m2=stepAIC(model,direction=c("backward"),scope=list(upper=model ,lower=~1),trace=F)         #Backward Selection
summary(m2)
pr2=predict(m2,newdata=autosclean)
r2=rmse(pr2, autosclean$price)
r2

m3=stepAIC(model,direction=c("both"),scope=list(upper=model ,lower=~1),trace=F)              #Both direction
summary(m3)
pr3=predict(m3,newdata=autosclean)
r3=rmse(pr3, autosclean$price)
r3


list(r1,r2,r3)
which.min(list(r1,r2,r3))

res1=residuals(m1)
qqnorm(res1)
qqline(res1,col=4)


res2=residuals(m2)
qqnorm(res2)
qqline(res2,col=4)


res3=residuals(m3)
qqnorm(res3)
qqline(res3,col=4)

#Studying the effect of vehicletype and power on reselling price


powerPS=autosclean$powerPS         #creating variable to hold the power value of the vehicle
prob3=lm(price ~ bus+cabrio+coupe+kleinwagen+kombi+limousine+suv+powerPS, data = autosclean)             #Linear regression with all vehicle types nd power
summary(prob3)


#Forward Direction
p31=stepAIC(prob3,direction=c("forward"),scope=list(upper=prob3 ,lower=~1),trace=F)
summary(p31)


#Backward Direction
p32=stepAIC(prob3,direction=c("backward"),scope=list(upper=prob3 ,lower=~1),trace=F)
summary(p32)

#Both Direction
p33=stepAIC(prob3,direction=c("both"),scope=list(upper=prob3 ,lower=~1),trace=F)
summary(p33)


when we try to perform stepwise selection no change occurs, now we verify the individual parameter t-test and build the model with paramaters satisfying t-test.

p34=lm(price ~ bus+cabrio+coupe+kombi+limousine+suv+powerPS, data = autosclean)     # remove the variable klienwagen as it fails the individual parameter test
summary(p34)
p3r=predict(p34,newdata=autosclean)
rmsep3=rmse(p3r, autosclean$price)
rmsep3
resp3=residuals(p34)
qqnorm(resp3)
qqline(resp3,col=4)


#Studying the effect of gearbox combined with fuel type on vehicle reselling price


manuell = ifelse(autosclean$gearbox=="manuell",1,0)                             #Variable to hold the mauell gearbox valuefor the vehicle
automatic = ifelse(autosclean$gearbox=="automatik",1,0)                         #Variable to hold the automatic gearbox valuefor the vehicle

prob4=lm(price ~ benzin+diesel+lpg+andere+cng+elektro+hybrid+manuell+automatic, data = autosclean)     #Regression model with gearbox and fuel type
summary(prob4)


p41=lm(price ~ benzin+lpg+andere+cng+elektro+hybrid+manuell+automatic, data = autosclean)           # remove the variable diesel
summary(p41)

p42=lm(price ~ benzin+lpg+andere+cng+elektro+manuell+automatic, data = autosclean)                  # remove the variable hybrid
summary(p42)

p43=lm(price ~ benzin+lpg+andere+cng+manuell, data = autosclean)                                    #remove variable elektro, automatic as it has NA
summary(p43)

predict4=predict(p43,newdata=autosclean)
rmp4=rmse(predict4, autosclean$price)
rmp4
resp4=residuals(p43)
qqnorm(resp4)
qqline(resp4,col=4)



#splitting of dataset into Training and test Dataset

select.data=sample(1:nrow(autosclean),0.7*nrow(autosclean))
tdata=autosclean[select.data,]                 #train data
nrow(tdata)
tedata=autosclean[-select.data,]               #test data
nrow(tedata)

select.data=sample(1:nrow(autosclean),0.7*nrow(autosclean))
tdata=autosclean[select.data,]                 #train data
nrow(tdata)
tedata=autosclean[-select.data,]               #test data
nrow(tedata)

bus = ifelse(tdata$vehicleType=="bus",1,0)                          #Creating variable for vehicle type bus
cabrio = ifelse(tdata$vehicleType=="cabrio",1,0)                    #Creating variable for vehicle type cabrio
coupe = ifelse(tdata$vehicleType=="coupe",1,0)                      #Creating variable for vehicle type coupe
kleinwagen = ifelse(tdata$vehicleType=="kleinwagen",1,0)            #Creating variable for vehicle type klienwagen
kombi = ifelse(tdata$vehicleType=="kombi",1,0)                      #Creating variable for vehicle type kombi
limousine = ifelse(tdata$vehicleType=="limousine",1,0)              #Creating variable for vehicle type limousine
suv = ifelse(tdata$vehicleType=="suv",1,0)                          #Creating variable for vehicle type suv
benzin = ifelse(tdata$fuelType=="benzin",1,0)                       #Creating variable for fuel type benzin
diesel = ifelse(tdata$fuelType=="diesel",1,0)                       #Creating variable for fuel type diesel
lpg = ifelse(tdata$fuelType=="lpg",1,0)                             #Creating variable for fuel type lpg
andere = ifelse(tdata$fuelType=="andere",1,0)                       #Creating variable for fuel type andere
cng = ifelse(tdata$fuelType=="cng",1,0)                             #Creating variable for fuel type cng
elektro = ifelse(tdata$fuelType=="elektro",1,0)                     #Creating variable for fuel type elektro
hybrid = ifelse(tdata$fuelType=="hybrid",1,0)                       #Creating variable for fuel type hybrid
notRepairedDamage = (tdata$notRepairedDamage)
age=tdata$age
kilometer=tdata$kilometer
gearbox=tdata$gearbox



#Group 1( Physical Appearance)
G1=lm(price ~ age +bus+cabrio+coupe+kleinwagen+kombi+limousine+suv+notRepairedDamage+kilometer,data = autosclean)
summary(G1)
rG1=residuals(G1)
qqnorm(rG1)
qqline(rG1,col=2)

pG1= predict(G1,newdata = tdata)
rmG1=rmse(pG1, tdata$price)
rmG1


#Group 2( Performance)
G2=lm(price ~ powerPS + gearbox + benzin+diesel+lpg+andere+cng+elektro+hybrid),data = tdata)
summary(G2)

g1=lm(price ~ powerPS + gearbox + benzin+diesel+lpg+andere+cng+hybrid,data = tdata)
summary(g1)

g2=lm(price ~ powerPS + gearbox + benzin+diesel+lpg+andere+cng,data = tdata)
summary(g2)

rg2=residuals(g2)
qqnorm(rg2)
qqline(rg2,col=2)

pg2= predict(g2,newdata = tdata)
rmg2=rmse(pg2, tdata$price)
rmg2


#Group 3( Combined appearance and performance)
G3 =lm(price ~  bus+cabrio+coupe+kleinwagen+kombi+limousine+suv + gearbox + powerPS + notRepairedDamage + kilometer,data = tdata)
summary(G3)
rG3=residuals(G3)
qqnorm(rG3)
qqline(rG3,col=2)

pG3= predict(G3,newdata = tdata)
rmG3=rmse(pG3, tdata$price)
rmG3

list(rmG1,rmg2,rmG3)
which.min(list(rmG1,rmg2,rmG3))



#full model with the important factors( vehicleType,gearbox,powerPS,kilometer,fuelType,notRepairedDamage,age)
fullmodel=lm(price~bus+cabrio+coupe+kleinwagen+kombi+limousine+suv+benzin+diesel+lpg+andere+cng+elektro+hybrid+gearbox + powerPS + notRepairedDamage+kilometer+age,data=tdata)
summary(fullmodel)

full1=stepAIC(fullmodel,direction=c("forward"),scope=list(upper=fullmodel ,lower=~1),trace=F)
summary(full1)

full2=stepAIC(fullmodel,direction=c("backward"),scope=list(upper=fullmodel ,lower=~1),trace=F)
summary(full2)


full3=stepAIC(fullmodel,direction=c("both"),scope=list(upper=fullmodel ,lower=~1),trace=F)
summary(full3)


 
full4=lm(formula = price ~ cabrio + coupe + kleinwagen + kombi + limousine + 
    suv + benzin + diesel + lpg + andere + cng  + gearbox + 
    powerPS + notRepairedDamage + kilometer + age, data = tdata) #removing variable elektro as it failed individual parameter test
summary(full4)

res=residuals(full4)                                   #residual assigned to variable 
plot(fitted(full4), res, main="Predicted vs Error")
abline(a=0,b=0,col=2)
hist(res,xlab="Residual",prob=TRUE,main="Histogram")   #residual histogram plot
xfit=seq(min(res),max(res),length=40)
yfit=dnorm(xfit,mean=mean(res),sd=sd(res))
lines(xfit,yfit,col="red",lwd=2)
tedata$nprice=sqrt(tedata$price)
tdata$nprice=sqrt(tdata$price)              # square root transformation on the dependent variabel and assigning to new variable
nrpice=autosclean$nprice


full5=lm(nprice~bus+cabrio+coupe+kleinwagen+kombi+limousine+suv+benzin+diesel+lpg+andere+cng+elektro+hybrid+gearbox + powerPS + notRepairedDamage+kilometer+age,data=tdata)
summary(full5)

full6=stepAIC(full5,direction=c("both"),scope=list(upper=full5 ,lower=~1),trace=T)
summary(full6)

full7=lm(formula = nprice ~ cabrio + coupe + kleinwagen + kombi + limousine +                    #removing variable elektro as it failed individual parameter test
    suv + benzin + diesel + lpg + andere + cng + gearbox + 
    powerPS + notRepairedDamage + kilometer + age, data = tdata)
summary(full7)

rest=residuals(full7)                  
plot(fitted(full7), rest, main="Predicted vs Error")
abline(a=0,b=0,col=2)
hist(rest,xlab="Residual",prob=TRUE,main="Histogram")
xfit=seq(min(rest),max(rest),length=40)
yfit=dnorm(xfit,mean=mean(rest),sd=sd(rest))
lines(xfit,yfit,col="red",lwd=2)


outlierTest(full7)
tdata=tdata[-c(18849,107262,28034,78258,103082,32119,43999,92047,49867,82837)]


Train= predict(full7,newdata = tdata)
TrainError=rmse(Train, tdata$nprice)
TrainError

bus = ifelse(tdata$vehicleType=="bus",1,0)                          #Creating variable for vehicle type bus
cabrio = ifelse(tdata$vehicleType=="cabrio",1,0)                    #Creating variable for vehicle type cabrio
coupe = ifelse(tdata$vehicleType=="coupe",1,0)                      #Creating variable for vehicle type coupe
kleinwagen = ifelse(tdata$vehicleType=="kleinwagen",1,0)            #Creating variable for vehicle type klienwagen
kombi = ifelse(tdata$vehicleType=="kombi",1,0)                      #Creating variable for vehicle type kombi
limousine = ifelse(tdata$vehicleType=="limousine",1,0)              #Creating variable for vehicle type limousine
suv = ifelse(tdata$vehicleType=="suv",1,0)                          #Creating variable for vehicle type suv
benzin = ifelse(tdata$fuelType=="benzin",1,0)                       #Creating variable for fuel type benzin
diesel = ifelse(tdata$fuelType=="diesel",1,0)                       #Creating variable for fuel type diesel
lpg = ifelse(tdata$fuelType=="lpg",1,0)                             #Creating variable for fuel type lpg
andere = ifelse(tdata$fuelType=="andere",1,0)                       #Creating variable for fuel type andere
cng = ifelse(tdata$fuelType=="cng",1,0)                             #Creating variable for fuel type cng
elektro = ifelse(tdata$fuelType=="elektro",1,0)                     #Creating variable for fuel type elektro
hybrid = ifelse(tdata$fuelType=="hybrid",1,0)                       #Creating variable for fuel type hybrid
notRepairedDamage = (tdata$notRepairedDamage)
age=tdata$age
kilometer=tdata$kilometer
gearbox=tdata$gearbox


full8=lm(formula = nprice ~ cabrio + coupe + kleinwagen + kombi + limousine + 
    suv + benzin + diesel + lpg + andere + cng + gearbox + 
    powerPS + notRepairedDamage + kilometer + age, data = tdata)
summary(full8)

reste=residuals(full8)
plot(fitted(full8), reste, main="Predicted vs Error")
abline(a=0,b=0,col=2)
hist(reste,xlab="Residual",prob=TRUE,main="Histogram")
xfit=seq(min(reste),max(reste),length=40)
yfit=dnorm(xfit,mean=mean(reste),sd=sd(reste))
lines(xfit,yfit,col="red",lwd=2)

Train= predict(full8,newdata = tdata)
TrainError=rmse(Train, tdata$nprice)
TrainError

