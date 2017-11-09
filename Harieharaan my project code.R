# Project Title: Room rent analysis for hotels
# NAME: Harieharaan Duraikannu
# EMAIL: harieharaan619@gmail.com
# COLLEGE / COMPANY: SSN college of Engineering

#Reading the dataset and creating a data frame
cities42.df<-read.csv(paste("Cities42.csv",sep = ""))

#Viewing the data
View(cities42.df)

#summarizing the data
summary(cities42.df)

#omitting the na
cities42.df=na.omit(cities42.df)

#Removing the repeated dates 

cities42.df$Date<-gsub("18-Dec-16", "Dec 18 2016", cities42.df$Date)
cities42.df$Date<-gsub("21-Dec-16", "Dec 21 2016", cities42.df$Date)
cities42.df$Date<-gsub("24-Dec-16", "Dec 24 2016", cities42.df$Date)
cities42.df$Date<-gsub("25-Dec-16", "Dec 25 2016", cities42.df$Date)
cities42.df$Date<-gsub("28-Dec-16", "Dec 28 2016", cities42.df$Date)
cities42.df$Date<-gsub("31-Dec-16", "Dec 31 2016", cities42.df$Date)
cities42.df$Date<-gsub("4-Jan-17", "Jan 04 2017", cities42.df$Date)
cities42.df$Date<-gsub("4-Jan-16", "Jan 04 2017", cities42.df$Date)
cities42.df$Date<-gsub("8-Jan-16", "Jan 08 2017", cities42.df$Date)
cities42.df$Date<-gsub("8-Jan-17", "Jan 08 2017", cities42.df$Date)
cities42.df$Date<-gsub("Jan 4 2017", "Jan 04 2017", cities42.df$Date)
cities42.df$Date<-gsub("Jan 8 2017", "Jan 08 2017", cities42.df$Date)


#dates to factors for labelling 
cities42.df$Date<-factor(cities42.df$Date)
is.factor(cities42.df$Date)

#Checking the labelling
levels(cities42.df$Date)

#Analyzing the summary of the data and describing the variables

library(psych)
describe(cities42.df)

summary(cities42.df)

#Taking Y = RoomRent, identifying the most relevent  predictors variables by corrgram


#Corrgram

library(corrgram)

corrgram(cities42.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Hotel  data")
##from the corrgram it is understood that HasSwimming, StarRating, HotelCapital are very well correlated to RoomRent
##therefore we take it as predictors

#boxplot for  predictors

boxplot(cities42.df$StarRating,xlab="STAR RATING")

boxplot(cities42.df$RoomRent,xlab="ROOM_RENT")

boxplot(cities42.df$HasSwimmingPool,xlab="Has Swimming Pool")

boxplot(cities42.df$HotelCapacity,xlab="HOTEL Capacity")


##Visualizing data for Y as Room rent and X1,X2,X3 as HasSwimmingPool, StarRating and HotelCapacity respectively

#Table for HasSwimmingPool
table(cities42.df$HasSwimmingPool)
barplot(table(cities42.df$HasSwimmingPool),main="Barrplot of Hotel Swimming Pool")


#Table for StarRating
table(cities42.df$StarRating)
barplot(table(cities42.df$StarRating),main = "Barrplot for Star Rating")

#BoxPlot for HotelCapacity
boxplot(cities42.df$HotelCapacity, main="Boxplot for Hotel Capacity",horizontal = TRUE)

#Scatterplot for predictor variable

library(car)
#StarRating Vs RoomRent

scatterplot(cities42.df$StarRating,cities42.df$RoomRent,main="RoomRent of Hotels  with StarRating",ylab = "RoomRent in INR", xlab="Star rating out of 5",cex=1.1)

#RoomRent Vs HotelCapacity

scatterplot(cities42.df$RoomRent,cities42.df$HotelCapacity,main="RoomRent of Hotels  with Hotel capacity",ylab = "Hotel Capacity in rooms", xlab="RoomRent in INR",cex=1.1)

#RoomRent Vs HasSwimmingPool

plot(jitter(cities42.df$RoomRent),jitter(cities42.df$HasSwimmingPool),main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent",cex=1.1)
library(lattice)
bwplot(HasSwimmingPool~RoomRent, data = cities42.df,main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent" )

#Scatterplot matrix

scatterplotMatrix(
  cities42.df[
    ,c("RoomRent","HasSwimmingPool","StarRating", "HotelCapacity")], 
  spread=FALSE, smoother.args=list(lty=2),
  main="Scatter Plot Matrix", diagonal = "histogram")


#Corrgram of Y, x1, x2, x3

library(corrgram)

xyz<-data.frame(cities42.df$RoomRent, cities42.df$HasSwimmingPool, cities42.df$HotelCapacity, cities42.df$StarRating)
corrgram(xyz, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Hotel Prices In India")

#Variance-Covariance Matrix for Y, x1, x2, x3

x<-cities42.df[,c("HasSwimmingPool","StarRating", "HotelCapacity")]
y<-cities42.df[,c("RoomRent")]

cov(x,y)
var(x,y)
cor(x,y)


#Comparing other factors and their pattern using other trends with roomrent

#Analyzing IsWeekeng effect on RoomRent
table(cities42.df$IsWeekend)

table1<-table(cities42.df$IsWeekend)
barplot(table1, main="Distribution of Weekend", xlab="Not weekend(0)         Weekend(1)", col="orange")

#Effect of Isweekend on RoomRent
iw= aggregate(RoomRent ~ IsWeekend, data=cities42.df,mean)
iw

boxplot(RoomRent~IsWeekend,data=cities42.df, main="Room rent vs. IsWeekend", ylab="Not weekend(0)  weekend(1)", xlab="Room Rent in rupees ", col=c("red","blue"),horizontal=TRUE)

#Comapring RoomRent on different dates
table(cities42.df$Date)

library(lattice)
histogram(~Date, data = cities42.df, main="Distribution of Dates", xlab = "Differnt of Dates", col="Blue")


#Effect of different dates on RoomRent

d = aggregate(RoomRent ~ Date, data = cities42.df,mean)
d

scatterplot(d$Date,d$RoomRent, main="Scatterplot between Date and RoomRent", xlab="Date", ylab = "Room Rent in Rupees")


boxplot(RoomRent~Date,data=cities42.df, main="Room rent vs. Date", xlab="Different Dates", ylab="Room Rent in rupees ", col=c("red","blue","green","yellow"))

#Analyzing IsMetroCity effect on RoomRent
table(cities42.df$IsMetroCity)

table1<-table(cities42.df$IsMetroCity)
barplot(table1, main="Distribution of IsMetroCity", xlab="Not a Metro city(0)         Is a Metro City(1)", col="blue")

#Effect of IsMetroCity on RoomRent
imc = aggregate(RoomRent ~ IsMetroCity, data = cities42.df, mean)
imc

boxplot(RoomRent~IsMetroCity,data=cities42.df, main="Room rent vs. IsMetroCity", ylab="Metro city(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)


#Analyzing IsTouristDestination effect on RoomRent
table(cities42.df$IsTouristDestination)

table1<-table(cities42.df$IsTouristDestination)
barplot(table1, main="Distribution of IsToursitDestination", xlab="Not a Tourist Destination(0)         Is a Tourist Destination(1)", col="yellow")

#Effect of IsTouristDestination on RoomRent
itd = aggregate(RoomRent ~ IsTouristDestination, data = cities42.df, mean)
itd

boxplot(RoomRent~IsTouristDestination,data=cities42.df, main="Room rent vs. IsTouristDestination", ylab="IsTouristDestination(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)

#Analyzing FreeWifi Vs RoomRent
table(cities42.df$FreeWifi)
fw<-table(cities42.df$FreeWifi)
barplot(fw, main="Borplot of FreeWifi",xlab= "FreeWifi" ,col="red")

#Effect of FreeWifi on RoomRent
fw = aggregate(RoomRent ~ FreeWifi, data = cities42.df, mean)
fw

boxplot(RoomRent~FreeWifi,data=cities42.df, main="Room rent vs. FreeWifi", ylab="Free Wifi available(1)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)

#Analyzing FreeBreakfast Vs RoomRent
table(cities42.df$FreeWifi)
fw<-table(cities42.df$FreeBreakfast)
barplot(fw, main="Borplot of FreeBreakfast",xlab= "FreeWifi" ,col="red")


#Effect of FreeBreakfast on RoomRent
fb = aggregate(RoomRent ~ FreeBreakfast, data =cities42.df, mean)
fb


boxplot(RoomRent~FreeBreakfast,data=cities42.df, main="Room rent vs. FreeBreakfast", ylab="Free Breakfast available(1)", xlab="Room Rent in rupees ", col=c("green","yellow"),horizontal=TRUE)


#Analyzing Airport distance from hotel effects in what way on RoomRent
summary(cities42.df$Airport)

boxplot(cities42.df$Airport, main="Boxplot of Airport",xlab= "Distance of airport from hotel(Km)" ,col="green",horizontal = TRUE)

#Effect of Airport distance on RoomRent

scatterplot(cities42.df$Airport,cities42.df$RoomRent, main="Room rent vs. Airport distance", xlab="Airport distance(km)", ylab="Room Rent in rupees ",cex=1.1)


##Hypothesis

#1.Average RoomRent in hotels having swimming pool is more than that which don't have.
t.test(RoomRent~HasSwimmingPool,data = cities42.df, alternative="less")

#2.Average RoomRent in hotels with high star rating is high as compared to one which has less star rating.
t.test(cities42.df$RoomRent,cities42.df$StarRating)

#3.Average RoomRent in hotels providing Free Breakfast is more than that which don't provide.
t.test(RoomRent~FreeBreakfast, data = cities42.df, alternative="less")

#4.Average RoomRent in metro cities hotels is more than that of non metro cities.
t.test(RoomRent~IsMetroCity, data = cities42.df, alternative="less")

#5.Average RoomRent in hotels having more hotel capacity is more compared to one with less capacity.
t.test(cities42.df$RoomRent,cities42.df$HotelCapacity)

#Generating a multiple linear regression model for RoomRent
#1.
fit1<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity-1, data = cities42.df)
summary(fit1)

#Coefficents of the model
fit1$coefficients

#Fitted residuals and values  are checked and the deviation was around 1000 , because of 
#large data points it's not suitable to show those in the output file.

###.  Model1:    salary = b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity
#   b0 = -1(assumption),  b1 =  1396.874562, b2=3719.6943, b3= -7.659814
#  Model:    salary = -1 + 1396.874562*StarRating + 3719.6943*HasSwimmingPool -7.659814*HotelCapacity


#2.
fit2<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+IsWeekend+IsTouristDestination-1, data = cities42.df)
summary(fit2)

#Coefficents of the model
fit2$coefficients

#Fitted residuals and values  are checked and the deviation was around 1000 , because of 
#large data points it's not suitable to show those in the output file.


###.  Model1:    salary = b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity +b4*IsWeekend(0) + b5*IsWeekend(1) + b6*IsTouristDestination
#   b0 = -1(assumption),  b1 =  3635.819, b2=2285.132, b3= -13.965, b4=-8396.67457, b5=-8325.09152,b6=1878.94395
#  Model:    salary = -1 + 3635.819*StarRating + 2285.132*HasSwimmingPool -13.965*HotelCapacity
# -8396.67457*IsWeekend(0) - 8325.09152*IsWeekend(1) + 1878.94395*IsTouristDestination 


#3.
fit3<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+Airport-1, data = cities42.df)
summary(fit3)

#Coefficents of the model
fit3$coefficients
