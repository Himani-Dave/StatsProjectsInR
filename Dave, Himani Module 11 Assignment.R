"
****************************************************************
Name:Himani Dave
Student Number:A00178443

QMM1002 Module 11 R Assignment
****************************************************************
"
#############						   	                  
#Question #1							
#############

births <- read.csv("Births.csv")

# a) 

births.ts <- ts(births[,-1], start = 1958, end = 2016, frequency = 1)

plot.ts(births.ts, xlab="Years from 1958 to 2016",
        ylab="Number of Births",
        main="Births per 10,000 of 23 year old women from 1958 to 2016",
        plot.type = "single", col="darkslategray") 
legend("topleft", legend="Births",col="darkslategray", lty=1, cex=0.5)

# b) 

#From the graph, it can be seen that the number of births tart decreasing from 1965,
#they keep decreasing until 1980. From 1980 onwards, the number starts increasing and 
#again from 2000, there is a steep decrease in the number of births. Since there is a 
#trend present but seasonal component absent, Holt exponential smoothing would work best.

# c) 

births.h <- HoltWinters(births.ts,gamma=FALSE) 
births.h

#Smoothing parameters:
#  alpha: 1
#beta : 0.1716082
#gamma: FALSE

#alpha = 1, current values have all the overall weight
#beta nearly 0, historical values have more weight than current trends
#No seasonality so gamma value false

# d) 

plot(cbind(births.ts, births.h$fitted[,1]), plot.type="s",  
     col=c( "lightcoral", "lightblue1"),  ylab="Number of Births",
     main="Original vs forecasted values of birth time series data") 
legend("topleft", legend=c("Data", "HES"),
       col=c( "lightcoral", "lightblue1"), lty=1, cex=0.5) 

# e) 

install.packages("forecast") 
library(forecast) 
error.births.h<-accuracy(forecast(births.h))
error.births.h

# f) 

births.fitted<-births.h$fitted[,1]
births.fitted 

# g) 

forecast.2017<- forecast(births.h, h = 1)
forecast.2017

#The forecasted number of births in 2017 per 10000 is 110.4641.

# h)

births.forecast<-forecast(births.h, h=5) 
births.forecast
plot(births.forecast, xlab="Time", ylab="Births", 
     main = "Forecast based on SES model")

#According to the graph, the number of births would continue to declinein 2020.

# i) 

#We can say with a 95% confidence that the upper bound is 146 for 2020. It can be predicted
#that the levels wouold be equivalent to those in 2016 and might even increase. 
# j)

plot(births.forecast$residuals, ylab="Residuals", main="Residuals for Births") 
abline(h=0, col="hotpink3", lty=2)

#No significant patterns. Peaks and troughs balance each other out.