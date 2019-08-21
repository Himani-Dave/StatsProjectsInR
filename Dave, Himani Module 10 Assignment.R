"
****************************************************************
Name:Himani Dave
Student Number:A00178443

QMM1002 Module 10 R Assignment

****************************************************************
"
#############								   	                  
#Question #1							
#############

att <- read.csv("ATTMonthlyReturn.csv", header = TRUE)

# a)

att.ts <- ts(data = att[,-1], start = c(1961, 1), end = c(1967, 12), frequency = 12)

# b) 

plot.ts(att.ts, xlab="Months since January 1961", 
        ylab="Returns", 
        main="AT&T's monthly returns from January 1961 to December 1967",  
        plot.type = "single", col=c("mediumorchid4"))

# c) 

plot(decompose(att.ts), col = c("navy"))

#Trend: The trend seems to fluctuate a lot. Overall, it seems stationary.

#Seasonal: The seasonal component is strong. 

#Cyclical: The returns follow a cyclical pattern with increase in some period and decrease
#in some.

#Irregularities: There are significant irregularities present in the data. 

# d) 

att.index <- att$Return/att[1, 2]*100

att.sub <- subset(att, att.index == max(att.index))

#The maximum monthly return when compared to January 1961 , was in October 1966.

# e) 

install.packages("TTR") 
library(TTR) 

att.ma5 <- SMA(att.ts, n=5)
att.ma8 <- SMA(att.ts, n=8)
att.ma10 <- SMA(att.ts, n=10)

# f)

plot.ts(cbind(att.ts, att.ma5, att.ma8, att.ma10),
        plot.type="s",  
        col=c("black", "red", "blue", "green"),
        ylab="Monthly Returns",    
        main="AT&T Monthly Returns") 

legend("top", legend=c("Data", "MA-5", "MA-8", "MA-10"), 
       col=c("black", "red", "blue", "green"), lty=1, cex=0.5)

# g) 

#Model MA-15 is the smoothest. MA-5 changes rapidly. Since MA-15 has a longer length,
#it is the smoothest. MA-5 has shorter length so it adjusts to the changes rapidly.

# h) 

ERRORS<-function(data, L){
  ma.data<-SMA(data, n=L)
  error<-NULL
  for (i in 1:length(data)-L){
    error[i]<-data[i+L]-ma.data[i+L-1]
  }
  error.p<-NULL
  for(i in 1:length(data)-L){
    error.p[i]<-abs(data[i+L]-ma.data[i+L-1])/abs(data[i+L])
  }
  MSE<-mean(error^2)
  MAD<-mean(abs(error))
  MAPE<-mean(error.p)*100
  error.df<-data.frame(errors=c(MSE, MAD, MAPE), row.names=c("MSE", "MAD", "MAPE"))
  return(error.df)
}

att.errors<-cbind(ERRORS(att.ts, 5),ERRORS(att.ts, 8),ERRORS(att.ts, 15))
colnames(att.errors)<-c("MA-5", "MA-8", "MA-15") 
att.errors 

#The MA-15 model is recommended.

# i)

att.ma5
att.ma10
att.ma8

#AT&T would exhibit a negative return.

# j) 

#In the goven scenario, the most accurate model would be MA-8.

