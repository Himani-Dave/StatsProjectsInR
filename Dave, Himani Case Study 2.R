"
****************************************************************
Name:Himani Dave
Student Number:A00178443

QMM1002 Case Study 2

***************************************************************
"

#Reading the data in

lakeerie <- read.csv("LakeErie.csv", header = TRUE)

#Making a time series object for the given data

lake.ts <- ts(lakeerie[, -c(1,2)], start = c(1967, 1), end = c(2016, 12), frequency = 12)

#Plotting the given data 

plot.ts(lake.ts, xlab = "Time", ylab = "Water Level", 
        main = "Water Levels in Lake Erie from 1967 to 2016",
        col = "mediumorchid4")
legend("bottomright", legend = "Water Levels", col = "mediumorchid4", lty = 1, cex = 0.8)

#The plot shows that the water levels in Lake Erie have been erratic and have seen some
#drastic changes over time. 

#Finding the average water level

avg.water.level <- mean(lakeerie$Monthly.Lake.Erie.Levels.1967.01.to.2016.12)

#The average water level in lake Erie from 1967 to 2016 was 14.9930.

#Decomposing the time series data. 

plot(decompose(lake.ts), col = "navy")

#Examining the four components we can see that:

#Trend: There was an upward trend near 1976 which dropped back down in about 1981.
#Since then the trend is a slightly increasing one.

#Seasonal: There is a definite seasonal component in the water levels.
#The water level seems to rise in the summer whereas it decreases during winter.

#Cyclical: There does not seem to be a cyclical component in the given data.

#Irregular: There are very few outliers with not many irregularities in the rest of the data

#Taking lengths 7, 12, and 15 to find the best moving average model and picking the one
#with the least error.

library(TTR)

lake.ma7 <- SMA(lake.ts, n = 7)
lake.ma12 <- SMA(lake.ts, n = 12)
lake.ma15 <- SMA(lake.ts, n = 15)

#Plotting them together to compare the smoothness of each

plot(cbind(lake.ts, lake.ma5, lake.ma7, lake.ma12), plot.type = "s",
     col = c("black", "red", "blue", "green"), ylab = "Water Levels",
     main = "Water levels of Lake Erie from 1967 to 2016")
legend("bottomright", legend = c("Data", "MA-7", "MA-12", "MA-15"), 
       col = c("black", "red", "blue", "green"), lty = 1, cex = 0.7)

#As evident from the graph, the smoothest curve is MA-15 while MA-12 and MA-7 capture
#more detailed changes in the water levels over time. 

#Comparing the errors in order to find the best model

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

lake.errors<-cbind(ERRORS(lake.ts, 7),ERRORS(lake.ts, 12),ERRORS(lake.ts, 15))
colnames(lake.errors)<-c("MA-7", "MA-12", "MA-15") 

#From the errors, we can see that the MA-12 model has the least value of errors. 
#Thus, the best moving average model would be MA-12. 

#Since the data has both seasonal and trend components, the Holt-Winter's Exponential
#Smoothing Model would be the best choice. 

#In order to find the model with the best fit, we would not specify the alpha, beta,
#and gamma components. 

#Applying the Holt-Winter's model to the data

lake.hw <- HoltWinters(lake.ts)
lake.hw

#From the results, it can be seen that alpha = 0.8964884, beta = 0.002994526, and
#gamma = 1. 

#Plotting the model 

plot(lake.hw, ylab = "Water Levels", main = "Holt-Winters Exponential Smoothing for Water Levels")
legend("bottomright", legend = c("Data", "Holt-Winters"), col = c("black", "red"), lty = 1, cex = 0.8)

#Thus, the Holt-Winters model follows the given data.

#The errors can be looked at to check the accuracy of the model 

accuracy(forecast(lake.hw))

#The percentage error is only about 2.3%. Thus, the model is pretty accurate. 

#Since the data has both seasonal and trend components, the moving averages method would
#not be appropriate to predict the future values. The best model to predict the future
#water levels would be Holt-Winters Exponential Smoothing model.

#Forecasting the water levels for the next five months

lake.fitted <- lake.hw$fitted[,1]
lake.fitted

lake.next.5 <- forecast(lake.hw, h = 5)
lake.next.5
plot(lake.next.5, xlab = "Time", ylab = "Water Levels in tens of metres")

#The forecast of the water levels for the next 5 months can be seen at both 80% confidence
#interval and 95% confidence interval.

#To assess the quality of the model, a plot can be created 

plot(lake.next.5$residuals, ylab = "Residuals", main = "Residulas for Lake Erie Data")
abline(h=0, col = "red", lty = 2)

#It can be seen from the plot that the residulas are pretty balanced with a few outliers.
#They do not seem to form any pattern. Thus, the model chosen is a valid one.

#Taking a random sample of 100 water levels. 

library(dplyr)
random.lake <- sample_n(lakeerie, 100)

random.lake <- random.lake %>%
  mutate(Season = case_when((Month >0 & Month < 4) ~ 'Winter',
                            (Month >3 & Month < 7)  ~ 'Spring',
                            (Month >6 & Month < 10) ~ 'Summer',
                            TRUE ~ 'Fall'))

rand.final <- random.lake[,c(3,4)]
rand.final$Level <- ifelse(rand.final$Monthly.Lake.Erie.Levels.1967.01.to.2016.12 >= 15, "High", "Low")

#Taking the observed and expected values to see if the water levels are dependent on the 
#seasons or not. Chi-square test needs to be performed for that. 

#Checking conditions for chi-square test:

# 1.Counted Data Condition: Since we count the number of observed values that fall at an 
#intersection of a season and a level, this condition is met. 

# 2.Independence Condition: the samples are taken randomly hence, they are indpendent.

# 3.Randomization Condition: Since the samples are randomly taken, this condition is satisfied. 

# 4. Sample size Condition: There are a 100 samples so this condition is satisified. 

#Null Hypothesis, H0: Season and level are independent
#Alternative Hypothesis, HA: Season and level are not independent. 

observed <- table(rand.final$Season, rand.final$Level)

chisq.test(observed)

#Since the p-value (0.2355) > alpha (0.05), we fail to reject the null. 
#Thus, the season and level are not independent of each other. 

#Plotting differences in High-Low and different seasons. 

library(reshape2)
library(ggplot2)

o<-data.frame(melt(observed, value.name = "Count"),               
              Distribution=rep("Obs", length(observed)))
e<-data.frame(melt(expected, value.name = "Count"),               
              Distribution=rep("Exp", length(expected)))
reshape<-rbind(o, e)
colnames(reshape)<-c("Level", "Seasons", "Count", "Distribution")
ggplot(reshape, aes(Distribution, Count, fill=Seasons))+  geom_bar(stat="identity")+  facet_grid(~Level)+  
  labs(title="Chi-Square Analysis for Seasons")

#From the plot it can be seen that there are quite a few differences in the observed and
#expected values. The observed values are lower for Fall, Spring, and Summer, while the 
#expected values are lower for Winter.  




