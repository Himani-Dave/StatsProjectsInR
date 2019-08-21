"
****************************************************************
Name: Himani Dave
Student Number: A00178443

QMM1002 Module 1 R Assignment
30/30
Very clearly written code Himani!
Great work on this assignment :)
****************************************************************
"

#############								   	                  
#Question #1		7/7					
#############
TVhoursweekly <- c(23, 22, 25, 14, 15, 23, 1, 21, 12, 14, 12, 9, 10, 23, 20, 20, 2, 29, 24, 22, 6, 3,2, 6, 23)

# a)
meanTV <- mean(TVhoursweekly) #The mean number of hours of TV watched per week is 15.24. 
stddev <- sd(TVhoursweekly) #The standard deviation for the number of hours of TV watched per week is 8.5062.

# b)
len <- length(TVhoursweekly)
SE <- stddev / sqrt(len) #The standard error is 1.7012

# c) 
df <- len - 1
#The t-statistic has 24 degrees of freedom as the sample size is 25.

# d)
ME <- SE*qt(0.99, df) #0.98/2 + 0.50 = 0 .99 
mean(TVhoursweekly) + c(-ME, ME)

#98% CI for mean number of hours of TV watched per week is (11.0002, 19.4798).

# e) Thus, we can be 98% confident that the avergae number of hours of TV watched by the population 
#lies between 11.0002 and 19.4798. On average, the number of hours of TV watched by the entire population
#is between 11 and 20. 

#############								   	                  
#Question #2						4/4	
#############

# a) Indepndence: Since this is a random sample, the data obtained is random and thus independent of any other data.
#Thus, it can be safely assumed that the independence condition is satisfied. 

# b) Random: Since the data weas gained from random individuals, the randomization considition is satisfied as well.

# c) The population is 1000 and the sample size is 25. Since 25 is less that 10% of 1000(i.e. 100)
# the 10% condition is satisfied. 

# d) 
hist(TVhoursweekly)
#The histogram shows that the data is unimodal and is not extremely skewed in either positiive or negative direction.
#Thus, it can be said that the data is reaosnably symmetric.  


#############								   	                  
#Question #3							5/5
#############

# a)  
len <- 43
qt(0.975, len-1)
#The critical t-va;ue is 2.0180.

# b) The critical t-value for 98% CI is 2.5 (from the t-distribution table). the degrees of freedom for the same is
# 23. Thus, the required sample size is 24.


#############								   	                  
#Question #4							
#############

# a) 
health.data <- read.csv("Health.csv", header = TRUE)

#Body temperature
hist(health.data$temperature)
#The histogarm is unimodal and is not very skewed in any one direction. Also, the sample size 129 > 40.
#Thus, all the nearly normal conditions seem to be satisfied for body temp.

#Heart rate
hist(health.data$heart.rate)
##The histogarm is unimodal and is not very skewed in any one direction. Also, the sample size 129 > 40.
#It appears pretty symmetric too. Thus, all the nearly normal conditions seem to be satisfied for heart rate.

# b)
len <- length(health.data$temperature)
SE <- sd(health.data$temperature) / sqrt(len)

# 90% CI
ME<-SE*qt(0.95, df=len-1)  #0 .95 = 0.90/2 + 0.50
mean(health.data$temperature)+c(-ME, ME)
#90% CI = (98.1539, 98.3687)
#Length of interval = 0.20873

# 95% CI
ME<-SE*qt(0.975, df=len-1)  #0 .975 = 0.95/2 + 0.50
mean(health.data$temperature)+c(-ME, ME)
#95% CI = (98.1397, 98.3889)
#Length of interval = 0.24928

# 98% CI
ME<-SE*qt(0.99, df=len-1)  #0 .99 = 0.98/2 + 0.50
mean(health.data$temperature)+c(-ME, ME)
#The 98% CI is (98.1159, 98.4127)
#Length of interval = 0.29679

#The sample size is the same for all the CI. Thus, the widest interval for body temp is 98%.

# c)
ME <- SE*qt(0.95, df=len-1)
#Thus, margin of error is 0.1043 for populaion.

# d)
# 95% CI
t.test(health.data$heart.rate, conf.level = 0.95)
#CI = (72.55697, 75.02443), Mean = 73.7907

#95% CI for male
heart.rate.male <- subset(health.data, gender == 1)
t.test(heart.rate.male$heart.rate, conf.level = 0.95)
# The CI is (71.946656, 74.89719), Mean = 73.42188
#Thus, for the given datset, males have a lower heart rate. 