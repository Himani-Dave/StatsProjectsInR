"
****************************************************************
Name:Himani Dave
Student Number:A00178443

QMM1002 Module 3 R Assignment

****************************************************************
"
#############								   	                  
#Question #1							
#############

# a)

US <- c(18, 15, 16, 16, 17, 15, 14, 16, 16, 15)
Japan <- c(24, 27, 27, 25, 31, 30, 24, 19, 28, 23)

# Null Hypothesis, H0: Mileage of Japanese cars = Mileage of US cars [mu(Japan) - mu(US) = 0]
# Alternative Hypothesis, HA: Mileage of Japanese cars > Mileage of US cars [mu(Japan) - mu(US) > 0]

# b)  

#Independence and randomization: Since the sample was selected randomly from the population, 
#it can safely be said that the values are random and independent from one another.
#Thus, these two conditions are satisfied.

# 10% condition: 10 <is less than 10% of the population. Thus, this condition is satisfied. 

#Nearly normal condition:
hist(US, breaks = 5)
hist(Japan, breaks = 5)

#Japan's histogram is bimodal whereas that of US is unimodal. 

# c)

t.test(Japan, US, var.equal = FALSE, paired = FALSE, conf.level = 0.99, alternative = "greater")

# t-statistic = 8.4785

# d) 

qt(0.01, 10.819, lower.tail = FALSE)

# t* = 2.725631

# e) Here, |t*| < |t| , so we rejact the null hypothesis. 
# Thus, the mielage given by Japanese cars is better than that given byh US cars.

#############								   	                  
#Question #2							
#############

realestate <- read.csv("RealEstate.csv", header = TRUE)

# a) 

realestate$Old <- ifelse(realestate$Age > 30, "Old", "New")

# b)

oldhouse <- subset(realestate, Old == "Old")
newhouse <- subset(realestate, Old == "New")

#Null Hypothesis, H0 : mu(oldhouse) - mu(newhouse) = 0
#Aletrnate Hypothesis, HA: mu(oldhouse) - mu(newhouse) != 0

t.test(oldhouse$Price, newhouse$Price, var.equal = FALSE, paired = FALSE, conf.level = 0.95, alternative = "two.sided")

#p-value < alpha, thus we reject the null hypothesis. 
#Hence, the mean price of old and new houses are significantly different.

# c) 

t.test(oldhouse$Price, newhouse$Price, var.equal = FALSE, paired = FALSE, conf.level = 0.95)

# The 95% confidence interval is: (131116.7, 177136.1)

# d)

install.packages("ggplot2")
install.packages("Hmisc")
library(ggplot2)
library(Hmisc)

ggplot(realestate, aes(realestate$Old, Price))+  stat_summary(fun.y=mean, geom="bar", fill="orchid", colour="black")+ 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+  
  labs(x="Houses", y="House Prices", title="Mean prices of new and old houses")