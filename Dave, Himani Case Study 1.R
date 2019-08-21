#Reading in the given data

rockland <- read.csv("Rockland.csv", header = TRUE)

#Basic one-sample t-test on List Prices to find out the confidence interval of mean


#Checking Assumptions:
# 1. Independence assumption: Since the houses are randonly selected, they are indpendent 
# of each other. Thus,this condition is met. 

# 2. Randomization condition: Since the houses are randomly selected, this condition is
# satisfied. 

# 3. 10% condition: 169 homes are less than the total number of homes in Rockland. Thus,
# this condition is satisfied. 

# 4. Nearly Normal Condition:

hist(rockland$List.Price, col = 'slateblue3', border = 'violetred', xlab = 'List Prices', 
     ylab = 'Number of homes',
     main = 'Histogram of List Prices vs Number of homes')

#From the histogram, we can see that the data for List Price is skewed to the right. 
#This indicates that there are a few outliers. From the data, we can see that the prices 
#for two of the houses in Rockland are $1289000 and $1338000. They seem to be the outlier.
#However, since the sample size is large, t-tests can be applied. 

# One-sample t-test:

t.test(rockland$List.Price, conf.level = 0.95)

#From the t-test, we can be 95% confident that the real mean for the list prices in 
#Rockland lies between $306133.7 and $355607.1.

#The budget of the family is $350000. 

#A hypothesis test can be carried out to check whether the family can afford a home
#in Rockland.

#Hypotheses:
#Null Hypothesis, H0: mu(list.price) = 350,000
#Alternate Hypothesis, HA: mu(list.price) < 350,000

#The conditions have already been checked. 

t.test(rockland$List.Price, mu = 350000, alternative = 'less', conf.level = 0.95)

#From this, we can see that the p-value is more than 0.05. 
#So, we can reject the null hypothesis which means, there is evidence that the mean 
#of list prices of homes in Rockland is less than 350000.

#Thus, the family can afford a home in Rockland.

#Comparing different types of homes

#There are four types of homes: Apartment, House, Mobile, Row/Townhouse
#In order to compare the mean prices of four homes, one-way anova can be used

#Hypotheses
#Null Hypothesis, H0: There is no difference in the prices of different types of homes.
#Alternate Hypothesis, HA: The list price of at least one type of home is different. 

type.anova <- aov(List.Price~Type, data = rockland)
summary(type.anova)

#Here, the p-value is much less than alpha, so we reject the null.
#Thus, we can say that at least one of the type of home has a different mean list price. 

#Checking the conditions for one-way anova:

#Independence assumption: The price of one home does not affect the price of others.
#Randomization condition: The homes are randomly selected. Hence, this condition is met.
#Independent group assumption: The list prices of one group do not affect that of others.

#Equal variance

boxplot(List.Price~Type, data = rockland,
        main = 'Comparison of Types of home',
        ylab = 'Number of homes', xlab = 'List Prices',
        col = c('violetred4', 'slateblue4', 'springgreen4', 'papayawhip'))

#All types seem to have pretty close means. Mobile homes seem to have a lower mean 
#than the others. There is no extreme skewness. 

plot(type.anova)

#The Residuals vs Predicted Plot shows the red line close to the equal spread line. 
#The normal Q-Q plot follows a straight line with a few outliers. 

hist(type.anova$residuals, main = 'Histogram of residuals', xlab = 'Residuals')

#The histogram is nearly normal with a few outliers.
#Thus, the one-way anova holds. 

#Tukey's HSD test can be applied to find out which type of homes are different.

TukeyHSD(type.anova, conf.level = 0.95)

#Here, we can see that the p-values for Mobile-Apartment, Mobile-House, and 
#Row/Townhouse-House are different. Thus, we can say that the mean list price of Mobile
#homes is different from the rest. 

library(ggplot2)
library(Hmisc)

ggplot(rockland, aes(Type, List.Price, fill=Type))+  
  stat_summary(fun.y=mean, geom="bar")+  
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+  	
  labs(x="Type", y="List Price", title="List Prices of different types of home")+  	
  scale_fill_brewer(palette="Set3")

#Comparing prices in different areas

clr.rock <- subset(rockland, Area == 'Clarence-Rockland')
rock <- subset(rockland, Area == 'Rockland')

#The mean prices of different areas can be compared using t-test

#Hypotheses
#Null Hypothesis, H0: mu(List Prices of home in Clarence-Rock)-mu(List Prices of home in Rock) = 0
#Alternate Hypothesis, HA: mu(List Prices of home in Clarence-Rock)-mu(List Prices of home in Rock) != 0

#Assumptions and conditions

#Independence Assumption: The data is drawn independently for both areas.
#Randomization Condition: The homes are randomly selected in both areas.
#10% condition: Both 38, and 131 are less than the total number of homes in both areas.
#Nearly Normal condition:

hist(clr.rock$List.Price, col = 'slateblue3', border = 'violetred', xlab = 'List Prices', 
     ylab = 'Number of homes',
     main = 'Histogram of List Prices of homes in Clarence-Rockland')

#The histogram is unimodal, symmetric, and nearly normal. 

hist(rock$List.Price, col = 'slateblue3', border = 'violetred', xlab = 'List Prices', 
     ylab = 'Number of homes',
     main = 'Histogram of List Prices of homes in Rockland')

#The histogram is nearly normal with a few outliers.

#Independent Groups Assumption: The two groups are independent of each other asthe homes
#are randomly selected. 

t.test(clr.rock$List.Price, rock$List.Price, 
       alternative = "two.sided",
       var.equal = FALSE,
       paired = FALSE,
       conf.level = 0.95)

#Here, p-value is greater than alpha (0.05). Thus, we fail to reject null.
#Thus, it can be said that the mean list price in both the areas is not significantly
#different from each other. 

#Assuming that the family of four includes parents and 2 children,
#they might need a three bedroom home. 

#Checking for types of homes with 3 bedrooms. 

three.bed <- subset(rockland, Number.of.Bedrooms == 3)

#A one-way anova needs to be conducted for this

#Hypotheses
#Null Hypothesis, H0: There is no difference in the prices of different types of homes with three bedrooms.
#Alternate Hypothesis, HA: The list price of at least one type of home with three bedrooms is different.

three.bed.anova <- aov(List.Price~Type, data = three.bed)
summary(three.bed.anova)

#Here, p-value is less than alpha. Thus, we reject the null.
#We have enough evidence to say that the price of at least one type of home is different with three bedrooms.

#Checking the conditions for one-way anova:

#Independence assumption: The price of one home does not affect the price of others.
#Randomization condition: The homes are randomly selected. Hence, this condition is met.
#Independent group assumption: The list prices of one group do not affect that of others.

#Equal variance

boxplot(List.Price~Type, data = three.bed,
        main = 'Comparison of Types of home with three bedrooms',
        ylab = 'Number of homes', xlab = 'List Prices',
        col = c('violetred4', 'slateblue4', 'springgreen4', 'papayawhip'))

#All types seem to have pretty close means. Mobile homes seem to have a lower mean 
#than the others. There is no extreme skewness. 

plot(three.bed.anova)

#The Residuals vs Fitted Plot shows the red line close to the equal spread line. 
#The normal Q-Q plot follows a straight line with a few outliers. 

hist(three.bed.anova$residuals, main = 'Histogram of residuals', xlab = 'Residuals')

#The histogram is nearly normal with a few outliers.
#Thus, the one-way anova holds. 

#Tukey's HSD test can be applied to find out which type of homes are different.

TukeyHSD(three.bed.anova, conf.level = 0.95)

#Here, we can see that the p-values for Mobile-House and 
#Row/Townhouse-House are different. Thus, we can say that the mean list price of House
#with three bedrooms is different from the rest. 

library(ggplot2)
library(Hmisc)

ggplot(three.bed, aes(Type, List.Price, fill=Type))+  
  stat_summary(fun.y=mean, geom="bar")+  
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+  	
  labs(x="Type", y="List Price", title="List Prices of different types of home with 3 bedrooms")+  	
  scale_fill_brewer(palette="Set2")

#Thus, the houses with three bedroom have a different price range than other types. 
