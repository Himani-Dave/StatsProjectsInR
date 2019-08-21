"
****************************************************************
Name:Himani Dave
Student Number:A00178443

QMM1002 Module 8 R Assignment
****************************************************************
"
#############								   	                  
#Question #1							
#############

card <- 1:13
count <- c(27, 17, 46, 21, 71, 79, 168, 95, 69, 97, 40, 53, 17)

# a) 

#Null Hypothesis, H0: There is no difference in the number of applications for various cards.
#Alternative Hypothesis, HA: The number of applications for various cards is different.

# b) 

expected <- rep(sum(count)/length(count), length(count))

#The expected number of applications per card under the null hypothesis is 61.5.

# c) 

chisq.test(x=count, p=expected, rescale.p = TRUE)

#X-statistuc = 353.33

#d) 

qchisq(0.01, df = length(count) - 1, lower.tail = FALSE)

#At 0.01 significance level, the critical chi-value is 26.217.

# e) 

#The X-squared value is much greater than the critical chi-value for alpha = 0.01.
#Thus, we will reject the null hypothesis.
#This means that the number of applications for different types of cards is different. 

# f) 

residuals <- chisq.test(x=count, p=expected, rescale.p = TRUE)$residuals
residuals

# g) 

resid.data <- data.frame(Card = c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
                                  "10", "11", "12", "13"), residuals = residuals)

library(ggplot2)
ggplot(resid.data, aes(Card, residuals, fill=Card))+
  geom_bar(stat="identity")+
  labs(x="Card", y="Standardized Residuals")+
  ggtitle("Residuals for Cards")+
  geom_hline(yintercept=c(-3, 3), linetype="dashed", color = "red")

#From the graph, we can see that cards 1, 13, 2, and 4 have less number of applications than
#the average number of applications is more than 3 standard deviations away from the mean.
#This means the numbers are unusually low. The cards 11, 12, 3 are within 3 standard deviations
#of avregae but lower. Cards, 5,6, and 9 are within 3 standard deviations but higher than the
#average. 10, 7, and 8 are greater than 3 standard deviations above the average, 
#which means they are uniusally high. Number 7 is farthest from the mean in the positive direction.
#Thus, I would recommend the company to market card number 7. 


#############								   	                  
#Question #2							
#############

observed <- c(315, 101, 108, 32)


# a) 

#Null Hypothesis, H0: The pea plants grow in the ratio 9:3:3:1 for yellow round:yellow wrinkled:green round:green wrinkled
#Alternative Hypothesis, HA: The pea plants grow a ratio different than 9:3:3:1 for yellow round:yellow wrinkled:green round:green wrinkled

# b) 

#The expected number of each phenotype is:

expected <- c(556*(9/16), 556*(3/16), 556*(3/16), 556/16)

# c) 

chisq.test(x = observed, p = expected, rescale.p = TRUE)
qchisq(0.05, df = (4-1), lower.tail = FALSE)

#Here, since chi-value is less than critical chi-value, we fail to reject the null.
#Thus, we can say that the pea plants grow in the ration 9:3:3:1 for 
#yellow round:yellow wrinkled:green round:green wrinkled

# d) 

pea.plants<-data.frame(Plant.Type=rep(c("Yellow Round", "Yellow Wrinkled", "Green Round", "Green Wrinkled"), 2), 
                     Counts=c(observed, expected), 
                     Distribution=c(rep("observed", 4), rep("expected", 4)))

ggplot(pea.plants, aes(Plant.Type, Counts, fill=Distribution))+
  geom_bar(stat="identity", position="dodge")+
  labs(x="Type of Pea Plant", y="Number of plants produced")+
  ggtitle("Number of pea plants produced by type")

#The plot confirms that the observed and the expected number of plants produced are almost
#the same. 