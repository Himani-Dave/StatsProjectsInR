"
****************************************************************
Name: Himani Dave
Student Number: A00178443

QMM1002 Module 1 R Assignment

****************************************************************
"

#############								   	                  
#Question #1					
#############

# a)

#The data is collected at different time periods, however, the same diagnostic tests have been
#used on it. Thus, it can be said that the data is paired.

# b)

#The data is collected randomly, hence all the values are independent of each other.

#############								   	                  
#Question #2					
#############

# a)

#Null Hypothesis , H0 : mu(day) - mu(afternoon) = 0
#Alternative Hypothesis, HA: mu(day) - mu(afternoon) != 0

# b)

days <- c(5,8,7,6,9,7)
afternoons <- c(8,10,7,11,9,12,14,9)

# Equal variances
t.test(afternoon, day, alternative="two.sided", var.equal=TRUE, paired=FALSE, conf.level=0.99)

#t test statistic = 2.8372, p-value = 0.01497

# c)

qt(0.01/2, length(afternoon) + length(day) - 2)
qt(0.01/2, length(afternoon) + length(day) - 2, lower.tail = FALSE )

# t* = 3.05

#d)

#Fail to reject null becuase |t*| > |t|. Thus, no significant difference between mean defeccts 
# in days and afternoons

# e)

t.test(afternoon, day, alternative="two.sided", var.equal=FALSE, paired=FALSE, conf.level=0.99)

#t test statistic = 3.0364, p-value = 0.01059
#Fail to reject null because p-value > alpha

# f) 

#There is no difference in the decision for the two tests. 
#The null hypothesis fails to be rejected in both the cases.
#Thus, it is safe to assume that the variances are equal.

#############								   	                  
#Question #3							
#############

store.profits <- read.csv("Store.Profits.csv",  header = TRUE)

#Sample groups are dependent on each other. Thus, paired t-test has to be used. 

#Null Hypothesis, Ho: mu(difference) = 0
#Alternative Hypothesis, HA: mu(difference) != 0

#Paired Data Assumption: Holds, as the data is from the same store for two different years.

#Independence: Holds, as behaviour of one store does not depend upon that of another

#Randomization: Holds, it's a random sample

##10% Condition: 15 < 10% of all the stores

#Nearly Normal:
store.profits$DiffInSales <- store.profits$Sales.Year.1 - store.profits$Sales.Year.2
hist(store.profits$DiffInSales)

#Unimodal and almost symmetric

t.test(store.profits$DiffInSales, mu=0, alternative="two.sided")

#Fail to reject the null as p-value > alpha. Thus, sales have been constant over the past two years.

# b)

#Sample groups are dependent on each other. Thus, paired t-test has to be used. 

#Null Hypothesis, Ho: mu(difference) = 0
#Alternative Hypothesis, HA: mu(difference) != 0

#Paired Data Assumption: Holds, as the data is from the same store for two different years.

#Independence: Holds, as behaviour of one store does not depend upon that of another

#Randomization: Holds, it's a random sample

##10% Condition: 15 < 10% of all the stores

#Nearly Normal:
store.profits$DiffInProfits <- store.profits$Profits.Year.2 - store.profits$Profits.Year.1
hist(store.profits$DiffInProfits)

#Unimodal and nearly normal

t.test(store.profits$DiffInProfits, mu=0, alternative="greater")

#Reject the null as p-value < alpha. Thus, year 2 saw higher profits than year 1. 

# c)

profits.plot<-data.frame(Profit=c(store.profits$Profits.Year.1, store.profits$Profits.Year.2, store.profits$DiffInProfits),Group=c(rep("Year_1_Profits", 15), rep("Year_2_Profits", 15), rep("Difference.in.profits", 15)), Data.Pair=c(1:15, 1:15, 1:15))

library(ggplot2)
ggplot(profits.plot, aes(factor(Data.Pair), Profit, fill=Group))+ geom_bar(stat="identity", position="dodge")+labs(x="Stores", y="Profit(in dollars)")+ggtitle("Comparison of two years' profits of 15 stores")