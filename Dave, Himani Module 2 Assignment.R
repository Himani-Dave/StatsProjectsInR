"
****************************************************************
Name:Himani Dave
Student Number:A00178443

QMM1002 Module 2 R Assignment
23.5/25
Great work Himani!
Please see comments below in 2a and 3c.
****************************************************************
"
#############								   	                  
#Question #1							2/2
#############

# t= -2.77, df = 43
pt(-2.77, 43)
# p-value -> 0.004121906

#############								   	                  
#Question #2							6.5/7
#############

# a)

# Null Hypothesis, H0 : number of calls = 420
# Alternative Hypothesis, H1 : number of calls < 420
#state the parameter in the null and alternative 
# = mean number of calls -0.5 

# b)

tst_st <- (386-420)/(85/sqrt(30))
tst_st
# test statistic = -2.19089

# c)

# For alpha = 0.01
qt(0.01, 30-1)
#Critical value for t* = -2.4602021

# d)

# Here, |t*| > |t|, we fail to reject the null hypothesis.Thus, there's no evidence that 
# the worker's performance is declining. 

#############								   	                  
#Question #3							8/9
#############

donors <- read.csv("Donors.csv", header = TRUE)

# a)

t.test(donors$Wealth, mu = 5, alternative = "two.sided")
qt(0.05, 916-1)

#Here, we can see that |t*|<|t|. Thus, we reject the null hypothesis.
#This means,there is a significant difference between the mean value and the actual value of wealth.

#SIGNIFCANTLY DIFFERENT THAN 5

#Also, we are 95% confident that the true population wealth lies in the interval (5.161017, 5.51839)
#which is higher than 5.

# b)

p_val <- 2*pt(3.7438, 916-1, lower.tail = FALSE)
p_val

#p_val -> 0.0001925746

# c)
qt(0.05, 916-1)  #you must divide alpha by 2 for a two sided test, -1

#critical value of t -> -1.646521

############								   	                  
#Question #4							7/7
############

hist(donors$Age, xlab = "Age of the donor", ylab = "No of donors", main = "Avergae age of donors")

#The histogram is unimodal and almost symmetric. Thus, the nearly normal condition is satisfied. 

t.test(donors$Age, mu = 39, alternative = "greater", conf.level = 0.99)

#Here, the p-value = 2.2e-16 is less than alpha = 0.01.
#Thus, we reject the null hypothesis. It can be said that the average age of donors is greater than 39. 