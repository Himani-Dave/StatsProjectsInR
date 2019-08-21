"
****************************************************************
Name:Himani Dave
Student Number:A00178443

ANA1002 Module 6 R Assignment
****************************************************************
"
#############								   	                  
#Question #1							
#############

placebo <- c(81, 80, 72, 82, 83, 89, 76, 88, 83)
mg.50 <- c(92, 86, 87, 76, 80, 87, 92, 83, 84)
mg.100 <- c(86, 93, 97, 81, 94, 89, 98, 90, 91)
drug.data <- data.frame(placebo, mg.50, mg.100)
treatments <- stack(drug.data)
colnames(treatments) <- c("values", "types")

# a) Null hypothesis, H0: mean(placebo) = mean(mg.50) = mean (mg.100)
# Alternative hypothesis, HA : No two means are the same

# b)
drug.data.anova <- aov(values~types, data = treatments)
summary(drug.data.anova)

# c)
qf(0.05, df1=2, df2=24, lower.tail = FALSE)

# d)

#Here, p-value < alpha. Thus, we reject the  null hypothesis. Hence,  at least one of the means is different.

# e)

TukeyHSD(drug.data.anova, conf.level=0.95)

#From Tukey's HSD test, we can say that the means for placebo and 100 mg is different.

#############								   	                  
#Question #2							
#############

finance <- read.csv("FinanceCanada.csv", header = TRUE)

# a) Null Hypothesis, H0: mean(Type 1) = mean(Type 2) = mean(Type 3) = mean(Type 4) = mean(Type 5) = mean(Type 6) = mean(Type 7)
# Alternate Hypothesis, HA: at least one pair of means is different.

# b)

fin.anova<- aov(finance$DividendPerShare~as.factor(finance$TypeOfIndustry), data = finance)
summary(fin.anova)

#Here. p-val < alpha. Thus, we reject the null hypothesis. 
#Hence, it can be said that the mean dividend is different for at least one of the pair of industries. 

# c)
 
#Indpendence and Randomization conditions: Since the companies are chosen at random, they are independent as well. 
#Thus, both the conditions are met.
#Indpendent Group Assumption: The industries are chosen randomly. 

boxplot(finance$DividendPerShare~as.factor(finance$TypeOfIndustry), data = finance, main="Dividends per share (by industry type)", ylab="Dividend per share", xlab="Industry Type")

plot(fin.anova)

#Normal population assumptions: The boxplot shows that the data is not too skewed. 

hist(fin.anova$residuals,main="Residuals Histogram",xlab="Residuals")

#The histogram is nearly normal and is unimodal.

# d)
install.packages("Hmisc")
library(Hmisc)
ggplot(finance, aes(as.factor(finance$TypeOfIndustry), finance$DividendPerShare, fill=as.factor(finance$TypeOfIndustry)))+ stat_summary(fun.y=mean, geom="bar")+ stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+
  labs(x="Industry Types", y="Dividend per share", title="Mean dividend per share by industry")+scale_fill_brewer(palette="Spectral")