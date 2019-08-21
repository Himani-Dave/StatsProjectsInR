"
****************************************************************
Name: Himani Dave
Student Number: A00178443

QMM1002 Module 9 R Assignment

****************************************************************
"

#############								   	                  
#Question #1					
#############

males <- c(315, 225, 300, 360)
females <- c(190, 160, 210, 240)

observed <- rbind(males, females)
colnames(observed) <- c("Business", 'Performing Arts', "Computer Technology", "Health Sciences")


# a) 

#Null Hypothesis, H0: Males and females were accepted at the same rate in all the programs.
#Alternate Hypothesis, HA: Males and femlaes were accepted at different rates in thr programs.

# b) 


row.total<-apply(observed, 1, sum)
column.total<-apply(observed, 2, sum)
total<-sum(observed)

expected<-outer(row.total, column.total)/total

# c) 

chisq.test(observed)

#The chi-square test statistic is 1.871847

# d) 

qchisq(0.05, df=(4-1)*(2-1), lower.tail=FALSE)

#The critical chi-square value is 7.814728

# e) 

#As chi-squared test satistic is less than the critical chi-square value,
#we fail to reject the null. hence, there is significant evidence that males and females
#were accepted at different rates in the programs. 

# f) 

residuals.prog <- chisq.test(observed)$residuals
residuals.prog

#It can be seen that in programs like business, the acceptance of males is higher whereas 
#that of females is higher in both Performing arts and Computer Technology. 

# g) 

assocplot(observed, xlab="Gender", ylab="Program",          
          main="Association Plot for Programs")

#The most unusual positive and negative residulas are for the Business program for males
#and females respectively. 

# h) 

mosaicplot(observed, shade=TRUE,            
           xlab="Gender", ylab="Program",           
           main="Mosaic Plot for Programs")

#None of the residulas are in the unusual range. 

# i) 

library(reshape2)
library(ggplot2)
o<-data.frame(melt(observed, value.name = "Count"),               
              Distribution=rep("Obs", length(observed)))
e<-data.frame(melt(expected, value.name = "Count"),               
              Distribution=rep("Exp", length(observed)))
prog.reshape<-rbind(o, e)
colnames(prog.reshape)<-c("Gender", "Program", "Count", "Distribution")
ggplot(prog.reshape, aes(Distribution, Count, fill=Gender))+  
  geom_bar(stat="identity")+  facet_grid(~Program)+  
  labs(title="Chi-Square Analysis for Programs")

#We can see that for all programs except Health Sciences, there are differences
#in the observed and expected values of the different programs. 
