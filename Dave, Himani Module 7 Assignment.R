"
****************************************************************
Name:Himani Dave
Student Number:A00178443

QMM1002 Module 7 R Assignment
****************************************************************
"
#############								   	                  
#Question #1							
#############

grass <- read.csv("Grass.csv", header = TRUE)

# a)

#Null Hypotheses, Ho: mean number of blades is same for each species; mean number of fertilizer
#is same for each species; different types of fertlizers have a consistent effect on each species
#Alternate Hypothesis, Ha: The mean number of blades is different for at least one region

# b) 

# It is a factorial design experiment. 

# c) 

#There are six different treatment groups.

# d) 

# The number of blades of grass per square inch is the response variable. 

# e) 

boxplot(Blades~as.factor(grass$Species), grass, col=c("cadetblue", "darkgreen"), 
        ylab="No of blades of grass per square inch", 
        main="No of blades of grass per square inch (by species)")

boxplot(Blades~as.factor(grass$Fertilizer), grass, col=c("cadetblue", "firebrick3","darkgreen"), 
        ylab="No of blades of grass per square inch", 
        main="No of blades of grass per square inch (by type of fertilizer)")

boxplot(Blades~as.factor(grass$Fertilizer)*as.factor(grass$Species), grass, 
        col=c("cadetblue", "firebrick3","darkgreen"), 
        ylab="No of blades of grass per square inch", 
        main="No of Blades of grass per square inch (by Type of Fertilizer and Species)")

#From the boxplots, we can conclude that species 1 has higher growth of grass blades per square inch 
#than species 2. Fertilizer 3 has the higher effectiveness than other type of fertlizers. Thus, 
#it can be concluded that the highest growth will result when fertiliser 3 is used with species 1
#and the lowest growth results when fertlizer 2 is used with species 2.

# f)
grass.ano <-aov(Blades~as.factor(grass$Fertilizer)*as.factor(grass$Species), data=grass)
summary(grass.ano)

#Thus, we will reject the null for both species and fertlizers as there is a significant difference
#between different types of species as well as different types of fertlisers. We will not reject the null
#for interaction of them, as the effects seem to be pretty consistent. 

# g) 

#Variance Homogenity 
# 1 - Boxplot 
boxplot(Blades~as.factor(grass$Fertilizer)*as.factor(grass$Species), grass, 
        col=c("cadetblue", "firebrick3","darkgreen"), 
        ylab="Numbers of Blades of grass per square inch", 
        main="Number of Blades of grass per square inch by Type of Fertilizer and Species")
#The variations are different for blades of grass per square inch (by type of fertliser and speices) 
#as observed from the boxplot.

# 2 - Residuals vs. Predicted Plot

plot(grass.ano)

# The red and the straight lines are very close. 

# Normal population - Extreme skewness cannot be observed in the plot. 

# Histogram of residuals 
hist(grass.ano$residuals,
     main="Histogram of Residuals",
     xlab="Residuals")

#The histogram is nearly normal 

# QQ plot - nearly a straight line with a few outliers

# h)

interaction.plot(as.factor(grass$Fertilizer), as.factor(grass$Species), grass$Blades,
                 xlab="Types of Fertilizers", ylab="No of blades of grass", 
                 main="Interaction Plot", trace.label="Species", 
                 xpd=FALSE)

#The number of blades are highest for species 1 with fertilizer type 3 and the lowest is for 
#species 2 with fertlizer type 2. Different fertlizers have different effects on both species of grass. 
#Species 1 has higher no of blades with all fertlizers as compared to species 2. 

# i)

TukeyHSD(grass.ano, conf.level=0.95)

#The result when using fertlizer 3 fro species 1 is significantly different from the others combinations.

# j) The best combination would be species 1 and fertilizer 3.

# k) 

library(ggplot2)
library(Hmisc)
ggplot(grass, aes(as.factor(grass$Fertilizer), Blades))+
  stat_summary(fun.y=mean, geom="bar", fill="cadetblue")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3)+
  labs(x="Types of Fertilizer", y="No of blades per square inch", title="Effect of type of fertilizer on grass")
#Fertlizer 3 grows highest number of grass blades per square inch.

ggplot(grass, aes(as.factor(grass$Species), Blades))+
  stat_summary(fun.y=mean, geom="bar", fill="cadetblue")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3)+
  labs(x="Species", y="No of Blades per sq.in.", title="Effect of Species of Grass on Blades of Grass grown")
#The grass of species 1 grows more than that of species 2. 

ggplot(grass, aes(as.factor(grass$Species), Blades, fill=as.factor(grass$Fertilizer)))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", position="dodge")+
  labs(x="Species", y="Blades of grass per square inch")+
  ggtitle("Effect of species and type of fertilizers on grass")

#Species 1 grass grows more than that of species 2. Fertilizer 3 is the most effective of all fertlizers.
#The best combination is species 1 and fertlizer 3. 




