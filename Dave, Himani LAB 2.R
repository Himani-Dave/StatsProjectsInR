"
****************************************************************
Name: Dave Himani 
Student Number: A00178443

QMM 1001 Lab 2
****************************************************************
"
#############								   	                  
#Question #1							
#############
crime.iowa <- read.csv("Criminal_Iowa.csv", header = TRUE)

#############								   	                  
#Question #2
#############

offense = table(crime.iowa$Convicting.Offense.Type)
print(offense)

#############								   	                  
#Question #3							
#############

relative.offense <- prop.table(offense)
print(relative.offense)

#############								   	                  
#Question #4							
#############

barplot(offense, main = "Number of Offenses by type", xlab = "Types of offenses", ylab = "Number of offenses", ylim = c(0, 8000),col = c("red", "blue", "green", "yellow", "pink") )

#############								   	                  
#Question #5							
#############

recidivism <- table(crime.iowa$Recidivism...Return.to.Prison)
relative.recidivism <- prop.table(recidivism)
pie(relative.recidivism, main = "Proportion of Recidivism")

#############								   	                  
#Question #6							
#############

install.packages("rpivotTable")
library(rpivotTable)
rpivotTable(crime.iowa)

#a) 33% of those going to prison intially committed a drug related offense.

#b) 74.3% of those that committed a violent crime did not return to prison. 