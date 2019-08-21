"
****************************************************************
Name: Dave Himani 
Student Number: A00178443

QMM 1001 Lab 9
****************************************************************
"

#############								   	                  
#Question #1							
#############

CI<-function(cl, p, n){
  q=1-p  
  se<-sqrt(p*q/n) 
  z.crit<-qnorm(cl/2+0.5)  
  me<-z.crit*se 
  lower<-p-me 
  upper<-p+me 
  return(c(lower, upper))  
}

# a)
CI(0.95, 288/350, 350)

# b)
CI(0.85, 288/1400, 1400)

# c) The width of the confidence interval decreases by half as we increase the sample
#sixe to 4 times the given sample size.

#############								   	                  
#Question #2							
#############

sample.size <- function(p, me, cl){
  q <- 1 - p
  z.crit <- qnorm(cl/2+0.5)
  size <- (z.crit*z.crit*p*q)/(me*me)
  return(ceiling(size)) #To round up the number
}

sample.size(0.25, 0.05, 0.95) #289 people from the younger age group need to be surveyed.