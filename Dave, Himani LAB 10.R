"
****************************************************************
Name: Dave Himani 
Student Number: A00178443

QMM 1001 Lab 10
****************************************************************
"

#############								   	                  
#Question #1							
#############

one.sided.test<-function(p0, p.hat, n, alpha){
  q0<-1-p0
  sd<-sqrt(p0*q0/n)
  z.test<-(p.hat-p0)/sd
  z.test<-ifelse(z.test<0, z.test, -z.test)
  p.value<-pnorm(z.test)
  decision<-ifelse(p.value<alpha, "Reject the null hypothesis", "Do not reject the null hypothesis")
  return(c(p.value, decision))
}

# a)

one.sided.test(0.92, 283/1000, 300, 0.05)
#The p-val = 0.068 which is greater than alpha = 0.05. Thus, we do not reject the null hypthesis.
# b)

one.sided.test(0.92, 283/1000, 300, 0.10)
#The p-val = 0.068 which is less than alpha = 0.10. Thus, we reject the null hypotheis.
#We change the decision as we change the significance level from 5% to 10%.

#############								   	                  
#Question #2							
#############

sdtwo <- function(p0, p.hat, n, alpha){
  q0 <- 1 - p0
  sd <- sqrt((p0 * q0)/n)
  z.score <- (p.hat - p0)/sd
  z.score <- ifelse(z.score < 0, z.score, -z.score)
  p.val <- 2 * pnorm(z.score)
  result <- ifelse(p.val < alpha, "Reject null hypothesis", "Do not reject null hypothesis")
  return(c(p.val, result))
}

sdtwo(0.30, 0.25, 500, 0.05)
# p-value = 0.014 which is less than alpha = 0.05, thus we reject the null hypothesis.