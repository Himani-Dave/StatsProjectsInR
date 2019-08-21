"
****************************************************************
Name: Dave Himani 
Student Number: A00178443

QMM 1001 Lab 6
****************************************************************
"

#############								   	                  
#Question #1							
#############
  
# a)

x <- 1:6
prob.x <- c(0.01477, 0.09900, 0.25800, 0.34200, 0.22600, 0.06023)
rbind(x, prob.x)

# b)

sum(x * prob.x)
sum((x - sum(x * prob.x))^2*prob.x)

# c)

#5 times
s5 <- sample(x, 5, prob = prob.x, replace = TRUE)
table(s5)
barplot(table(s5))

#500 times
s500 <- sample(x, 500, prob = prob.x, replace = TRUE)
table(s500)
barplot(table(s500))

#50000 times
s50000 <- sample(x, 50000, prob = prob.x, replace = TRUE)
table(s50000)
barplot(table(s50000))

# d)

#5 times
mean(s5)
var(s5)

#500 times
mean(s500)
var(s500)

#50000 times
mean(s50000)
var(s50000)

#They seem to approach a normal distribution.

#############								   	                  
#Question #2							
#############

# a) Exactly 50 people have high blood pressure = 0.01485

dbinom(50, 200, 0.20)

# b) Less than 70 people have high blood pressure = 0.9999

pbinom(69, 200, 0.20)

# c) More than 25, but less than 50 people have high blood pressure = 0.9958

pbinom(49, 200, 0.20) - pbinom(25, 200, 0.20)

# d) More than 160 people do not have high blood pressure = 0

1 - pbinom(160, 200, 0.20, FALSE)