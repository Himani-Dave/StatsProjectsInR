"
****************************************************************
Name: Dave Himani 
Student Number: A00178443

QMM 1001 Lab 7
****************************************************************
"

#############								   	                  
#Question #1							
#############

#mean = 7, stddev = 2.5

# a)

pnorm(7.5, 7, 2.5) - pnorm(3, 7, 2.5)

# b)

pnorm(2, 7, 2.5)

# c)

pnorm(6, 7, 2.5, FALSE )

#############								   	                  
#Question #2							
#############

#mean = 500, stddev = 75

# a) 

qnorm(0.85, 500, 75)

# b)

qnorm(0.73, 500, 75, FALSE)

#############								   	                  
#Question #3							
#############

# mean = 0, stddev = 3

y1 <- qnorm(0.71, 0, 3) 
y2 <- qnorm(0.29, 0, 3)

#y = 1.660154 