"
****************************************************************
Name: Dave Himani 
Student Number: A00178443

QMM 1001 Lab 1
****************************************************************
"

#############								   	                  
#Question #1							
#############
options(digits = 16)

#############								   	                  
#Question #2							
#############
2^29
#Number 4 is missing from the resut of 2^29.

#############								   	                  
#Question #3							
#############
1 - ((1 - 0.40)*(1 - 0.25)*(1 - 0.05))
#Answer - 0.5725

#############								   	                  
#Question #4							
#############
1000 * ((((1+0.03)^24) - 1) / 0.03 )
#Answer - 34426.47022

#############								   	                  
#Question #5							
#############
# a)
a <- 2
#In order to print it, run the above line and type 'a' in the commandline and hit Enter

# b)
b <- (2 * a + 10) / 2
print(b)
#In order to print it from commandline, run the above line and type 'b' in the commandline and hit Enter

# c)
c <- b - a
print(c)
#In order to print it from commandline, run the above line and type 'c' in the commandline and hit Enter

#############								   	                  
#Question #6							
#############

# a)
x <- c(20:50)

# b)
y <- c(30:60)

# c)
z <- x + y

# d)
z[1]

# e)
z[length(z)]

# f)
w <- z[c(2, 4, 6, 8)]

# g)
z[-27]

#############								   	                  
#Question #7							
#############

# a)
thousand <- c(1 : 1000)

# b) 
sample_1 <- sample(thousand, 50)

# c)
sample_2 <- sample(thousand, 50)

# d)
help(head)
head(sample_1, 7)
head(sample_2, 7)

# No, the first 7 elements of sample_1 and sample_2 are not same. 