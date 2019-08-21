"
****************************************************************
Name: Dave Himani 
Student Number: A00178443

QMM 1001 Lab 8
****************************************************************
"

#############								   	                  
#Question #2							
#############

# a)

# p = 0.052, n = 8400

np = 8400 * 0.052 #np = 436.8 > 10
nq = 8400 * (1 - 0.052) #nq = 7963.2 > 10

# b)

sd <- sqrt(0.052*(1 - 0.052)/8400)

# c)

p.hat <- 425/8400
pnorm(p.hat, 0.052, sd, TRUE)

# d)

pnorm(500/8400, 0.052, sd, FALSE)

# e)

pnorm(465/8400, 0.052, sd) - pnorm(400/8400, 0.052, sd)

# f)

qnorm(0.10, 0.052, sd)

#The 10th percentile for the proportion of piot deaths is 0.04889, which is 4.88%
# 4.88% of 8400 is 409.92 ~ 410.Thus 410 pilots is the 10th percentile.