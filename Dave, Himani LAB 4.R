"
****************************************************************
Name: Dave Himani 
Student Number: A00178443

QMM 1001 Lab 4
****************************************************************
"

#############								   	                  
#Question #1							
#############

NHL.Data <- read.csv("NHL.Data.csv", header = TRUE)

# a)

plot(NHL.Data$Weight, NHL.Data$Height, main = 'Weight vs Height of NHL Players', xlab = 'Weight', ylab = 'Height', pch = 17)

# b)

sub.NHL <- subset(NHL.Data, select = c(Weight, Height, Age))
sub.NHL

# c)

cor(sub.NHL)

#Height and Weight have the strongest correlation of 0.681.
#Height and Age have the weakest correlation of -0.025.

#############								   	                  
#Question #2							
#############

House.Price.Data <- read.csv("House.Price.csv", header = TRUE)

# a) Living.Area is the explanatory / independent variable. 

# b) House.Price is dependent / response variable. 

# c) 

plot(House.Price.Data$Living.Area, House.Price.Data$House.Price, main = "Living Area vs House Price", xlab = "Living Area", ylab = "House Price", pch = 19)

# d)

reg.prices <- lm(House.Price ~ Living.Area, House.Price.Data)
reg.prices

# e)

reg.prices$coefficients

# The line of best fit is : House.Price = 6376.0831 + 115.1314(Living.Area)

# f) 

abline(reg.prices, col = "tomato")

# g) 

newdata <- data.frame(Living.Area = 2000)
predict.lm(reg.prices, newdata)

#The price of a 2000 square footage house is $236,638.9

# h)

(235000 - reg.prices$coefficients[1])/(reg.prices$coefficients[2]) 

#The predicted square footage of a house costing $235,000 is 1985.765 square foot.

# i)

plot(reg.prices$residuals)

#The residual plot shows the residuals of any data, their shape, direction, and form.
#The points should be scattered throughout the plot in a random fashion and should not have a direction, shape, or pattern. 
#However, in this residual plot, the points seem to aggregate in a straight line near the bottom of the plot. 
#There are some outliers as well.
#This shows that the relationship between the two variables is not linear. 