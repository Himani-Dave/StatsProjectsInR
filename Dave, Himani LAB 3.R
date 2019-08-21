"
****************************************************************
Name: Dave Himani 
Student Number: A00178443

QMM 1001 Lab 3
****************************************************************
"

#############								   	                  
#Question #1							
#############

# a)
Car.Discounts <- read.csv("Car.Discounts.csv")
mean(Car.Discounts$Discount)

# Mean = 1266.81

# b)
range.discounts <- max(Car.Discounts$Discount) - min(Car.Discounts$Discount)
range.discounts

# Range = 2389

# c)
sd(Car.Discounts$Discount)

# Standard Deviation = 537.3578

# d)
quantile(Car.Discounts$Discount)

# Q1 = 845.25, Q3 = 1662.50

# e)
car.discounts.male <- subset(Car.Discounts, Car.Discounts$Gender == 0)
male.discounts <- mean(car.discounts.male$Discount)
male.discounts

# Mean discount for males = 962.0556

# f)
car.discounts.female <- subset(Car.Discounts, Car.Discounts$Gender == 1)
female.discounts <- mean(car.discounts.female$Discount)
female.discounts

# Mean discount for females = 1624.565

# g)
boxplot(Car.Discounts$Discount~Car.Discounts$Gender, col=c('blue', 'pink'), main = 'Discounts by Gender', ylab = 'Discounts', xlab = 'Gender ( 1= female, 0 = male )')

#From the boxplot, we can see that the median for discount given to males is lower than that given to females. The range of discounts is also less for males. 
#Thus, we can deduce that females are given more discounts on cars as compared to males.

#############								   	                  
#Question #2							
#############

House.Price <- read.csv("House.Price.csv")

# a) 
hist(House.Price$House.Price, breaks = 20, xlab = 'Prices', ylab = 'No. of houses', col = 'mediumseagreen', border = 'midnightblue', main = 'House Prices')

# b)
#The shape of the distribution is positively skewed (right skewed).

mean.house.price <- mean(House.Price$House.Price)
mean.house.price

# Mean = 220287.9

median.house.price <- median(House.Price$House.Price)
median.house.price

# Median = 199950

#Here, mean is higher than the median which confirms that the histogram is right skewed. 

# c)

hist(log10(House.Price$House.Price), xlab = 'Logarithm of Prices', ylab = 'No. of houses', col = 'mediumseagreen', border = 'midnightblue', main = 'Logarithm of House Prices')
#Taking the logarithm to the base 10 of the house prices and making a histogram using those values gives us a histogram which is almost symmetric.


#############								   	                  
#Question #3					
#############

workforce <- read.csv("Workforce.csv")

# a)

workforce.ts <- ts(subset(workforce, select = c(Annual.Average, Male, Female), start = 1948, end = 2015))

# b)

plot.ts(workforce.ts, main = "Workforce participations")

# c)

# From the time series plot, we can see that the annual average participation increased initially but later in the years, it has slowly started decreasing.
# The male workforce participation has been decreasing steadily.
# The female workforce participation has been increasing steadily.
