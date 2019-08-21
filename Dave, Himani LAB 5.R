"
****************************************************************
Name: Dave Himani 
Student Number: A00178443

QMM 1001 Lab 5
****************************************************************
"

#############								   	                  
#Question #1							
#############

library(prob)

# a)

die.nine <- rolldie(2, 9, makespace = TRUE)

# b)

less.than.six <- subset(die.nine, die.nine$X1 + die.nine$X2 <6)

# c)

greater.than.fifteen <- subset(die.nine, die.nine$X1 + die.nine$X2 > 15)

# d) 

intersection.b.and.c <- intersect(less.than.six, greater.than.fifteen)
sum(intersection.b.and.c$probs)

#Since there would not be any instances where the sum of the two rolls would be both,
#greater than fifteen and less than six, the probability of this event occuring is zero.

# e)

union.b.and.c <- union(less.than.six, greater.than.fifteen)
sum(union.b.and.c$probs)

#Using the special rule for multiplication
prob.less.than.six <- sum(less.than.six$probs)
prob.greater.than.fifteen <- sum(greater.than.fifteen$probs)
multi.rule <- prob.greater.than.fifteen * prob.less.than.six
sum(intersection.b.and.c$probs)

#Since multi.rule (0.009144947) and probability of intersection.b.and.c (0), are not equal,
#the events are dependent.

# f)

same.num <- subset(die.nine, die.nine$X1 == die.nine$X2)
different <- 1 - sum(same.num$probs)

#The rule of complement is used here.

#############								   	                  
#Question #2							
#############

diamonds <- subset(cards(makespace = TRUE), suit == "Diamond")
kings <- subset(cards(makespace = TRUE), rank == "K")
union.diamond.kings <- union(diamonds, kings)
sum(union.diamond.kings$probs)
ans <- 16/52

#Hence, ans = probability of union.diamond.kings =  0.3076923