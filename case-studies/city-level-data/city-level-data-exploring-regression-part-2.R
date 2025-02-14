################################################################################
#
# Regression using City-Level Data (Part 2)
#
################################################################################

# In the code below, we'll review some of the concepts introduced in lecture  
# and the coding activities. We'll also get some practice working with
# a new data set. 




# INSERT: As of 2.14.2025 this needs to be updated (primarily Q2). 






################################################################################
# Data for this Activity
################################################################################

# I've created a sample data set which includes the following variables for 
# 50 fictional cities: 

#   - city.ID: Unique ID for each city (row) in our data 
#   - city.output: GDP per capita for each city
#   - tech.share: Fraction of the city's workforce employed in the tech industry
#   - college.grad: Fraction of each city's population with a college degree

# To load the data for this activity, run the code below: 

library(tidyverse)

data.url <- paste0("https://raw.githubusercontent.com/mackaytc/R-resources/", 
                   "main/data/sample-city-data.csv")

sample.city.data <- read_csv(data.url)

# We can see a summary of this data set using the following: 

summary(sample.city.data)



################################################################################
# Q1: Interpreting Regression Output
################################################################################

# Let's start by running the following regression: 

q1.model <- lm(city.output ~ tech.share + college.grad, sample.city.data)

summary(q1.model)

# Our tech.share and college.grad variables both take on values between 0 & 1. 
# Before running the code below, how would you interpret the coefficients on
# these variables?



# Let's use the predict() function to check our interpretation - here, we're 
# going to see how predicted GDP changes if tech.share goes from 0 to 1 (i.e., 
# the tech share of employment rises from 0 percent to 100 percent), while
# keeping college.grad constant at 0.5. 

test.data <- data.frame(tech.share = c(0, 1), 
                        college.grad = c(0.5, 0.5))

# Our test.data object has two rows corresponding to tech.share = 0 & 1: 

test.data

# This means our predictions output below will have 2 values as well: 

predictions <- predict(q1.model, test.data)

predictions

# We can calculate the difference in predicted values using the following: 

predictions[2] - predictions[1]

# Note that this is identical to the coefficient on tech.share in our summary 
# output from above: 

q1.model$coefficients["tech.share"]

# Last note - suppose we wanted to know the effect of a 0.01 unit change (in
# other words, given our X variables, a 1 percentage point change). All we 
# need to do is divide our coefficient by 100: 

q1.model$coefficients["tech.share"] / 100

# To test this, we can calculate the change in predicted values when tech.share
# goes from 0.50 to 0.51. 

test.data <- data.frame(tech.share = c(0.5, 0.51), 
                        college.grad = c(0.5, 0.5))

predictions <- predict(q1.model, test.data)

predictions[2] - predictions[1]



################################################################################
# Q2: Using Log Values in Regression
################################################################################

# INSERT - alternative text using crime and unemployment: 


# Using the logged value of variables can sometimes make be a helpful way of  
# regression coefficients easier to interpret. Consider the following: 

model.2 <- lm(log(rate_property_crime) ~ unemp_rate, working.data)

# Now, we're using the log value of the property crime rate as our outcome
# variable, by using the log() function in our regression formula. In general,
# when we have a log(Y) ~ X regression, we can  we can interpret the coefficient
# on X as telling us, "A 1 unit change in X is associated with a 100% * beta
# increase in Y."

summary(model.2)

# Now, we can interpret our unemployment rate coefficient as saying, "A one
# percentage point increase in the unemployment rate is associated with a 0.05
# percent increase in property crime rates." 

# Alternatively, you multiply both figures by 10 and say a 10 percentage point
# increase in unemployment is associated with a 0.5 percent increase in property
# crime rates.



# Let's use predict() again - now, tech.share will rise from 0.5 to 0.51 (again,
# keeping college.grad fixed at 0.5): 

test.data <- data.frame(unemp_rate = c(4, 5))

predictions <- predict(model.2, test.data)

predictions

# Let's compare the difference in predicted values - from last week, I skipped 
# a slide, where the punchline was, "Small differences in log values can be 
# interpreted as percentage changes." That's what we see below: 

model.2$coefficients["unemp_rate"]

predictions[2] - predictions[1]

# Multiply the difference above by 100 to get percentage change: 

100*(predictions[2] - predictions[1])

# Remember that we're generating predicted values of log city output. We can use 
# the exp() function to calculate the level (non-log) values of predicted GDP:

level.diff <- exp(predictions[2]) - exp(predictions[1])

level.diff 

# This tells us the dollar value difference in GDP associated with a 1 percent
# increase in tech.share. We can confirm our percentage change interpretation
# is correct by doing the following: 

new.rate <- exp(predictions[1]) + level.diff
old.rate <- exp(predictions[1])

pct.change.in.crime <- 100*(new.rate - old.rate) / old.rate

pct.change.in.crime

# Notice that the percentage change we've calculated here and the version we
# calculated above aren't identical - this is because the percentage change
# interpretation above is an approximation. In most cases (like we see here),
# the differences are very minor.
















# Using the log value of variables in a regression can make interpreting our 
# coefficients easier. Let's see how our estimates change if we use the log
# value of our Y variable, city.output: 

q2.model <- lm(log(city.output) ~ tech.share + college.grad, sample.city.data)

summary(q2.model)

# In general, when we have a log(Y) ~ X regression, we can  we can interpret our
# coefficient on X as telling us, "A 1 unit change in X is associated with a
# 100% * beta increase in Y."

# The advantage of this approach when we have X variables that take on values
# between 0 and 1 is that we can divide both changes by 100, and say, "A
# 1-percentage point increase (e.g., a 0.01 change instead of a 1.00 change) in
# X is associated with a beta-percent increase in Y.

# Additionally, it's often more natural to think about variables like tech.share
# or college.grad changing incrementally by 1 percentage point as compared to an
# all-or-nothing change from 0% to 100%.



# Let's use predict() again - now, tech.share will rise from 0.5 to 0.51 (again,
# keeping college.grad fixed at 0.5): 

test.data <- data.frame(tech.share = c(0.50, 0.51), 
                        college.grad = c(0.5, 0.5))

predictions <- predict(q2.model, test.data)

predictions

# Let's compare the difference in predicted values - from last week, I skipped 
# a slide, where the punchline was, "Small differences in log values can be 
# interpreted as percentage changes." That's what we see below: 

q2.model$coefficients["tech.share"]

predictions[2] - predictions[1]

# Multiply the difference above by 100 to get percentage change: 

100*(predictions[2] - predictions[1])

# Remember that we're generating predicted values of log city output. We can use 
# the exp() function to calculate the level (non-log) values of predicted GDP:

dollar.diff <- exp(predictions[2]) - exp(predictions[1])

dollar.diff 

# This tells us the dollar value difference in GDP associated with a 1 percent
# increase in tech.share. We can confirm our percentage change interpretation
# is correct by doing the following: 

new.GDP <- exp(predictions[1]) + dollar.diff
old.GDP <- exp(predictions[1])

pct.change.in.GDP <- 100*(new.GDP - old.GDP) / old.GDP

pct.change.in.GDP

# Notice that the percentage change we've calculated here and the version we
# calculated above aren't identical - this is because the percentage change
# interpretation above is an approximation. In most cases (like we see here),
# the differences are very minor.



################################################################################
# Q3: Setting the Stage for Talking about Omitted Variables Bias (OVB) 
################################################################################

# We can use the cor() function below to check the correlations of each of the 
# variables included in our data set (I've used filter here to remove city.ID):

cor(select(sample.city.data, -city.ID))

# Think back to the Venn Diagram and "interdependence" discussion we had last 
# week - what do you notice about the relationships between each of our 3
# variables?



# Let's see what happens if we don't include the college.education variable 
# in our regression: 

summary(lm(log(city.output) ~ tech.share + college.grad, sample.city.data))

summary(lm(log(city.output) ~ tech.share, sample.city.data))

# Notice that the coefficient on tech.share is larger when we don't include 
# college.grad. By failing to include college.grad, we wind up "overstating" 
# the impact of tech.share on city output - this is because tech.share and 
# college.grad are positively correlated. 

# Next week, we'll define this difference in estimated coefficients as 
# Omitted Variables Bias (OVB) and talk about why this change in estimated 
# coefficients occurs. 



################################################################################
# End of Activity
################################################################################

# You don't need to turn anything in after completing this activity (although 
# you should save this file because it will be helpful later!). 

