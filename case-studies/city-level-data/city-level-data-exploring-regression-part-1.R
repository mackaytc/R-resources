################################################################################
#
# Regression using City-Level Data (Part 1)
#
################################################################################

# In the code below, we'll review some of the concepts introduced in lecture  
# and the coding activities. We'll also get some practice working with
# a new data set. 

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
                   "refs/heads/main/case-studies/city-level-data/", 
                   "sample-city-data.csv")

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

# Our test.data object has two rows corresponding to tech.share = 0 & 1, with 
# college.grad held constant at 0.5: 

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
# Q2: Residualization
################################################################################

# Let's walk through an example of the "Option 2" residualization process 
# discussed during lecture. Here's our "Option 1" starting point: 

q2.model <- lm(city.output ~ tech.share + college.grad, sample.city.data)

summary(q2.model)

# Now we can run a regression of tech.share on college.grad: 

res.model <- lm(tech.share ~ college.grad, sample.city.data)

# We want the residuals from this model to be saved as a new variable in our 
# sample.city.data data set: 

sample.city.data$tech.share.tilde <- res.model$residuals

# Let's check the correlation between our tech share residuals and college.grad:

round(cor(sample.city.data$tech.share.tilde, sample.city.data$college.grad), 4)

# Finally, let's run the following regression, and compare the coefficient  on 
# residual tech share (tech.share.tilde) to the coefficient on tech.share
# from our initial regression: 

summary(lm(city.output ~ tech.share.tilde, sample.city.data))

# Original output: 

summary(q2.model)



################################################################################
# Q3: Setting the Stage for Talking about Omitted Variables Bias (OVB) 
################################################################################

# We can use the cor() function below to check the correlations of each of the 
# variables included in our data set (I've used filter here to remove city.ID):

cor(select(sample.city.data, -city.ID))

# Think back to the Venn Diagram and "interdependence" discussion we had during 
# lecture- what do you notice about the relationships between each of our 3
# variables?



# Let's see what happens if we don't include the college.education variable 
# in our regression: 

summary(lm(log(city.output) ~ tech.share + college.grad, sample.city.data))

summary(lm(log(city.output) ~ tech.share, sample.city.data))

# Notice that the coefficient on tech.share is larger when we don't include 
# college.grad. By failing to include college.grad, we wind up "overstating" 
# the impact of tech.share on city output - this is because tech.share and 
# college.grad are positively correlated. 

# This difference in estimated coefficients is caused by Omitted Variables Bias 
# (OVB) which we will (or have already) covered during lecture. 


################################################################################
# End of Activity
################################################################################

# You don't need to turn anything in after completing this activity (although 
# you should save this file because it will be helpful later!). 

