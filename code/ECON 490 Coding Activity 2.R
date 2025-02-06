################################################################################
# ECON 490 Coding Activity 2: Subsetting Data and Introducing Regression in R
################################################################################

# In this activity, we'll review the following: 

#   - Subsetting vectors and working with matrices (covered via Swirl)
#   - Using the tidyverse package for basic data manipulation tasks
#   - Introducing the lm() function for OLS regression

# When you're finished with the questions below, save a copy of your code with
# your name included in the file name and then upload this file to the Canvas
# page for this assignment.



################################################################################
# Swirl Lessons
################################################################################

# Let's start by continuing with several quick lessons from Swirl R. If you 
# don't have  Swirl installed, check the .R code from Coding Activity 1. 

# To get started with the Swirl lessons for this week, run the following lines: 

library(swirl)

swirl()

# After entering your name, select the "R Programming" course. Before completing
# the questions below, you should complete the following lessons: 

#   6) Subsetting Vectors
#   7) Matrices and Data Frames
#   8) Logic

# As with Coding Activity 1, you don't need to save or submit anything from 
# the Swirl activities on Canvas. The questions below will reference what's
# covered in the activities, however, so make sure you're comfortable with 
# the concepts in the lessons!



################################################################################
# The tidyverse Package
################################################################################

# The Swirl lessons above provide an introduction to subsetting data using Base
# R. These tools are helpful for smaller data cleaning tasks. However, when
# you're working with larger data sets for this class as part of either coding
# activities or the capstone project, you'll want to use the tidyverse package -
# this package provides a set of tools that help with a wide range of data
# cleaning tasks.

# To get started with tidyverse, uncomment (remove the #) and run the following 
# line to install the package: 

# install.packages("tidyverse")

# After you've installed the tidyverse package, you can load it by running the 
# following line: 

library(tidyverse)

# Before completing the questions below, take a look at the Canvas page for 
# Coding Activity 2 - you'll find a range of resources to help you get started 
# with using basic tidyverse functions.  



################################################################################
#
# Coding Activity Questions
#
################################################################################

# Once you've completed the Swirl lessons above and reviewed the tidyverse 
# material on the Coding Activity 2 Canvas page, you can complete the questions 
# included below. 

# Each question is included as a comment, with space left below for you to write
# out your answers as code. When you're done, save this file, and upload it to
# Canvas.



################################################################################
# Question 1: Swirl Review
################################################################################

# Suppose we create the following vector: 

q1.vector <- c(124, 233, 7123, 120, 895, 2103, NA, 65, 912)

# Using the ":" operator and brackets []'s, we can select elements of our new 
# vector. Here's how to select the 2nd through 4th elements: 

q1.vector[2:4]

# In addition to using row indices inside brackets, we can also use logical 
# operators. Suppose we wanted to only select elements that were greater than 
# 200. We can do this like so: 

q1.vector[q1.vector > 200]

# To remove missing values, we can use the is.na() function. Remember that this 
# function returns TRUE for any missing (NA) values, so to subset our vector
# to only non-missing values, we'll want to use "!" in conjunction with is.na().

q1.vector[!is.na(q1.vector)]

# In the space provided below, use the ":" operator to select the 4th through
# 6th elements of q1.vector: 





# Using logical operators, subset q1.vector to only elements with values in
# between 500 and 1000 that are not missing. HINT: Remember that you can combine
# multiple logical conditions using & (AND) and | (OR).





# Using logical operators, subset q1.vector to only elements with values that 
# are either LESS than 500 or GREATER than 1000. Make sure to omit any NAs. 





# Use the mean() function to calculate the average value of the non-missing 
# elements in the q1.vector. Make sure that your answer returns a numeric
# answer (meaning, your answer should not be NA). 





################################################################################
#
# Analyzing ACS Data using the tidyverse Package
#
################################################################################

# For the remaining questions, we'll use the ACS sample data set that we've used
# in examples during lecture. This data set is available online. You can
# download this data and load it into R by running the following lines:

data.url <- paste0("https://raw.githubusercontent.com/mackaytc/R-resources/",
                   "main/data/ECON-490-ACS-sample-data-CSV.csv")

acs.data <- read_csv(data.url) 

# To set us up for the questions below, we'll do some quick data cleaning - the 
# code below drops every row with any missing values, then restricts the sample
# to only observations with household income greater than 0 and ages 20-60: 

acs.data <- drop_na(acs.data) %>% 
  filter(hhincome > 0) %>% 
  filter(age > 20 & age < 60)

# You can find a list of the variables included with this data set, and their
# coding + definitions, on the Canvas page for this Assignment. 



################################################################################
# Question 2: Summary Statistics with tidyverse
################################################################################

# We can use the summarize() from tidyverse to calculate a wide range of summary 
# statistics. Let's look at an example: 

summarize(acs.data, 
          mean.renter = mean(renter), 
          total.food.stamp = sum(food_stamp))

# In the summarize() function above, we start by telling R that we want to 
# calculate summary statistics for the acs.data data set. Then, for each 
# summary statistic we want to calculate, we give R a name for the statistic, 
# and a function describing how it should be calculated. 

# There is a list of functions we can use with summarize() in the Introduction 
# to Tidyverse PDF posted on the Canvas page for this coding activity. 

# In the space provided below, calculate the following statistics (you can make
# up whatever names you like for each statistic) for the age variable:

#   - The mean value
#   - The median value
#   - The minimum and maximum values





# We can also save the output from summarize as a new object. In the example
# below, we'll save the summary stats we created above as a new object named
# summary.stats using the <- operator: 

summary.stats <- summarize(acs.data, 
                           mean.renter = mean(renter),
                           total.food.stamp = sum(food_stamp))

# Our new summary.stats object should appear in your "Environment" pane, and 
# we can access the results later as needed:

summary.stats

# In the space provided below, create a new object named income.stats that
# reports the following statistics (like above, you can include whatever names
# you like for the individual statistics) for hhincome:

#   - The median
#   - The standard deviation
#   - The 10th and 90th percentiles




################################################################################
# Question 3: Selecting Columns with tidyverse
################################################################################

# Suppose we want to create a new data set with only the state_name and state
# fips codes for each observation. We can do that using the select() function: 

new.data.frame <- select(acs.data, state_name, statefip)

# The new data set should appear in the "Environment" pane. We can view this
# new data set by either (1) clicking on the corresponding spreadsheet icon in 
# the "Environment" pane or (2) using the view() command:  

view(new.data.frame)

# We can use the rm() command to remove data sets from our environment: 

rm(new.data.frame)

# In the space provided below, use the select command to select just the age 
# and education variables. Here, you don't need to save your output as a new 
# object - just start your code with select() and the output will print in the 
# Console window. 





# One last note on select() - if you want to remove a column from your data 
# frame, you can put a "-" sign in front of the variable you want to remove. 
# As an example, let's remove renter from our acs.data data set: 

select(acs.data, -renter)



################################################################################
# Question 4: Filtering Rows with tidyverse
################################################################################

# Suppose we wanted to filter out observations of our data set according to some
# criteria. As an example, suppose we wanted to create a data set that only
# included observations who were renters. 

# We can use the filter() function to do this:

renter.data <- filter(acs.data, renter == 1)

# Let's check that our new data set only includes renters: 

table(renter.data$renter)



# In the space provided below, use the rm() function to remove the renter.data
# data set we just created. Then, create a new data set named subset.data that
# only includes observations who are employed. 





# You can combine multiple logical criteria using the & ("and") and | ("or)
# operators. In the space provided below, use the filter function to subset 
# our data to only include observations who are unemployed and older than 39
# years old. You don't need to save this output - just print it in the console. 





################################################################################
# Question 5: Creating New Variables with tidyverse
################################################################################

# The tidyverse package also gives us tools to create new variables (columns
# of our data set). The main function we'll use for this task is mutate().  
# Let's look at an example below: 

acs.data <- mutate(acs.data, log.age = log(age))

# The function above tells R to create a new variable, log.age, and add it to 
# our existing acs.data set. 

# Notice that we assigned the output from the mutate() function to our existing
# acs.data data set. If we didn't do this, the output would print in the console
# but it wouldn't be saved. Let's see what happens when we run the following:

mutate(acs.data, age.squared = age^2)

# R prints a data frame with 11 total variables, but it hasn't added age.squared
# to acs.data because we didn't assign the mutate output to acs.data using <-.
# We can confirm this by checking the variable names in acs.data - age.squared
# is not included: 

names(acs.data)

# In the space provided below, use the mutate() function to create a variable
# named log.hhincome with the log value of median household incomes and save
# this new to our existing acs.data data frame. 





# In the space provided below, use the mutate() and ifelse() functions to create
# a new binary variable named working.renter that is equal to 1 for everyone who
# is a renter (renter == 1) and currently working (employed == 1) and 0 for
# everyone else. Save this new variable to your existing acs.data data frame.





################################################################################
#
# Regression using lm()
#
################################################################################

# The basic OLS regression function in R is lm(). To regress employed on 
# hhincome and age, we can run the following:

lm(hhincome ~ renter + employed, data = acs.data)

# Several things to note about the command above: 

#   - The first argument is our regression formula - this is the regression
#       equation that we want to estimate. 

#   - The syntax for our regression formula has our outcome / Y / LHS variable  
#       first, then a "~" sign, then our X / RHS / explanatory variables 
#       separated by "+" signs

#   - The second argument tells R what data frame we want to use - for the sake
#       of clarity, I'll often include "data =" before the data set name, but 
#       you don't have to do this. 

# We can save our lm() model to access it later using the <- operator: 

ols.model <- lm(hhincome ~ renter + employed, acs.data)

# To get a summary of the estimated output from our regression, we can use the 
# summary() function: 

summary(ols.model)

# If we want to access particular coefficients from our regression, we can do
# so using the following: 

ols.model$coefficients["renter"]

# If we want to calculate predicted values of hhincome using this regression
# model, we can run the following: 

acs.data$predicted.employment <- ols.model$fitted.values

mean(acs.data$predicted.employment)

# R calculates a predicted (or "fitted") value for each observation in our data
# set. The code above stores this as a new variable in our acs.data frame. We'll 
# remove this variable for now, however. 

acs.data <- select(acs.data, -predicted.employment)

# We can also store the residuals from our regression. Residuals are the
# difference between the actual outcome Y and our predicted or fitted Y for 
# each observation. 

acs.data$residuals <- ols.model$residuals

# Being able to save residuals will be useful in a later activity, but for now, 
# let's remove this variable before starting the questions below. 

acs.data <- select(acs.data, -residuals)



################################################################################
# Question 6: Regression using lm()
################################################################################

# In the space provided below, run a regression with household income (hhincome)
# as our outcome variable and employed and age as our explanatory variables. 
# Save your output as q6.model.





# In the space provided below, use the summary() function to print the results
# of your regression above. 





# We can use the str() function to learn about the structure of our new lm()
# model. Use str() to check the structure of q6.model. Then, print the fitted
# (aka predicted) values of hhincome from our model (see the example above 
# for how to access fitted values). 





# After running the regression above, we can use our estimated coefficients to
# calculate predicted values of household income. Remember from above that R
# refers to predicted values as "fitted" values. 

# For someone who's 30 years old and currently working, we can calculate 
# their predicted income as:

prediction.30 <- 
  q6.model$coefficient["(Intercept)"] + 
  q6.model$coefficient["age"]*30 + 
  q6.model$coefficient["employed"]

prediction.30

# You might've noticed our prediction.30 inherited the name attribute for 
# the intercept term from our lm() output. We can change this like so: 

names(prediction.30) <- "Predicted Household Income for 30-Year-Olds"

prediction.30

# In the space provided below, calculate the predicted value of hhincome using
# the formula above with age set to 31. 





# In the space provided below, calculate the difference between your prediction
# for 31 year olds and 30 year olds. Using a comment (a line starting with #), 
# compare this difference to our regression output. How does this difference
# compare to the estimated age coefficient? 





################################################################################
# End of Activity
################################################################################

# Once you've included responses to each of the questions above, save this file
# with your name included in the file name, and upload your code to Canvas. 

# Make sure that you upload your .R code file (and not another file e.g., HTML
# formatted output, etc.). Remember that you don't need to submit anything for
# the swirl lessons.


