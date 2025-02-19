################################################################################
# ECON 490 Coding Activity 3: Exploring Regression in R
################################################################################

# In this activity, we'll review the following: 

#   - Using the tidyverse package for basic data manipulation tasks
#   - Using the lm() regression function with factor variables

# NOTE: For this activity, you don't need to complete anything with Swirl. We
# will pick up Swirl lessons again during Coding Activity 4. 

# When you're finished with the questions below, save a copy of your code with
# your name included in the file name and then upload this file to the Canvas
# page for this assignment.



################################################################################
# The tidyverse Package
################################################################################

# We introduced the tidyverse package in Coding Activity 2. In this activity, 
# we'll explore several additional tidyverse tools - to use these tools, 
# we'll need to load the tidyverse package first: 

library(tidyverse)

# NOTE: Make sure you've run the line above before starting with the questions!



################################################################################
#
# State-by-Year Housing Data Set for this Activity
#
################################################################################

# For this activity, we'll use a new data set posted to the course GitHub page.
# Just like the ACS data set we've used previously, you can load this data by
# running the following code:

data.url <- paste0("https://raw.githubusercontent.com/mackaytc/R-resources/", 
                   "refs/heads/main/data/state-housing-data/", 
                   "housing-data-state-by-year.csv")

housing.data <- read_csv(data.url) %>% 
  drop_na()

# We can use the view() function to see what this data set contains: 

view(housing.data)

# The zhvi_home_value variable stores Zillow's Home Value Index (ZHVI). This 
# index represents the "typical" home price in a given year (we can interpret
# it as similar to median home price although it's slightly different.   

summary(housing.data$zhvi_home_value)



################################################################################
#
# Working with Factor Variables in R 
# 
################################################################################

# In the code below, I'll provide a summary of how to work with factor variables
# in R. First, let's create a subset of our data that contains only the West 
# Coast states - California, Oregon, and Washington: 

west.coast <- housing.data %>%
  filter(state_name %in% c("California", "Oregon", "Washington"))

# The filter() statement above says, "Look at the state_name variable and only
# keep observations that have one of the listed state names." The %in% operator
# here is a helpful way to filter using a list of attributes like states names. 

# Our current state_name variable is a character variable (meaning, it stores
# strings of text). We can check this using the str() function: 

str(west.coast$state_name)

# We can use table() to see how often each state appears - note below that this
# variable stores the full name of each state (e.g., California instead of CA): 

table(west.coast$state_name)

# Using the code below, let's create a factor variable version of our state name
# variable. In the options used below, levels corresponds to the levels of the 
# original variable we're using, and labels corresponds to the new labels 
# we'll create for our factor variable:

west.coast <- west.coast %>%
  mutate(state.name.factor = 
           factor(state_name,
                  levels = c("California", "Oregon", "Washington"),
                  labels = c("CA", "OR", "WA")))

# Now we can check what type of variable our new state.name.factor variable is 
# using the str() function that we've seen previously: 

str(west.coast$state.name.factor)

# The str() tells us our new variable is a factor with 3 levels (corresponding
# to each of the 3 states in our West Coast data set). Variable types are also
# listed in "Environment" Pane of RStudio - just click on the icon next to
# west.coast and take a look at state.name.factor.

# Now, R knows that state_name is a factor variable, meaning that the values
# in and of themselves are not meaningful. This means that if you try to use
# a function that requires a numeric input with state_name, such as mean(),
# you'll get an error.

# We can use the table() function to tabulate observations that take on each 
# level of our new factor variable:

table(west.coast$state.name.factor)

# We can also is.factor() to double check that we've created a factor variable:

is.factor(west.coast$state.name.factor)

# Suppose we wanted to run a regression with zhvi_home_value as our outcome
# variable and state.name.factor as our explanatory variable.

# In general, whenever we include a factor variable in a lm() formula, we want
# to make sure that R knows it's a factor variable. One way to do this is
# using as.factor(), as in the following function:

lm(zhvi_home_value ~ as.factor(state.name.factor), data = west.coast)

# Because we've already coded state_name as a factor variable, and confirmed
# this using is.factor(), we don't have to use as.factor(). However, whenever
# you're including a new variable in a regression, make sure that R
# is handling it correctly! When in doubt, use as.factor().



################################################################################
# Question 1: Defining Factor Variables
################################################################################

# The code below creates a new numeric-formatted binary indicator variable 
# called post_2014 that is equal to 1 if the year is 2015 or later, and 0 for
# all earlier years.

west.coast <- mutate(west.coast, 
                     post.2014 = ifelse(year >= 2015, 1, 0))

# This new variable is numeric-formatted: 

str(west.coast$post.2014)

# In the space provided below, create a new variable called post.2014.factor
# that is a factor variable version of post.2014, using the general process
# outlined above. 

# NOTE: The levels here are now the 0 and 1 values created by ifelse() and our
# new factor variable labels should be "Yes" (for 1) and "No" (for 0).



# After creating post.2014.factor, use the is.factor() function to confirm that
# its been coded correctly as a factor variable. Then, use table() to check how
# many observations are before / after 2014. 



################################################################################
# Working with the %>% "Pipe" Operator
################################################################################

# One useful tool that tidyverse provides is the %>% operator. Using this in 
# your code is referred to as "piping." In general, the %>% operator's job is 
# to pass output from one line to the next as an input to another function. 

# Let's consider a simple example - suppose we wanted to calculate the mean of 
# of a vector of values, then round this mean to 2 digits. One way of doing 
# this is the following

mean.value <- mean(c(12, 48, 281))

mean.value <- round(mean.value, digits = 2)

mean.value

# Notice that we have two separate lines assigning mean.value. If we were 
# performing a more complex operation (with more lines and assignment steps), 
# this could be hard to follow! 

# We can simplify the process using the %>% operator:

mean.value <- c(12, 48, 281) %>% 
  mean() %>% 
  round(digits = 2)

# We start by telling R we want to create an object named mean.value (just
# like we did above). What is mean.value going to store? 

# Let's breakdown what's happening in each line of the code: 

#   - We said above that the job of the %>% operator is to pass objects along
#       between functions. Here, the object we're starting with is our vector
#       of numbers - c(12, 48, 281). 

#   - The first %>% says, "Pass that vector down to the next line as an input
#       to the next function." In this case, the next function is mean(), so 
#       mean() will take our vector as the input, and calculate the mean. 

#   - After calculating the mean, the next %>% operator says, "Pass this new 
#       mean value down to the next line as an input to the next function." 
#       This line then calculates the rounded value of the mean() we calculated 
#       in the line above. 

#   - Finally, after we've moved all the way from our starting point (the vector
#       of values on the first line), to our ending point (the round() function)
#       R will assign the output from our final function to mean.value, so 
#       mean.value will record the rounded average of c(12, 48, 281). 



################################################################################
# Question 2: Using the %>% "Pipe" Operator
################################################################################

# Let's get some practice working with the %>% operator. One thing to note at 
# the start - you can use CTRL + SHIFT + M (on windows) as a hotkey for %>%. 

# In the space provided below, use the select() function to pick out just the
# zhvi home value column from our west.coast data set. Then, use %>% to send
# this variable to the summary() function:





# One reason the %>% function is helpful is because it makes data cleaning code
# clearer by "stacking" operations. Suppose we wanted to return to our original
# housing.data data set and then subset to only the years 2016 to 2018, and
# calculate the mean of the zhvi_home_value variable for those years: 

filter(housing.data, year >= 2016 & year <= 2018) %>% 
  select(zhvi_home_value) %>% 
  summarize(mean.prices = mean(zhvi_home_value))
  


# In the space provided below, use the filter() function to pick out rows of 
# housing.data corresponding to (1) Arizona in (2) the years 2010-2020. Then, 
# use the tidyverse summarize() function to calculate the minimum and maximum
# prices observed during that time period. 

# HINT: Remember from Coding Activity 2 and your Introduction to Tidyverse guide
# that you can use summarize() once and include two separate min() / max() 
# calculations. 



################################################################################
# Question 3: Grouping Data with tidyverse
################################################################################

# Factor variables are often used to define groups of observations. This can be
# particularly helpful when we want to perform operations on data separately
# across each group. We can use the tidyverse function group_by() to tell R, "I
# want to perform operations on this data separately for each level of a given
# variable."

# The code below tells R, "Group our west.coast data by state, then calculate
# average home prices for each state separately."

group_by(west.coast, state.name.factor) %>% 
  summarize(mean_price = mean(zhvi_home_value))

# Notice that we get three different average values - these correspond to the
# average home values across each state.

# In the space provided below, group our west.coast data set by post.2014.factor
# and then calculate the median value of home values (using the summarize()
# function). Your output should produce two separate values:



# Next, group the west.coast data set by year, and report the average home value
# for each year (meaning, you should have a single average for each year, 
# calculated across all 3 states for that year): 



################################################################################
# Question 4: Regression with Factor Variables
################################################################################

# We were introduced to the lm_robust() robust function during lecture. This 
# function gives us robust standard errors and makes regression output easier
# to work with. To get started using this function, we'll need to install 
# the estimatr package first: 

install.packages("estimatr") # NOTE: Make this line a comment after running

# REMEMBER: You only need to install packages once - after running the line 
# above, you should add a comment in front of it so it doesn't run. Each time 
# you open R, you'll need to load the package using library() function like so: 

library(estimatr)

# Now that we have the estimatr package installed, we can use the lm_robust()
# function. The syntax for this function is very similar to the lm() function
# we've seen previously. 

# Suppose we run a regression with zhvi_home_value as our outcome variable and
# state.name.factor as our explanatory variable.

q4.model <- lm_robust(zhvi_home_value ~ state.name.factor, data = west.coast) 

# Remember our "N levels -> N-1 coefficients" rule of thumb. Our state factor
# variable has 3 levels, so we get two coefficients (N = 3, so N-1 = 2).

summary(q4.model)

# The lm_robust output gives us similar output as lm(), but we also get 
# confidence intervals, and its easier to work with output like p-values.  

# Remember our general statistical significance rule of thumb  - a coefficient
# is significant if it has a p-value less than 0.05. It's often easiest to 
# round p-values to check them: 

round(q4.model$p.value, 3) # Using round() with the 3 option gives us 3 decimals

# In the space provided below, use the q4.model output stored above to calculate
# the average home prices in each state in our sample (CA, OR, and WA). 



# Finally, in the space provided below, run a regression using lm_robust() with
# home prices as our outcome variable and post.2014.factor as the explanatory
# variable. Are average prices higher before or after 2014? Is the coefficient
# statistically significant? 



################################################################################
# End of Activity
################################################################################

# Once you've included responses to each of the questions above, save this file
# with your name included in the file name, and upload your code to Canvas. 

# Make sure that you upload your .R code file (and not another file e.g., HTML
# formatted output, etc.). Remember that you don't need to submit anything for
# the swirl lessons.
