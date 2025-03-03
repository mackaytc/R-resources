################################################################################
# ECON 490: In-Class Activity Exploring the Capstone Data Sets
################################################################################

# In the code below, we'll review some of the regression-related concepts from
# lecture and the coding activities using two of the capstone data sets.

################################################################################
# Data for this Activity
################################################################################

# For this activity, we'll use the long-run state-level economic data and the
# crime data sets. While all of the capstone data sets are available via
# Dropbox, I've also posted them online to GitHub to make loading them easier
# (with the exception of the CPS data file, which is too large).

# To get started, run the code below: 

library(tidyverse)

# Load crime data first:  

crime.url <- paste0("https://raw.githubusercontent.com/mackaytc/R-resources/", 
                    "main/data/crime-data-state-by-year/", 
                    "crime-data-state-by-year.csv")

crime.data <- read_csv(crime.url)

# Then load state economic data: 

state.data <- paste0("https://raw.githubusercontent.com/mackaytc/R-resources/", 
                     "main/data/longer-run-state-level-data/", 
                     "longer-run-state-by-year-data.csv")

state.data <- read_csv(state.data)

# We can see a snapshot of both data sets by using the head() function: 

head(crime.data)
head(state.data)

# You can find PDF documentation on Dropbox and GitHub for definitions and notes
# for all of the variables included both data sets.



################################################################################
# Q1: Combining Data Sets
################################################################################

# Whenever we run regressions, we'll want to combine all the variables we'd like
# to use in a single data set. In general, we'll refer to the data set on which
# you run regressions for your capstone project as your "working" data set.

# Both the crime and state economic data sets are state-by-year, meaning each
# row of the data is uniquely identified by a combination of the statefip
# (or state_name) and year variables. 

# To combine both data frames, we can use the inner_join() function from the 
# tidyverse package. This function tells R to keep rows of data corresponding
# to state and year combinations that are present in BOTH data sets. Because
# the time range of our data is identical, we'll keep all 750 rows. 

working.data <- inner_join(state.data, crime.data, 
                           by = c("year", "statefip", "state_name"))

# Take a look at the output below - R used the year and statefip variables as ID
# variables to combine both data sets. Our new working.data object has these
# variables in addition to all of the variables included in both of our original
# data sets. 

head(working.data)



################################################################################
# Q2: Baseline Regression Output
################################################################################

# We're interested in the relationship between unemployment rates and crime. 
# In particular, let's focus on property crimes (which are more likely to be
# impacted by economic conditions), and run the following regression: 

model.1 <- lm(rate_property_crime ~ unemp_rate, working.data)

summary(model.1)

# Our unemployment rate variable takes on values between 0 and 100. Check the
# data documentation for our crime data - how is the property crime rate 
# variable calculated? How should we interpret the coefficient on unemp_rate?





# Let's use the predict() function to check our interpretation. Here, we're 
# going to see how the predicted property crime rate changes if unemployment
# rates rise by 1 percentage point from 4% to 5%.  

test.data <- data.frame(unemp_rate = c(4, 5))

# Our test.data object has two rows corresponding to unemp_rate = 4 & 5: 

test.data

# This means our predictions output below will have 2 values as well: 

predictions <- predict(model.1, test.data)

predictions

# We can calculate the difference in predicted values using the following: 

predictions[2] - predictions[1]

# Note that this is identical to the coefficient on unemp_rate in our summary
# output from above:

model.1$coefficients["unemp_rate"]

# In words, this tells us that a 1 percentage point increase in the unemployment
# rate is associated with an increase of ~118 crimes in the predicted value (or
# conditional mean) of property crimes per 100,000 people.



################################################################################
# Q3: Interactions in Regressions
################################################################################

# The regression above tells us about the overall relationship between property
# crime and unemployment rates. If we want to see how this relationship varies
# across regions of the country, we can use regression with an interaction. 

# Let's create a binary variable indicating whether or not a state is on the
# West Coast: 

working.data <- mutate(working.data, 
                       west.coast = ifelse(state_name   == "California" | 
                                             state_name == "Washington" | 
                                             state_name == "Oregon", 1, 0))

# Let's make sure this variable is coded correctly - first using table(), then
# by checking which states have west.coast == 1: 

table(working.data$west.coast)

working.data %>% 
  filter(west.coast == 1) %>% 
  select(state_name) %>% 
  table()

# NOTE: In the code above, while west.coast is binary (0/1), R technically 
# thinks it is a numeric variable. Because west.coast is binary, we don't need
# to use as.factor() to tell R to treat it as a factor variable - the results
# below will be the same whether we tell R it is a factor or numeric variable.

# However, if you try something similar with a factor variable with more than 2
# levels (e.g., by including a factor variable for state_name or Census region),
# you'll want to make sure it's treated as a factor variable by R.

# Now we can run a variation of the regression above where we interact our new
# west.coast variable with unemp_rate: 

model.2 <- lm(rate_property_crime ~ unemp_rate + unemp_rate:west.coast, 
              working.data)

summary(model.2)

# Our regression output now has two coefficients on unemp_rate. The first 
# coefficient (unemp_rate by itself) characterizes the relationship between
# crime and unemployment in non-West Coast states. 

# We can see this by predicting the property crime rate for these states (who
# all have west.coast = 0) given an increase in unemployment from 4% to 5%: 

non.WC.prediction <- predict(model.2, 
                             data.frame(unemp_rate = c(4, 5), 
                                        west.coast = c(0, 0)))

non.WC.prediction[2] - non.WC.prediction[1] 

# The difference above is just the coefficient on unemp_rate from model.2: 

model.2$coefficients["unemp_rate"]

# Now let's see what how a similar increase in unemployment affects West 
# Coast states: 

WC.prediction <- predict(model.2, data.frame(unemp_rate = c(4, 5), 
                                             west.coast = c(1, 1)))

WC.prediction[2] - WC.prediction[1] 

# Now, the difference above is the sum of our coefficient on unemp_rate AND 
# the coefficient on the interaction term unemp_rate:west.coast: 

model.2$coefficients["unemp_rate"] + 
  model.2$coefficients["unemp_rate:west.coast"]

# In words, this tells us that a 1 percentage point increase in the unemployment
# rate causes a larger increase in predicted property crime in West Coast states
# than in other states in the US. The size of the difference is given by 
# the coefficient on the interaction term (i.e., ~35.7 crimes). 

model.2$coefficients["unemp_rate:west.coast"]



################################################################################
# Q3: Thinking More about Interaction Regressions
################################################################################

# In the lecture notes on OVB and fixed effects, we talked about how fixed 
# effects can absorb potential omitted variables that are correlated with 
# both our outcome and explanatory variables. 

# In the regression above, West Coast states tend to have both higher property
# crime rates and higher unemployment rates. We can see this via the following:

working.data %>% 
  group_by(west.coast) %>% 
  summarize(mean(rate_property_crime), 
            mean(unemp_rate))

# In this context, it makes sense to include a fixed effect for being a West
# Coast state - we can do this by including our west.coast variable in the 
# interaction regression from above: 

model.3 <- lm(rate_property_crime ~ 
                unemp_rate + west.coast + unemp_rate:west.coast, 
              working.data)

summary(model.3)

# Take a look at the coefficient on unemp_rate:west.coast - now our estimated
# interaction coefficient is negative! 

# In the space provided below, use predict() to see how an increase in
# unemployment affects West Coast and non-West Coast states. Given your output,
# how should we interpret our model.3 regression estimates? 





# For your capstone data analysis, you'll want to put together results that tell
# a story. In that context, the approach above is just a starting point - you'll
# want to be able to talk about your output and draw meaningful conclusions.



################################################################################
# End of Activity
################################################################################

# You don't need to turn anything in after completing this activity (although 
# you should save this file because it will be helpful later!). 
