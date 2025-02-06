################################################################################
# ECON 490 Coding Activity 1: Introduction to R
################################################################################

# This file provides a quick introduction to using R. When you're finished with 
# the questions below, save a copy of your code with your name included in the  
# file name and then upload this file to the Canvas page for this assignment. 

################################################################################
# Getting Started with Swirl
################################################################################

# We'll use several quick lessons from Swirl as a way of getting started with 
# R. If you haven't installed the Swirl package yet, you can do so by running 
# the line of code below. To do this, "uncomment" (deleting the hash tag) the 
# next line, then click / place your cursor on it, and press CTRL + ENTER: 

# install.packages("swirl")

# Once you've installed the Swirl package, we can use the library function to 
# tell R to load the package so we can use it. Run the following line by placing
# your cursor on the line, clicking, and then pressing CTRL + ENTER: 

library(swirl)

# The next step is to install the lessons we'll use over the next several 
# classes. The course is "R Programming," and you can install it by running 
# the following line: 

install_course("R Programming")

# Now that we've loaded the Swirl package and the R programming course, we can 
# get started. Run the following line of code and follow the included prompts: 

swirl()

# After entering your name, select the "R Programming" course. Before completing
# the questions below, you should complete the following lessons: 

#   1) Basic Building Blocks
#   3) Sequences of Numbers
#   4) Vectors
#   5) Missing Values

# Each Swirl lesson takes place entirely in the console, so you don't need to 
# save or submit anything from the Swirl activities on Canvas. The questions 
# below will all build on what's covered in the activities, however, so make 
# sure you're comfortable with the concepts in the lessons! 

################################################################################
# Coding Activity Questions
################################################################################

# Once you've completed the Swirl lessons above, you can complete the questions
# included below. Each question is included as a comment, with space left below
# for you to write out your answers as code. When you're done, save this file,
# and upload it to Canvas.

################################################################################
# Question 1 
################################################################################

# Using the R assignment operator "<-" and the c() concatenate function, create
# a vector named q1.vector that stores the values 1, 3, 5, and 7 in the space 
# provided below. 





# Use the mean() function to calculate the average value of the elements in your 
# new q1.vector. 





# You can use the min() and max() function to identify the smallest and largest
# elements of a vector respectively. Use these functions to calculate the 
# minimum and maximum values of q1.vector. 





################################################################################
# Question 2
################################################################################

# Using the ":" operator, create a sequence of numbers from 0 to 50 in the space
# provided below. Save this sequence as q2.vector, then print your new vector.





# Let's create a new vector from 0 to 50 with increments of 0.5 using the seq()
# function. Let's replace our previous q2.vector with this new sequence (to do 
# this, you can just assign q2.vector to your new output below). 





# Use the length() function to check how many elements are in your q2.vector. 





# We can use the quantile() function to calculate percentiles of our q2.vector. 
# The line below calculates the median (50th percentile) value - the number 
# after the comma tells R what percentile we want to calculate (ranging from 
# from 0 to 1):  

quantile(q2.vector, 0.5)

# In the space below, calculate the 10th and 90th percentiles of q2.vector: 





################################################################################
# Question 3
################################################################################

# We can use the rnorm() function to create a vector where each element is a 
# random draw from a normal distribution. Run the line below to create a new
# vector with 25 draws from a normal distribution with a mean of 1 and sd of 2. 

q3.vector <- rnorm(25, mean = 1, sd = 2)

q3.vector

# Let's create a logical vector that tests whether each element of q3.vector is 
# greater than or equal to 2. Save this new vector as q3.test. 





# Use length() to check your new q3.test vector (the length should equal 25). 
# Then, use the table() function to tabulate the number of TRUE and FALSE values
# in q3.test. 





# Thus far, we've created numeric and logical vectors. We can also create 
# character vectors which store text (and numbers in text format), like the 
# example below: 

q3.char <- c("ECON", "490", "coding activity")

q3.char 

# We can use the paste() function to print the elements of character vectors 
# (and other vector types) as a single string of text. Use paste() to print
# q3.char & "homework" with each element separated by a space (using sep = " ").





# Finally, we can use the str() function to learn about the structure of the 
# vectors we've created above. Run the line below - the output tells you the 
# type of vector and the number of elements: 

str(q3.char)

# In the space below, use the str() function to check the structure of your
# q3.vector and q3.test vectors: 





################################################################################
# Question 4 
################################################################################

# R uses "NA" to represent missing values. Suppose the vector below records 
# survey responses for a question about the hourly wages of 10 people, and 3 
# people didn't respond to this particular question (meaning that we have hourly
# wages for 7 people and missing values = NA for 3 people). 

q4.income <- c(15, 17, 25, 30, NA, 12, NA, 32, 17, NA)

# Use the is.na() function to create a new vector named q4.missing that tells us
# whether each individual response in q4.income is missing or not: 





# Your new q4.missing vector should have a length of 10, with each element being
# either TRUE (meaning income is missing for that element of q4.income) or 
# FALSE (meaning that income is not missing in q4.income). 

# Use the table() function with our q4.missing vector to show the number of
# missing and non-missing values of income:





# Use the mean() function to calculate the proportion of TRUE elements in our 
# q4.missing. NOTE: Remember that for logical vectors, TRUE = 1 and FALSE = 0. 





# Let's see what happens when we try to calculate summary statistics with 
# missing values. Take a look at the output from the mean() function below: 

mean(q4.income)

# Notice that R returns a value of NA - this is because we haven't told R what
# to do with the NAs in the q4.income vector. We can calculate the average of 
# non-NA elements by using the "na.rm = TRUE" option, like so: 

mean(q4.income, na.rm = TRUE)

# Using the na.rm option above, calculate the 10th, 50th, and 90th percentile
# values of q4.income. HINT: Instead of using the quantile function 3 separate
# times for each of the 3 percentile values, try using c(0.1, 0.5, 0.9). 





################################################################################
# End of Activity
################################################################################

# Once you've included responses to each of the questions above, save this file
# with your name included in the file name, and upload your code to Canvas. 

# Make sure that you upload your .R code file (and not another file e.g., HTML
# formatted output, etc.). Remember that you don't need to submit anything for
# the swirl lessons.


