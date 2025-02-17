################################################################################
#
# Simple Regression Examples Using NBA Data
#
################################################################################

# The code below provides several examples of basic regressions using NBA
# basketball data. Before you get started, click on the data set in the
# environment pane to see how the data set is structured.

library(tidyverse)

################################################################################
# Data Overview
################################################################################

# We'll load player-level NBA data from the 2023-2024 season. Each row of the
# data is a player on a given team, and the columns contain various player
# statistics. Download data from GitHub: 

nba.data <- read_csv(
  paste0("https://raw.githubusercontent.com/mackaytc/R-resources/refs/heads/",
         "main/case-studies/NBA-data/NBA-player-per-game-data-2023-2024.csv"))

# Note that if a player is traded, they can appear more than once. There is a 
# season total row for each traded player that we can use to get season totals. 

nba.data <- nba.data %>% 
  group_by(Player) %>%
  mutate(teams.played.for = n())

table(nba.data$teams.played.for)

# Now, keep all rows for players that weren't traded (first line), or the total
# rows for players that were traded (second and third lines).

nba.data <- filter(nba.data, (teams.played.for == 1)        | 
                     (teams.played.for > 1 & Team == "2TM") | 
                     (teams.played.for > 1 & Team == "3TM"))

# Finally, we'll restrict our sample to only those players who played in at 
# least 25 games for 5 or more minutes per game. 

nba.data <- filter(nba.data, G >= 25 & MP >= 5)

################################################################################
#
# A Couple of Silly Examples to Start
#
################################################################################

# Examples below are designed to help us think about regression by making 
# comparisons that aren't necessarily that meaningful! 

################################################################################
# Q1: Does playing more minutes per game increase turnovers per game?
################################################################################

# Turnovers (TOV in the data set) happen when a player loses the ball and the
# other team gains possession (turnovers are a bad thing). In the regression
# below, we ask, "How does the number of minutes you play (MP in the data set)
# predict the number of turnovers you have?"

q1.model <- lm(TOV ~ MP, data = nba.data)

summary(q1.model)

# Let's put these results in context - given our estimated coefficient on
# minutes played (MP), each additional minute of playing time is associated 
# with 0.07 more turnovers (TOV).

# In practical terms, this might be a bit tricky to interpret - is 0.07 a lot of
# turnovers or not? Let's experiment with transforming our coefficient. To do
# this, we can start by seeing how many minutes players tend to play:

quantile(nba.data$MP)

# The median player plays around 22 minutes, with most people playing between 
# 15 and 30 minutes. Given these ranges, suppose we wanted to know, "If a player
# plays 10 more minutes, how many more turnovers should we expect?"

# To answer this, all we need to do is multiply our coefficient by 10 (because
# our coefficient gives us turnovers per minute): 

q1.model$coefficients["MP"] * 10

# Playing 10 more minutes leads to around ~0.7 more turnovers on average. This
# feels a bit easier to interpret. 

# Can we make things even easier? Suppose we ask, "How many more minutes would a
# player need to play to cause 1 additional turnover?" To answer this, we need
# to solve the following equation:

minutes.for.1.additional.turnover <- 1 / q1.model$coefficients["MP"]

# Using this formula, we expect them to average ~1 additional turnover for each
# 14 additional minutes that they play:

minutes.for.1.additional.turnover

# We can clean up the output above by using the round() function to shorten our
# minutes value and the unname() function to remove labels and make the output
# cleaner to read:

round(unname(minutes.for.1.additional.turnover))

# Note that this is an easier way to interpret this coefficient than using a 1
# minute change. Remember that you are always free to scale coefficients to make
# them easier to interpret!



################################################################################
# Q2: Does having a first name starting with A, B, or C affect points scored?
################################################################################

# The code below uses the str_detect() function to create a binary indicator
# variable named abc.name that is equal to 1 for players whose names start with
# the letters A through C, and 0 for all other players.  

nba.data <- mutate(nba.data, 
                   abc.name = ifelse(str_detect(Player, "^[ABC]"), 1, 0))

table(nba.data$abc.name) # Number of players with names starting with A, B, or C

# With this new variable, we can run the regression below to see if the first
# letter of your first name matters for points scored (remember, this question
# is intentionally a bit silly - names shouldn't impact performance!):

lm(PTS ~ abc.name, data = nba.data) %>% summary()

# Having a name starting with A, B, or C is associated with scoring ~0.07 fewer
# points per game. 

# Using some background knowledge, it's unlikely that someone's name really
# matters for their performance. Given that, how should we interpret our
# coefficient? Is it significant?

# The first thing we can do is compare our coefficient to the average number of 
# points scored: 

mean(nba.data$PTS)

# Relative to the overall average, the magnitude of our coefficient is tiny. We 
# can refer to this as "practical" or economic significance. In practical terms, 
# our coefficient is not significant.

# What about statistical significance? First, we can check the stars and
# p-values on the regression output to see if the coefficient is statistically
# significant. 

# The large p-value here indicates that our estimated relationship likely just
# reflects noise in the data. 

# We can construct a confidence interval for further context:

confint(lm(PTS ~ abc.name, data = nba.data))

# This interval tells us the range of estimated effects we would likely observe 
# if we were to imagine replaying this season many times (or gathering data 
# across many seasons). 

# This interval is both 1) very wide compared to the coefficient we observed and
# 2) includes both positive and negative values. This tells us that given the
# underlying noise in the data, it is unlikely that there is any meaningful
# relationship between our outcome and explanatory variables.



################################################################################
#
# A Couple of More Informative Examples
#
################################################################################

# The regressions below also help us think about regression, while making 
# comparisons that are a bit more meaningful!

################################################################################
# Q3: What Factors Affect Scoring?
################################################################################

# Let's predict points in terms of the number of shots you take (FGA) and the
# percentage of shots that you make (eFG% is effective field percentage, and its
# technically a weighted average of how you do at different kinds of shots).

q3.model <- lm(PTS ~ FGA + `eFG%`, nba.data) 

summary(q3.model)

# Let's interpret the coefficients on our explanatory variables. The first 
# coefficient on FGA says for each additional shot you take, your predicted
# points scored increases by ~1.3. This is reasonable - shooting more means
# you're likely to score more, but its not a guarantee. 

# What about the coefficient on eFG%? Suppose we interpret this coefficient by 
# saying "Increasing my shooting percentage by 1 increases my predicted points 
# by 12" - that's a huge change! 

# To see what's going on - and how we should improve our interpretation - let's
# look at summary statistics for eFG% (notice that we use backticks around eFG% 
# because % is a special character): 

summary(nba.data$`eFG%`)

# Notice that all of our values range between ~0.37 and ~0.74 - these percentage
# values are all stored as numbers between 0 and 1. So when we said "increase 
# eFG% by 1" above, we were really saying, "Increase our shooting percentage 
# from 0 to 100 percent." Let's put things in terms that make more sense: 

q3.model$coefficients["`eFG%`"] / 100

# Now, this tells us that for each additional percentage point of improvement
# in our shooting percentage, we expect to score an additional 0.12 points.

# We can double-check that using the predict function (note that the check.names
# option here is included so we can match the % sign in eFG%). 

scoring.eFG.50 <- predict(q3.model, newdata = 
                            data.frame(FGA = 10, `eFG%` = 0.5, check.names = F))

scoring.eFG.51 <- predict(q3.model, newdata = 
                            data.frame(FGA = 10, `eFG%` = 0.51, check.names = F))

# Above, we generated predicted values for a player who takes 10 shots per game
# and makes 50 and 51 percent of their shots. Now, we can check the difference
# in predicted values: 

scoring.eFG.51 - scoring.eFG.50

# Notice that this difference is exactly equal to our coefficient above!



################################################################################
# Q4: How Does Scoring Vary by Position?
################################################################################

# Let's look at scoring across the five different positions in basketball. To 
# start, we'll do a bit of data cleaning with the position factor variable. 

nba.data <- mutate(nba.data, 
                   Pos = factor(Pos, levels = c("PG", "SG", "SF", "PF", "C")))

# The code above reorders our factor variable so that PG is listed first in our
# lm() output below. 

# Let's start by considering the following regression. Here, PG is the omitted
# group, so we're making comparisons of each position relative to point guards 
# (who happen to be the highest scoring group). 

lm(PTS ~ as.factor(Pos), nba.data) %>% summary()

# We mentioned briefly in lecture that we could remove our intercept term and 
# get a coefficient for all levels of our factor variable. We can add "- 1" to 
# our regression code to use this option: 

lm(PTS ~ as.factor(Pos) - 1, nba.data) %>% summary()

# Now, each coefficient is the predicted value or conditional average of points
# scored for each position group. 

# AN ADVANCED QUESTION: Did you happen to notice the differences in statistical
# significance across both regressions? What might explain these seemingly
# large differences?

# HINT: Think about the comparisons you're making in each regression. One model
# makes relative comparisons (across positions) while the other makes absolute 
# comparisons (i.e., tests for differences in predicted values relative to 0 
# for each position separately).  



################################################################################
# Q5: Binary Variables in Regression Models
################################################################################

# Let's create a binary indicator variable set equal to 1 for players who won
# an individual award at the end of the season (awards like MVP, All-NBA, etc. 
# are handed out to the best players in the league). 

nba.data <- mutate(nba.data, 
                   award.winner = ifelse(!is.na(Awards), 1, 0))

# The ifelse() function above says, "If you have a non-missing value for Awards, 
# assign a value of 1 to award.winner, otherwise assign 0." Around 13 percent of 
# the league won some kind of award. 

mean(nba.data$award.winner)

# We'll consider two separate regressions. The first uses our binary variable as 
# an explanatory variable. Remember that when we do this, we can interpret the 
# regression as telling us a difference in means between two groups (in this 
# case, between award winners and non-winners). 

# Consider the following regression: 

lm(PTS ~ award.winner, nba.data) %>% summary()

# QUESTION: Who has a higher scoring average, awards winners or non-awards 
# winners. How large is the difference? 



# Next, let's use our award winner variable as an outcome variable. Let's see 
# how minutes played (MP) and points scored (PTS) affect the probability that 
# you won an award. 

q5.model <- lm(award.winner ~ PTS + MP, nba.data)

summary(q5.model)

# When we have a binary outcome variable, our regression tells us how our
# explanatory variables change the predicted probability that our outcome 
# variable is equal to 1 (in this case, that someone won an award). 

# Let's look at the coefficient on points: 

q5.model$coefficients["PTS"]

# This says that for each additional point that a player scores, the probability
# that they win an award increases by 4 percentage points (p.p.). You can think
# about multiplying the coefficient here by 100 p.p. if it helps - remember that
# the event (winning an award here) happens if Y = 1, hence the scaling.

# QUESTION: Consider the coefficient on minutes played - does it seem odd that 
# the coefficient is negative? Remember that we have controlled for points 
# scored - if we are holding points scored fixed, does it make more sense that
# minutes is negatively related to the probability of winning an award?



################################################################################
# End of Activity
################################################################################

# You don't need to turn anything in after completing this activity (although 
# you should save this file because it will be helpful later!). 



