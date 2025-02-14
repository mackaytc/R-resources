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
# with 0.07 more turnovers.

# How many more minutes would a player need to play to cause one additional
# turnover? Let's check: 

minutes.for.1.additional.turnover <- 1 / q1.model$coefficients["MP"]

# Using this formula, we expect them to average ~1 additional turnover for each
# 14 additional minutes that they play. 

minutes.for.1.additional.turnover

# We can clean up the output above by using the round() function to shorten our
# minutes value and the unname() function to remove labels makes the output
# cleaner to read:

round(unname(minutes.for.1.additional.turnover))

# Note that this is an easier way to interpret this coefficient than using a 1
# minute change. Remember that you are always free to scale coefficients to make
# them easier to interpret!



################################################################################
# Q2: Does having a last name starting with A, B, or C make you a better scorer?
################################################################################

# Start by creating an indicator variable for players whose names start with 
# the letters A through C. 

nba.data <- mutate(nba.data, 
                   abc.name = as.numeric(str_detect(Player, "^[ABC]")))

table(nba.data$abc.name)

# Then, run a regression to see if these players score more points...

lm(PTS ~ abc.name, data = nba.data) %>% summary()

# Having a name starting with A, B, or C is associated with scoring ~0.37 more
# points per game. Is this a meaningful difference? Intuitively, we should 
# suspect that this is likely just noise in the data. How can we check?

# First, note that our coefficient is not statistically significant. This 
# tells us that the estimated relationship likely just reflects noise in the 
# data. We can construct a confidence interval for further context: 

confint(lm(PTS ~ abc.name, data = nba.data))

# This interval tells us the range of estimated effects we would likely observe 
# if we were to imagine replaying this season many times (or gathering data 
# across many seasons). 

# Note that this interval is very wide compared to the coefficient we observed, 
# and includes both positive and negative values. This tells us that given 
# the underlying noise in the data, it is unlikely that there is any meaningful
# relationship between our outcome and explanatory variables. 



################################################################################
# Q3: Do Offensive and Defensive Rebounds Predict Total Rebounds?
################################################################################

# Let's run each of these regressions separately, then compare results: 

lm(TRB ~ ORB, data = nba.data) %>% summary()
lm(TRB ~ DRB, data = nba.data) %>% summary()

# Notice that in both cases, our coefficients are statistically significant and
# large in magnitude - each additional offensive and defensive rebound are
# associated with an increase of more than 1 total predicted rebounds. Does this
# mean these are informative regressions? No! 

# To see why, let's add both rebounding categories to our regression: 

lm(TRB ~ ORB + DRB, data = nba.data) %>% summary()

# There are only two types of rebounds, offensive and defensive, so we're
# looking at a "mechanical" relationship. The important point is that R /
# regression doesn't "know" that this is the case - we need to apply subject
# matter knowledge to assess things.

# SMALL NOTE: Because of rounding in the averages, offensive and defensive 
# rebounds don't exactly add up to total rebounds.



################################################################################
#
# A Couple of More Informative Examples
#
################################################################################

# The regressions below also help us think about regression, while making 
# comparisons that are a bit more meaningful!

################################################################################
# Q4: What Factors Affect Scoring?
################################################################################

# Let's predict points in terms of the number of shots you take (FGA) and the
# percentage of shots that you make (eFG% is effective field percentage, and its
# technically a weighted average of how you do at different kinds of shots).

q4.model <- lm(PTS ~ FGA + `eFG%`, nba.data) 

summary(q4.model)

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

q4.model$coefficients["`eFG%`"] / 100

# Now, this tells us that for each additional percentage point of improvement
# in our shooting percentage, we expect to score an additional 0.12 points.

# We can double-check that using the predict function (note that the check.names
# option here is included so we can match the % sign in eFG%). 

scoring.eFG.50 <- predict(q4.model, newdata = 
                            data.frame(FGA = 10, `eFG%` = 0.5, check.names = F))

scoring.eFG.51 <- predict(q4.model, newdata = 
                            data.frame(FGA = 10, `eFG%` = 0.51, check.names = F))

# Above, we generated predicted values for a player who takes 10 shots per game
# and makes 50 and 51 percent of their shots. Now, we can check the difference
# in predicted values: 

scoring.eFG.51 - scoring.eFG.50

# Notice that this difference is exactly equal to our coefficient above!



################################################################################
# Q5: How Does Scoring Vary by Position?
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
# Q6: Interaction Terms
################################################################################

# Do something around interacting Center with Minutes Played - make sure you 
# have some setup for this during lecture beforehand!




################################################################################
# End of Activity
################################################################################

# You don't need to turn anything in after completing this activity (although 
# you should save this file because it will be helpful later!). 



