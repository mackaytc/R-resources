################################################################################
#
# Exploring Regression Residuals using NBA Data
#
################################################################################

# The code below provides two examples of using regression residuals to explore
# variation in an estimated relationship using our NBA data set. 

# Before you get started, click on the data set in the environment pane to see
# how the data set is structured.

library(tidyverse)
library(ggplot2)

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
  mutate(teams.played.for = n()) %>% 
  ungroup()

table(nba.data$teams.played.for)

# Now, keep all rows for players that weren't traded (first line), or the total
# rows for players that were traded (second and third lines).

nba.data <- filter(nba.data, (teams.played.for == 1)        | 
                     (teams.played.for > 1 & Team == "2TM") | 
                     (teams.played.for > 1 & Team == "3TM"))

# Finally, we'll restrict our sample to only those players who played in at 
# least 41 games (half the season) for 24 or more minutes per game (half the 
# total minutes in each game). 

nba.data <- filter(nba.data, G >= 41 & MP >= 24)

################################################################################
# Example 1: Simple OLS 
################################################################################

# Let's start with a simple example - below, we'll generate predicted values of 
# pointed scored (PTS) as a function of shots taken (FGA). 

model.1 <- lm(PTS ~ FGA, nba.data)

# Let's check our model output - each shot (FGA) is associated with a roughly
# 1.3 point increase in expected points scored. In other words, our predicted
# value of total points scored increases by 1.3 points for each additional 
# shot we assume they take.

summary(model.1)

# Now, let's save the residuals from this regression as a new variable in our 
# working data set using the resid function and our model.1 lm() object: 

nba.data$residual.points <- resid(model.1)

# What do our residuals look like? Let's use the summary function: 

summary(nba.data$residual.points)

# Several important properties of residuals to remember: 

#   - The average value of your residuals is always equal to 0 (this is a basic 
#       property of regression and happens because we've included an intercept).

#   - The residuals are the difference between the actual value of our outcome 
#       variable and the value predicted by our regression coefficients. 

# Let's see who had the largest positive values of residual points - these are 
# the players who score more than we'd expect based on their shot attempts. In
# other words, these are players who are unusually efficient - a good thing!

nba.data %>% 
  arrange(desc(residual.points)) %>% 
  select(Player, Team, Pos, PTS, FGA, residual.points) %>% 
  head(10)

# Let's see who had the most negative residual values - these are the players 
# who score less than we'd expect based on their shot volume. In other words, 
# they're very inefficient - a bad thing. 

nba.data %>% 
  arrange(residual.points) %>% 
  select(Player, Team, Pos, PTS, FGA, residual.points) %>% 
  head(10)

################################################################################
# Example 2: Incorporting FEs 
################################################################################

# Let's use regression to think about who the best passers are in the NBA. To 
# get started, we can look at a list of the players with the highest assist
# totals (AST) in the league. 

nba.data %>% 
  arrange(desc(AST)) %>% 
  select(Player, Team, Pos, AST, FGA) %>% 
  head(20)

# There's a lot of great passers on this list! But you might've noticed that
# basically everyone on the list is a point guard. By nature of the position, 
# point guards have a lot of opportunities to get assists since their job is 
# often setting up other players. 

# What if we wanted to take this into account, and estimate who the best passers
# were, accounting for differences across position? We can do this via the 
# regression below. We'll estimate assists as a function of position (as a 
# factor variable) as well as a minutes played (MP) and total shots (FGA) to 
# control for players' overall involvement in their team's offense.

model.2 <- lm(AST ~ as.factor(Pos) + FGA + MP, nba.data)

# Let's see what our estimated coefficients look like: 

summary(model.2)

# Using the resid() function, we can save residual assists as a new variable 

nba.data$residual.assists <- resid(model.2)

# What do residual assists look like? 

summary(nba.data$residual.assists)

# Who most outperformed their predicted assists, relative to their position, 
# playing time, and shots taken? Let's see: 

nba.data %>% 
  arrange(desc(residual.assists)) %>% 
  select(Player, Team, Pos, AST, FGA, Pos, MP, residual.assists) %>% 
  head(15)

# Notice we've got a much wider range of positions here now! Importantly, some
# players like Khris Middleton don't get that many assists in overall or "raw"
# terms, but when we account for his position, minutes, and shots, he looks like
# a much better passer. If you watch the Bucks at all, this probably makes 
# sense - Middleton is an excellent passer even if he doesn't rack up a ton 
# of assists. 

# Now let's look at who most under-performed their expected assists, relative to 
# their position, minutes, and shots: 

nba.data %>% 
  arrange(residual.assists)%>% 
  select(Player, Team, Pos, AST, FGA, Pos, MP, residual.assists) %>% 
  head(15)

# What do you notice here? There are a surprising number of PGs, who we said 
# before tended to get more assists on average. There's a lot of good PGs here, 
# including De'Aron Fox and Steph Curry, but importantly, most of these players
# tend to be score-first guards. Because PGs already get a lot of assists, when
# we control for their position, our model thinks they should be assisting 
# more relative to their comparison group (other PGs). 

################################################################################
# Returning to Example 1 - Visualizing Outliers 
################################################################################

# Below, we draw a scatter plot plot showing PTS ~ FGA, and label the points 
# corresponding to the 2 players who most over- and under-performed their 
# predicted PTS value based on their number of field goal attempts. 

# NOTE: To run this code, you need to install the ggrepel package if you haven't
# already done so - you can do this by running install.packages("ggrepel") in
# the console.

# A BIT OF ADVICE: If you want to draw a similar graph using your own data, I 
# would strongly encourage you to ask ChatGPT for some help. Show it your 
# working data and describe what you're trying to do - you can even copy my 
# code below and tell ChatGPT to modify it to work with your data. This is the 
# kind of thing that requires some tweaks and adjustment to get working.

library(ggrepel)

# Define the 4 players who's points we want to label on our scatter plot

outlier.players <- c("Giannis Antetokounmpo", "Shai Gilgeous-Alexander",
               "Nikola Vučević", "Scoot Henderson")

# Draw our graph, using mutate to create an indicator variable "Highlighted" for
# the players we'll be labelling, then sending this data to ggplot to draw the
# scatter plot.

nba.data %>% 
  mutate(highlighted = Player %in% outlier.players) %>%
  ggplot(aes(x = FGA, y = PTS)) +
  geom_point(aes(color = highlighted), 
             size = ifelse(nba.data$Player %in% outlier.players, 3, 1)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("black", "red")) +
  geom_text_repel(data = filter(nba.data, Player %in% outlier.players),
                  aes(label = Player), 
                  box.padding = 1.2,        # Increased for more space
                  point.padding = 0.8,      # Increased for more distance
                  segment.color = "grey50",
                  force = 10,               # Higher force pushes labels away
                  nudge_x = c(-2, 2, -2, 2),    # Push in different directions
                  nudge_y = c(2, -2, -2, 2),    # Push in different directions
                  min.segment.length = 0,   # Allow all segments
                  max.overlaps = Inf) +     # Don't hide any labels
  labs(title = "Scatterplot Showing Relationship between Points and Field Goals Attempted",
       x = "Field Goals Attempted",
       y = "Points Scored") +
  theme_minimal() +
  theme(legend.position = "none")

