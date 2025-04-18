################################################################################
#
# Exploring Regression Residuals using NBA Data
#
################################################################################

# The code below provides INSERT. 

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

# Let's explain ree 

model.1 <- lm(PTS ~ FGA, nba.data)

summary(model.1)

# Now, let's save the residuals from this regression as a new variable in our 
# working data set 

nba.data$residual.points <- resid(model.1)

# What do our residuals look like? Let's use the summary function: 

summary(nba.data$residual.points)

# Several important properties of residuals to remember: 

#   - The average value of your residuals is always equal to 0 (this is a basic 
#       property of regression and happens because we've included an intercept).

#   - The residuals are the difference between the actual value of our outcome 
#       variable and the value predicted by our regression coefficients. 

# Let's see who had the largest positive values of residual points: 

nba.data %>% 
  arrange(desc(residual.points)) %>% 
  select(Player, Team, Pos, PTS, FGA, residual.points) %>% 
  head(10)

# Let's see who had the most negative residual values: 

nba.data %>% 
  arrange(residual.points) %>% 
  select(Player, Team, Pos, PTS, FGA, residual.points) %>% 
  head(10)


################################################################################
# Returning to Example 1 - Visualizing Outliers 
################################################################################

# Below, we draw a scatter plot plot showing PTS ~ FGA, and label the points 
# corresponding to the 2 players who most over- and under-performed their 
# predicted PTS value based on their number of field goal attempts. 

# NOTE: To run this code, you need to install the ggrepel package if you haven't
# already done so - you can do this by running install.packages("ggrepel") in
# the console.

library(ggrepel)

outlier.players <- c("Giannis Antetokounmpo", "Shai Gilgeous-Alexander",
               "Nikola Vučević", "Scoot Henderson")

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

