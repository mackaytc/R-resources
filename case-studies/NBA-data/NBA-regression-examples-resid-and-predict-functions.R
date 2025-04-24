################################################################################
#
# Exploring Predicted and Residual Values using NBA Data
#
################################################################################

# The code below demonstrates how to use the predict() and resid() functions
# to generate predicted values and residuals from regression models. We'll
# examine how these values can help us understand which NBA players over-
# or under-perform expectations when it comes to assists.

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
# Exploring Assists: Who are the Best Passers?
################################################################################

# Let's start by looking at which players have the most assists in the league.
# This gives us a starting point for understanding passing in the NBA.

# Players with the most assists:

nba.data %>% 
  arrange(desc(AST)) %>% 
  select(Player, Team, Pos, AST) %>% 
  head(20)

# Notice that the list is dominated by point guards (PG). This makes sense since
# distributing the ball is a key responsibility for that position. To understand
# this better, let's calculate the average assists by position:

# Average assists by position:

nba.data %>% 
  group_by(Pos) %>% 
  summarize(average.assists = mean(AST))

# As expected, PGs average more assists than other positions. To control for
# these position-based differences, we can use regression.

################################################################################
# Regression Model 1: Assists by Position
################################################################################

# Let's build a simple model that predicts assists based on position alone.
# This will help us identify players who get more assists than we'd expect
# given their position.

# Estimate a regression with position as our only explanatory variable:

model.position <- lm(AST ~ as.factor(Pos), nba.data)

# Summary of our model:

summary(model.position)

# Let's add predicted values and residuals to our data set:

nba.data$predicted.assists.1 <- predict(model.position)
nba.data$residual.assists.1 <- resid(model.position)

# Now, let's see which players most exceed their position-based expectations:

nba.data %>% 
  arrange(desc(residual.assists.1)) %>% 
  select(Player, Team, Pos, AST, predicted.assists.1, residual.assists.1) %>% 
  head(20)

################################################################################
# Regression Model 2: Assists by Position and Shot Attempts
################################################################################

# Position isn't the only factor affecting assists. A player's offensive role
# also matters - players who shoot more, and thus are a larger part of their 
# team's offense, may have more chances to get an assist (other players with 
# fewer shots may be defensive specialists, centers, etc.).

# Let's add Field Goal Attempts (FGA) to our model by estimating a regression
# with position and FGA as explanatory variables:

model.position.fga <- lm(AST ~ as.factor(Pos) + FGA, nba.data)

# Summary of our expanded model:

summary(model.position.fga)

# Let's add these new predicted values and residuals to our data set:

nba.data$predicted.assists.2 <- predict(model.position.fga)
nba.data$residual.assists.2 <- resid(model.position.fga)

# Now, let's see which players most exceed their expectations when accounting
# for both position and shot attempts:

nba.data %>% 
  arrange(desc(residual.assists.2)) %>% 
  select(Player, Team, Pos, AST, FGA, 
         predicted.assists.2, residual.assists.2) %>% 
  head(20)
