################################################################################
# Simple Regression Examples Using NBA Data
################################################################################

library(tidyverse)
library(ggplot2)

################################################################################
# Data Overview
################################################################################

# We'll load player-level NBA data from the 2023-2024 season. Each row of the
# data is a player on a given team, and the columns contain various player
# statistics. Note that if a player is traded, they can appear more than once. 

nba_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/nba_regression/main/nba_data.csv")