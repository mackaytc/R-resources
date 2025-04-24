# NBA Data Case Study

## Overview

This folder contains R scripts and data for exploring regression analysis using NBA player performance statistics from the 2023-2024 season. These examples are designed to introduce key regression concepts in an engaging, accessible context.

## Data Description

The main dataset (`NBA-player-per-game-data-2023-2024.csv`) contains per-game statistics for NBA players during the 2023-2024 season. Key variables include:

- **Player**: Player name
- **Team**: Team abbreviation (or "2TM"/"3TM" for players traded between teams)
- **Pos**: Position (PG, SG, SF, PF, C)
- **G**: Games played
- **MP**: Minutes per game
- **PTS**: Points per game
- **FGA**: Field goal attempts per game
- **FG%**: Field goal percentage
- **3P**: Three-pointers made per game
- **AST**: Assists per game
- **TOV**: Turnovers per game
- **TRB**: Total rebounds per game
- **STL**: Steals per game
- **BLK**: Blocks per game

## R Scripts

### NBA-regression-examples.R
Introduces basic regression concepts using NBA data:
- Data cleaning and filtering for traded players
- Simple regression examples with interpretation
- Working with binary variables
- Handling factor variables (basketball positions)
- Transforming coefficients for interpretation

### NBA-regression-examples-resid-and-predict-functions.R
Demonstrates using prediction and residual functions:
- Finding players who get more assists than expected based on position
- Extending models with additional explanatory variables
- Interpreting residuals to identify over/under-performing players

### NBA-regression-examples-using-residuals.R
Explores residual analysis with two detailed examples:
- Using residuals to identify unusually efficient scorers
- Controlling for position, minutes, and shot attempts to find the best passers
- Visualizing outliers in scatter plots

## How to Use

1. Each script contains detailed comments explaining the concepts and code
2. Run the scripts sequentially, as later sections build on earlier ones
3. Pay attention to the interpretation questions throughout
4. Experiment by modifying the models or creating your own analyses

## Getting Started

The scripts automatically download the required data from GitHub - just down the file you're interested in and run the code from the beginning! 

Note that you will need the `tidyverse` package to run all files. In additional, for the the visualization example in `NBA-regression-examples-using-residuals.R`, you'll need to install the ggrepel package:

```r
# Install package (you just have to run this once)
install.packages("ggrepel")

# Load package (run this each time you open RStudio)
library(ggrepel)
```
