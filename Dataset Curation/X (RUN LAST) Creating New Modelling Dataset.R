library(tidyverse)

file_path <- "C:/Users/ArthurMarsh/Desktop/Data Science Projects/8) Dissertation - IMPORTANT/movies_data_finalised.csv"
# Read the CSV file
movies_data <- read_csv(file_path)
# View the first few rows of the dataset
head(movies_data)

################################## BEGIN ############################################################################

library(readr)
library(dplyr)


################################## FILTERING DATA ###################################################################


movie_data <- movies_data

# Remove film names
movie_data <- movie_data %>% select(-title)
# POTENTIALLY REMOVE BUDGET BRACKET? - Think about this 
movie_data <- movie_data %>% select(-budget_bracket)
# Remove season? - Think about this 
movie_data <- movie_data %>% select(-season)

# Rename revenue and budget 
movie_data <- rename(movie_data, budget = `budget_($M)`, revenue = `revenue_($M)`, profit = `net_profit_($M)`)


# Create 3 columns for the ratio of revenue to budget. x1 means revenue was at least higher than budget. x2 if it was over twice budget etc.
movie_data <- mutate(movie_data,
                     revenue_x1 = ifelse(revenue > budget, 1, 0),
                     revenue_x2 = ifelse(revenue > 2 * budget, 1, 0),
                     revenue_x3 = ifelse(revenue > 3 * budget, 1, 0))

# Remove profit and profitable columns 
movie_data <- select(movie_data, -profit, -profitable)


# Change revenue columns to factors
# Convert the columns to factors
movie_data <- mutate_at(movie_data, .vars = c(449, 450, 451), .funs = as.factor)


# Save to CSV 
write.csv(movie_data, "movie_modelling.csv", row.names = FALSE)
