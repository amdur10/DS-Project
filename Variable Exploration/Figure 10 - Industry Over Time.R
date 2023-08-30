library(tidyverse)

file_path <- "C:/Users/ArthurMarsh/Desktop/Data Science Projects/8) Dissertation - IMPORTANT/movies_data_finalised.csv"
# Read the CSV file
movie_data <- read_csv(file_path)
# View the first few rows of the dataset
head(movie_data)

################################## BEGIN ############################################################################

library(readr)
library(dplyr)
library(ggplot2)

# MUST CHANGE LABELS
colnames(movie_data)[10] <- "budget"
colnames(movie_data)[12] <- "revenue"
colnames(movie_data)[13] <- "profit"

# Remove any remaining rows with NA values, removing 3 films.  
movie_data <- na.omit(movie_data)


################################## READY ############################################################################


### FIGURE 10 - BUDGET, REVENUE, NET PROFIT OVER TIME graph (Up to 2016) 

# Filter the data to include only the films up to 2016
filtered_movie_data <- movie_data[movie_data$year <= 2016,]

# Calculate yearly averages
average_yearly <- aggregate(cbind(budget, revenue, profit) ~ year, data = filtered_movie_data, FUN = mean)

# Use pivot_longer() from the tidyverse to reshape the data
average_yearly_long <- average_yearly %>% 
  pivot_longer(cols = c(budget, revenue, profit),
               names_to = "variable",
               values_to = "value")

# Create the scatter plot with smoothed trend

ggplot(average_yearly_long, aes(x = year, y = value, colour = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Average Budget, Gross, and Net Revenues (up to 2016)",
       x = "Year",
       y = "Average in Millions($)") +
  scale_colour_manual(name = "Variable",
                      values = c("budget" = "red", "revenue" = "blue", "profit" = "green"),
                      labels = c("Budget", "Net Revenue", "Gross Revenue")) +
  scale_x_continuous(breaks = seq(min(average_yearly_long$year), max(average_yearly_long$year), by = 20)) +
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title