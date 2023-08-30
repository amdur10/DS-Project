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


################## Vote Rating X Revenue #############################

# Compute IQR for vote_rating
Q1_vote_rating <- quantile(movie_data$vote_rating, 0.25)
Q3_vote_rating <- quantile(movie_data$vote_rating, 0.75)
IQR_vote_rating <- Q3_vote_rating - Q1_vote_rating

# Compute IQR for revenue
Q1_revenue <- quantile(movie_data$revenue, 0.25)
Q3_revenue <- quantile(movie_data$revenue, 0.75)
IQR_revenue <- Q3_revenue - Q1_revenue

# Define bounds for outliers for vote_rating using 2 * IQR
lower_bound_vote_rating <- Q1_vote_rating - 2 * IQR_vote_rating
upper_bound_vote_rating <- Q3_vote_rating + 2 * IQR_vote_rating

# Define bounds for outliers for revenue using 2 * IQR
lower_bound_revenue <- Q1_revenue - 2 * IQR_revenue
upper_bound_revenue <- Q3_revenue + 2 * IQR_revenue

# Filter out the outliers
filtered_data_vote_revenue <- movie_data %>%
  filter(vote_rating > lower_bound_vote_rating & vote_rating < upper_bound_vote_rating) %>%
  filter(revenue > lower_bound_revenue & revenue < upper_bound_revenue)

# Plot the filtered data with a centered title
ggplot(filtered_data_vote_revenue, aes(x = vote_rating, y = revenue)) +
  geom_point(aes(color = revenue), alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Rating vs Revenue (Without Outliers)", x = "Vote Rating", y = "Revenue in Millions ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # This line centers the title


################### Budget x Revenue ################################

# Compute IQR for budget 
Q1_budget <- quantile(movie_data$budget, 0.25)
Q3_budget <- quantile(movie_data$budget, 0.75)
IQR_budget <- Q3_budget - Q1_budget

# Compute IQR for revenue
Q1_revenue <- quantile(movie_data$revenue, 0.25)
Q3_revenue <- quantile(movie_data$revenue, 0.75)
IQR_revenue <- Q3_revenue - Q1_revenue

# Define bounds for outliers for budget using 3 * IQR
lower_bound_budget <- Q1_budget - 2 * IQR_budget
upper_bound_budget <- Q3_budget + 2 * IQR_budget

# Define bounds for outliers for revenue using 3 * IQR
lower_bound_revenue <- Q1_revenue - 2 * IQR_revenue
upper_bound_revenue <- Q3_revenue + 2 * IQR_revenue

# Filter out the outliers
filtered_data <- movie_data %>%
  filter(budget > lower_bound_budget & budget < upper_bound_budget) %>%
  filter(revenue > lower_bound_revenue & revenue < upper_bound_revenue)

# Plot the filtered data with a centered title
ggplot(filtered_data, aes(x = budget, y = revenue)) +
  geom_point(aes(color = revenue), alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Budget vs Revenue (Without Outliers)", x = "Budget in Millions ($)", y = "Revenue in Millions ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # This line centers the title

