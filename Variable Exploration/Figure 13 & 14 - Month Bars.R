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

############# FIGURE 13 ################
############# NET REVENUE V RELEASE month ##################


# Calculate the average net profit by month
average_profit <- aggregate(profit ~ month, data = movie_data, FUN = mean)

# Convert month to a factor with the levels ordered
average_profit$month <- factor(average_profit$month, levels = c("January", "February", "March", "April", "May", "June", 
                                                                "July", "August", "September", "October", "November", "December"))

ggplot(average_profit, aes(x = month, y = profit)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label=round(profit, 2)), vjust=-0.3) +
  labs(title = "Average Net Revenue by Month",
       x = "Month",
       y = "Average Net Revenue in Millions($)") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title




######## FIGURE 14  Number of Films by Month #################

# Count the number of observations per month
obs_count <- aggregate(profit ~ month, data = movie_data, FUN = length)
colnames(obs_count) <- c("month", "count")

# Convert month to a factor with the levels ordered
ordered_months <- c("January", "February", "March", "April", "May", "June", 
                    "July", "August", "September", "October", "November", "December")
obs_count$month <- factor(obs_count$month, levels = ordered_months)

# Plot the data
ggplot(obs_count, aes(x = month, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label=count), vjust=-0.3) +
  labs(title = "Number of Films Released by Month",
       x = "Month",
       y = "Quantity of Films") +
  scale_y_continuous(limits = c(0, 800)) + # Manually set y-axis limits here
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
        axis.text.x = element_text(angle = 45, hjust = 1)) # Adjust the x-axis labels for better visualization