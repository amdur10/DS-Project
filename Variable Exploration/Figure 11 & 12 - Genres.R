
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








### FIGURE 11) GENRE NET REVENUE 

# Filter the data to include only the films up to 2016
filtered_movie_data <- movie_data[movie_data$year <= 2016,]

# Melt the data so that the genres are in one column
library(tidyverse)
melted_data <- filtered_movie_data %>% 
  pivot_longer(cols = 15:33,
               names_to = "genre",
               values_to = "is_genre")

# Remove the rows where is_genre is not true (i.e., the movie doesn't belong to the genre)
melted_data <- melted_data[melted_data$is_genre == TRUE,]

# Calculate total average profits for each genre
average_profit_genre <- aggregate(profit ~ genre, data = melted_data, FUN = mean)

# Create the bar graph
ggplot(average_profit_genre, aes(x = genre, y = profit, fill = genre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(profit, 2)), vjust = -0.3) +
  labs(title = "Total Average Net Revenue by Genre",
       x = "Genre",
       y = "Average Net Revenue in Millions($)",
       fill = "Genre") +
  theme(plot.title = element_text(hjust = 0.5), # Center the plot title
        axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability






############# FIGURE 12 - SPECIFIC FILM LINE GRAPH
############# Individual genre investigation 

# Filter the data to include only the films up to 2016
filtered_movie_data <- movie_data[movie_data$year <= 2016,]

# Calculate yearly average profits for each genre
average_profit_Animation <- aggregate(profit ~ year, data = filtered_movie_data[filtered_movie_data$genres_Animation == TRUE,], FUN = mean)
average_profit_Western <- aggregate(profit ~ year, data = filtered_movie_data[filtered_movie_data$genres_Western == TRUE,], FUN = mean)
average_profit_Comedy <- aggregate(profit ~ year, data = filtered_movie_data[filtered_movie_data$genres_Comedy == TRUE,], FUN = mean)
average_profit_Science_Fiction <- aggregate(profit ~ year, data = filtered_movie_data[filtered_movie_data$genres_Science_Fiction == TRUE,], FUN = mean)
average_profit_Horror <- aggregate(profit ~ year, data = filtered_movie_data[filtered_movie_data$genres_Horror == TRUE,], FUN = mean)
average_profit_War <- aggregate(profit ~ year, data = filtered_movie_data[filtered_movie_data$genres_War == TRUE,], FUN = mean)

# Combine the data
average_profit_genres <- rbind(data.frame(average_profit_Animation, genre = "Animation"), 
                               data.frame(average_profit_Western, genre = "Western"),
                               data.frame(average_profit_Comedy, genre = "Comedy"),
                               data.frame(average_profit_Science_Fiction, genre = "Science Fiction"),
                               data.frame(average_profit_Horror, genre = "Horror"),
                               data.frame(average_profit_War, genre = "War"))

# Define a vector of colors
my_colours <- c("#964B00", "#FFD700", "#800080", "#008000", "#0072B2", "#FF0000") 


ggplot(average_profit_genres, aes(x = year, y = profit, colour = genre)) +
  geom_smooth(se = FALSE) +
  labs(title = "Average Net Revenue Over Time by Genre",
       x = "Year",
       y = "Average Net Revenue in Millions($)",
       colour = "Genre") +
  scale_x_continuous(breaks = seq(min(average_profit_genres$year), max(average_profit_genres$year), by = 10)) +
  scale_colour_manual(values = my_colours) +
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title
