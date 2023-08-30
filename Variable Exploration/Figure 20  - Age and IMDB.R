library(tidyverse)
library(dplyr)
library(ggplot2)

file_path <- "C:/Users/ArthurMarsh/Desktop/GENDER FILM CODE/movies.csv"
# Read the CSV file
movie_industry <- read_csv(file_path)


# Remove broken film
movie_industry <- movie_industry[movie_industry$name != 'Monsters University', ]
movie_industry <- movie_industry[movie_industry$name != 'Finding Nemo', ]


# Remove useless columns 
movie_industry_selected <- movie_industry[, c(2,4,6,13)]
movie_industry_selected <- na.omit(movie_industry_selected)


# Remove redundant ratings 
ratings_to_remove <- c("X", "Unrated", "TV-MA", "NC-17", "Approved", "Not Rated", "TV-14", "TV-PG")
movie_industry_selected <- movie_industry_selected[!movie_industry_selected$rating %in% ratings_to_remove,]

# Remove 2020 year as it is too few results 
movie_industry_selected <- movie_industry_selected[!movie_industry_selected$year %in% c(2019, 2020),]


# Just tables 
table(movie_industry_selected$rating)
table(movie_industry_selected$year)






# Make list of unique ratings after removing the ones you specified earlier
ratings_list <- unique(movie_industry_selected$rating)

# Create an empty list to hold data frames
avg_score_per_rating <- list()

# For each unique rating, filter the data, group by year and calculate the average score
for (i in seq_along(ratings_list)) {
  avg_score_per_rating[[i]] <- movie_industry_selected %>%
    filter(rating == ratings_list[i]) %>%
    group_by(year) %>%
    summarise(avg_score = mean(score, na.rm = TRUE)) %>%
    mutate(rating = ratings_list[i])  # add a column with the rating
}

# Combine all data frames into one
avg_score_per_rating_df <- bind_rows(avg_score_per_rating)

# Plot
ggplot(avg_score_per_rating_df, aes(x = year, y = avg_score, color = rating)) +
  geom_smooth(se = FALSE) +
  geom_point(size = 0.8) +
  scale_x_continuous(breaks = seq(min(movie_industry_selected$year, na.rm = TRUE), max(movie_industry_selected$year, na.rm = TRUE), by = 2)) +
  scale_y_continuous(breaks = seq(0, max(movie_industry_selected$score, na.rm = TRUE), by = 1),
                     expand = expand_scale(mult = c(0, 0.05)),
                     name = "Average IMDB Score") +
  labs(x = "Year", 
       title = "Average IMDB Score per Year by Age Rating", 
       color = "Age Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.title.position = "plot")




################## BAR GRAPH 

# Calculate average scores per rating
avg_score_per_rating_total <- movie_industry_selected %>%
  group_by(rating) %>%
  summarise(avg_score = mean(score, na.rm = TRUE))

ggplot(avg_score_per_rating_total, aes(x = rating, y = avg_score, fill = rating)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label=round(avg_score, 2)), vjust=1.6, color="black", size=5, position = position_stack(vjust = .5)) +
  scale_y_continuous(breaks = seq(1, 10, by = 1)) +
  coord_cartesian(ylim = c(1, 10)) +
  labs(x = "Age Rating", 
       y = "Average IMDB Score", 
       title = "Average IMDB Score by Age Rating", 
       fill = "Age Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.title.position = "plot")







