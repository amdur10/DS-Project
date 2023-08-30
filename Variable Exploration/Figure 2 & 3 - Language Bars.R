library(tidyverse)
library(randomForest)
library(caret)
library(ggplot2)

file_path <- "C:/Users/ArthurMarsh/Desktop/Data Science Projects/8) Dissertation - IMPORTANT/movie_modelling.csv"
# Read the CSV file
movie_data <- read_csv(file_path)
# View the first few rows of the dataset
head(movie_data)

# Convert the columns to factors
movie_data <- mutate_at(movie_data, .vars = c(449, 450, 451), .funs = as.factor)

######## COLUMNS ########
######## Revenue_x1 = 449
######## Revenue_x2 = 450
######## Revenue_x3 = 451

# Remove rows with NA values 
movie_data <- na.omit(movie_data)


######################### SECTION 2 EDA ########################################


# 1) ########### Number of Observations per Original Language ALL ###################

# Order the levels of the factor by the count
movie_data$original_language <- with(movie_data, 
                                     factor(original_language, levels = names(sort(table(original_language)))))

# Plot
p <- ggplot(movie_data, aes(x = original_language)) +
  geom_bar(aes(fill = original_language), position = "dodge") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..), vjust = -0.5, size = 3.5, position = position_dodge(width=0.9)) +
  labs(title = "Number of Observations per Original Language",
       x = "Original Language",
       y = "Number of Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  guides(fill = guide_legend(title = "Original Language Codes"))  # Adjust the legend title

print(p)



# 2) ########### Number of Observations per Original Language FILTERED ###################
###### For filtered movie data 

library(dplyr)

language_counts_df <- movie_data %>%
  group_by(original_language) %>%
  summarise(count = n()) %>%
  arrange(-count) # Arranges in descending order based on count

print(language_counts_df, n = nrow(language_counts_df))



filtered_movie_data <- movie_data %>%
  group_by(original_language) %>%
  filter(n() >= 25) %>%
  ungroup()  # This step is optional but can be useful to remove grouping from the resulting dataset

movie_data <- filtered_movie_data


language_map <- c(
  ml = "Malayalam",
  sv = "Swedish",
  cn = "Cantonese",
  zh = "Chinese",
  ko = "Korean",
  ta = "Tamil",
  de = "German",
  it = "Italian",
  es = "Spanish",
  ja = "Japanese",
  ru = "Russian",
  fr = "French",
  hi = "Hindi",
  en = "English"
)

movie_data$original_language <- as.character(language_map[movie_data$original_language])

# Order the levels of the factor by the count
ordered_languages <- names(sort(table(movie_data$original_language)))
movie_data$original_language <- factor(movie_data$original_language, levels = ordered_languages)

# Plot
p <- ggplot(movie_data, aes(x = original_language)) +
  geom_bar(aes(fill = original_language), position = "dodge") +
  geom_text(stat = 'count', aes(label = ..count.., y = ..count..), vjust = -0.5, size = 3.5, position = position_dodge(width=0.9)) +
  labs(title = "Number of Observations per Original Language",
       x = "Original Language",
       y = "Number of Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  guides(fill = guide_legend(title = "Original Language"))  # Adjust the legend title

print(p)











