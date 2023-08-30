library(tidyverse)

file_path <- "C:/Users/ArthurMarsh/Desktop/Data Science Projects/8) Dissertation - IMPORTANT/movies_data_finalised.csv"
# Read the CSV file
movie_data <- read_csv(file_path)
# View the first few rows of the dataset
head(movie_data)

################################## BEGIN ############################################################################

library(readr)
library(dplyr)

# MUST CHANGE LABELS
colnames(movie_data)[10] <- "budget"
colnames(movie_data)[12] <- "revenue"
colnames(movie_data)[13] <- "profit"



# Remove any remaining rows with NA values, removing 3 films.  
movie_data <- na.omit(movie_data)





########## Bar chart for profit to budget ratio 

# Define the language codes and names
language_names <- c(
  cn = "Chinese",
  de = "German",
  en = "English",
  es = "Spanish",
  fr = "French",
  hi = "Hindi",
  it = "Italian",
  ja = "Japanese",
  ko = "Korean",
  ml = "Mali",
  ru = "Russian",
  sv = "Swedish",
  ta = "Tamil",
  te = "Telugu",
  zh = "Zhuang"
)

# Calculate the count for each language
language_counts <- movie_data %>% 
  count(original_language)

# Create a subset of movie_data including only languages with more than 20 occurrences
filtered_movie_data <- movie_data %>% 
  filter(original_language %in% language_counts$original_language[language_counts$n > 20])

# Calculate average revenue, and budget for each language
language_data <- filtered_movie_data %>%
  group_by(original_language) %>%
  summarise(average_revenue = mean(revenue, na.rm = TRUE),
            average_budget = mean(budget, na.rm = TRUE), 
            .groups = 'drop')

# Calculate the ratio of revenue to budget
language_data <- language_data %>%
  mutate(revenue_budget_ratio = average_revenue / average_budget)

# Add the language names to the data frame
language_data$language_name <- language_names[language_data$original_language]

# Create bar chart
ggplot(language_data, aes(x = reorder(language_name, revenue_budget_ratio), y = revenue_budget_ratio, fill = language_name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(revenue_budget_ratio, 2)), vjust = -0.5) +
  labs(x = "Language", 
       y = "Revenue to Budget Ratio",
       fill = "Language",
       title = "Revenue to Budget Ratio by Language") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
