library(tidyverse)
library(randomForest)
library(caret)


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

# Remove last 2 columns 
movie_data <- movie_data[, -c(ncol(movie_data), ncol(movie_data)-1)]



#
selected_columns <- movie_data[, c(1, 4, 5, 6, 7, 8, 9)]
#selected_columns$revenue_x1 <- as.numeric(as.character(selected_columns$revenue_x1))




library(GGally)
library(ggplot2)
library(dplyr)

# Rename columns
selected_columns <- selected_columns %>%
  rename(
    'Year' = year,
    'Runtime (mins)' = runtime_minutes,
    'TMDB Popularity' = TMDB_popularity,
    'Vote Count' = vote_count,
    'Vote Rating' = vote_rating,
    'Budget' = budget,
    'Revenue' = revenue
  )

# Custom function to display both correlation coefficients and significance
cor_fun <- function(data, mapping, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr_val <- round(cor(x, y, use = "complete.obs"), 2)
  p_val <- cor.test(x, y, use = "complete.obs")$p.value
  
  label <- ifelse(p_val < 0.05, paste0(corr_val, "*"), as.character(corr_val))
  ggplot(data=data, mapping=mapping) + 
    geom_blank() +
    theme_void() +
    annotate("text", x = Inf, y = Inf, 
             label = label, 
             vjust = 1, hjust = 1, 
             size = 7,  
             color = ifelse(abs(corr_val) > 0.6, "red", "black"))
}

# Modified lower panel function to only remove the most extreme outliers
lower_fn <- function(data, mapping, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  IQR_x <- IQR(x, na.rm = TRUE)
  IQR_y <- IQR(y, na.rm = TRUE)
  
  lower_bound_x <- quantile(x, 0.25) - 12 * IQR_x
  upper_bound_x <- quantile(x, 0.75) + 12 * IQR_x
  
  lower_bound_y <- quantile(y, 0.25) - 12 * IQR_y
  upper_bound_y <- quantile(y, 0.75) + 12 * IQR_y
  
  # Filter data for plotting
  data <- data[x > lower_bound_x & x < upper_bound_x & y > lower_bound_y & y < upper_bound_y, ]
  
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))
}

# Custom function to create a blank plot for the diagonal
blank_diag_fun <- function(data, mapping, ...){
  ggplot() + geom_blank() + theme_void()
}

# Using the ggpairs function with adjusted settings
p <- ggpairs(data = selected_columns, 
             lower = list(continuous = lower_fn), 
             upper = list(continuous = cor_fun),
             diag = list(continuous = blank_diag_fun))

# Adjust column/row label sizes and strip text sizes
p + theme(axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold"),
          strip.text.x = element_text(size = 15, face = "bold"),  # Adjusting size of strip text for x-axis
          strip.text.y = element_text(size = 15, face = "bold"))  # Adjusting size of strip text for y-axis






