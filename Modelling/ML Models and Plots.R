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

# Remove revenue 
movie_data <- movie_data %>% select(-revenue)

# Remove rows with NA values 
movie_data <- na.omit(movie_data)

# To keep only x1
#movie_data <- movie_data[,-c(ncol(movie_data), ncol(movie_data)-1)]

# To keepy only x2
movie_data <- movie_data[,-c(ncol(movie_data), ncol(movie_data)-2)]

#To keep only x3
#movie_data <- movie_data[,-c(ncol(movie_data)-2, ncol(movie_data)-1)]

## Remove languages <25
filtered_movie_data <- movie_data %>%
  group_by(original_language) %>%
  filter(n() >= 25) %>%
  ungroup()  # This step is optional but can be useful to remove grouping from the resulting dataset

movie_data <- filtered_movie_data


# One-hot encoding for 'original_language'
language_dummies <- model.matrix(~original_language - 1, data=movie_data)
language_dummies_df <- as.data.frame(language_dummies)

# One-hot encoding for 'month'
month_dummies <- model.matrix(~month - 1, data=movie_data)
month_dummies_df <- as.data.frame(month_dummies)

# Drop the 'original_language' and 'month' columns
movie_data <- movie_data %>% select(-original_language, -month)

# Add the dummy variables
movie_data <- cbind(movie_data, language_dummies_df, month_dummies_df)



# Extract the column to be moved
column_to_move <- movie_data[, 446]

# Store the original name
original_name <- colnames(movie_data)[446]

# Drop the column from its original position
movie_data <- movie_data[,-446]

# Append the column to the end of the dataframe and keep its original name
movie_data[[original_name]] <- column_to_move








# Load the H2O library
library(h2o)

# Initialize H2O
h2o.init(nthreads= 14)

# Convert dataframe to H2OFrame
movie_data_h2o <- as.h2o(movie_data)


# Specify predictors and response
# (Assuming the response is the last column now)
predictors <- colnames(movie_data_h2o)[1:(ncol(movie_data_h2o)-1)]
response <- colnames(movie_data_h2o)[ncol(movie_data_h2o)]


# Split the data into training and testing
splits <- h2o.splitFrame(
  data = movie_data_h2o, 
  ratios = 0.75,   # Specify the ratio for the training set here
  seed = 1234
)
train <- splits[[1]]
test <- splits[[2]]



# Set a maximum number of models. Here we set a max of 20 models.
max_models <- 20

# Run H2O's AutoML
automl_models <- h2o.automl(
  x = predictors,
  y = response,
  training_frame = train,
  leaderboard_frame = test,
  max_models = max_models,
  seed = 1234
)


# View the entire AutoML Leaderboard
lb <- h2o.get_leaderboard(automl_models, extra_columns = "ALL")
print(lb, n = Inf)





# Extract the best model
best_model <- automl_models@leader
# Get model summary
summary(best_model)



# Predict on test data
predictions <- h2o.predict(best_model, newdata = test)

# Get the performance of the best model on the test data
perf <- h2o.performance(best_model, newdata = test)


# Get the accuracy
accuracy <- h2o.accuracy(perf, threshold = 0.5)
print(paste("Accuracy: ", accuracy))

# Get the AUC
auc <- h2o.auc(perf)
print(paste("AUC: ", auc))

# Get the MSE
mse <- h2o.mse(perf)
print(paste("MSE: ", mse))

# Get the RMSE
rmse <- h2o.rmse(perf)
print(paste("RMSE: ", rmse))

# Get the logloss
logloss <- h2o.logloss(perf)
print(paste("Logloss: ", logloss))

# Get the confusion matrix
confusion_matrix <- h2o.confusionMatrix(perf)
print(confusion_matrix)                                      






################### Creating Plot for Average Scaled Importance 


# 1. Retrieve the base model IDs from the stacked ensemble.
base_model_ids <- best_model@model$base_models

# 2. Extract variable importance from each of the base models.
# Initialize an empty list to store variable importances
varimps <- list()

# Loop over the base models
for (i in seq_along(base_model_ids)) {
  # Get the model
  model <- h2o.getModel(base_model_ids[[i]])
  
  # Try to get variable importance
  # This will fail silently for models that don't support variable importance
  var_imp <- tryCatch({
    as.data.frame(h2o.varimp(model))
  }, error = function(e) NULL)
  
  # If we got variable importance, store it in the list
  if (!is.null(var_imp)) {
    # Name the list element with the model id for easier reference
    varimps[[base_model_ids[[i]]]] <- var_imp
  }
}

# 3. Convert the list of variable importances to a data frame
all_varimps <- do.call(rbind, varimps)

# Check if all_varimps is a data frame now
print(is.data.frame(all_varimps))

# Compute average importance for each variable
avg_varimp <- aggregate(relative_importance ~ variable, all_varimps, mean)

# Print the average importances
print(avg_varimp)

# Order by importance
avg_varimp <- avg_varimp[order(-avg_varimp$relative_importance), ]

# Print the top 20 variables
print(head(avg_varimp, 20))

########## Scaled importance
# Compute average scaled_importance for each variable
avg_scaled_imp <- aggregate(scaled_importance ~ variable, all_varimps, mean)

# Print the average scaled importances
print(avg_scaled_imp)

# Order by scaled_importance
avg_scaled_imp <- avg_scaled_imp[order(-avg_scaled_imp$scaled_importance), ]

# Print the top 20 variables
print(head(avg_scaled_imp, 20))



# USE THIS for X1
new_var_names <- c("vote_count" = "Vote Count", 
                   "TMDB_popularity" = "TMDB Popularity", 
                   "year" = "Year", 
                   "budget" = "Budget", 
                   "vote_rating" = "Vote Rating", 
                   "runtime_minutes" = "Runtime (Minutes)", 
                   "original_languagehi" = "Language: Hindi",
                   "original_languageen" = "Language: English", 
                   "genres_Comedy" = "Genre: Comedy",
                   "genres_Horror" = "Genre: Horror",
                   "keywords_sequel" = "Keyword: Sequel",
                   "studios_Walt_Disney_Pictures" = "Studio: Walt Disney Pictures",
                   "genres_Romance" = "Genre: Romance",
                   "original_languageru" = "Language: Russian",
                   "monthJune" = "Month: June",
                   "keywords_duringcreditsstinger" = "Keyword: During Credits Stinger",
                   "original_languageta" = "Language: Tamil",
                   "monthJuly" = "Month: July",
                   "studios_Metro-Goldwyn-Mayer" = "Studio: Metro-Goldwyn-Mayer",
                   "monthMarch" = "Month: March")



# USE THIS for X2

new_var_names <- c(
  "vote_count" = "Vote Count", 
  "TMDB_popularity" = "TMDB Popularity", 
  "budget" = "Budget", 
  "year" = "Year", 
  "vote_rating" = "Vote Rating", 
  "runtime_minutes" = "Runtime (Minutes)", 
  "original_languagehi" = "Language: Hindi",
  "original_languageen" = "Language: English", 
  "original_languageru" = "Language: Russian",
  "original_languageta" = "Language: Tamil",
  "genres_Horror" = "Genre: Horror",
  "genres_Documentary" = "Genre: Documentary",
  "original_languageit" = "Language: Italian",
  "monthOctober" = "Month: October",
  "keywords_woman_director" = "Keyword: Woman Director",
  "genres_Romance" = "Genre: Romance",
  "monthSeptember" = "Month: September",
  "monthJanuary" = "Month: January",
  "genres_History" = "Genre: History",
  "genres_Music" = "Genre: Music"
)



# USE THIS for X3
new_var_names<- c(
  "vote_count" = "Vote Count",
  "budget" = "Budget",
  "TMDB_popularity" = "TMDB Popularity",
  "vote_rating" = "Vote Rating",
  "year" = "Year",
  "monthOctober" = "Month: October",
  "original_languageru" = "Language: Russian",
  "runtime_minutes" = "Runtime (Minutes)",
  "monthApril" = "Month: April",
  "genres_Fantasy" = "Genre: Fantasy",
  "original_languagehi" = "Language: Hindi",
  "genres_History" = "Genre: History",
  "monthSeptember" = "Month: September",
  "genres_Documentary" = "Genre: Documentary",
  "original_languageta" = "Language: Tamil",
  "studios_Metro-Goldwyn-Mayer" = "Studio: Metro-Goldwyn-Mayer",
  "genres_Horror" = "Genre: Horror",
  "original_languageen" = "Language: English",
  "keywords_woman_director" = "Keyword: Woman Director",
  "monthMay" = "Month: May"
)



# Replace the variable names in the data frame
avg_scaled_imp$variable <- new_var_names[avg_scaled_imp$variable]

# Create a bar plot of the top 20 variables
ggplot(head(avg_scaled_imp, 20), aes(x = reorder(variable, scaled_importance), y = scaled_importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(scaled_importance, 2)), hjust = -0.1, color = "black") +
  labs(title = "Average Scaled Variable Importance Across Stacked-Ensemble for x2 Revenue ",
       x = "Variable",
       y = "Average Scaled Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()





