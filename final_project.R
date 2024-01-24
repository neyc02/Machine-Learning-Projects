
setwd("C:\\Users\\h043y640\\OneDrive - University of Kansas\\Desktop\\Classes\\750 Machine Learning\\")

#### EDA 
library(ggplot2)
library(dplyr)

# the downloaded datasets are from https://www.kaggle.com/datasets/econdata/pisa-test-scores
# I combined the two datasets into one for data preparation/cleaning 
data<-read.csv("pisa_data.csv")
## 1. Data Structure Analysis
str(data)
summary(data)
colSums(is.na(data))

## 2. Basic Statistical Summaries of Numerical Variables
summary(select_if(data, is.numeric))

## 3. Frequency Distributions of the Categorical Variable: Race
categorical_vars <- select_if(data, is.character)
sapply(categorical_vars, table)

# 4. Correlation Analysis
# numerical_vars <- select_if(data, is.numeric)
# correlations <- cor(numerical_vars, use = "complete.obs")
# print(correlations)

# 5. Visualizations
# Bar Chart for Race 
raceeth_data <- data.frame(
  RaceEthnicity = c("American Indian/Alaska Native", "Asian", "Black", "Hispanic", 
                    "More than one race", "Native Hawaiian/Other Pacific Islander", "White"),
  Count = c(51, 204, 635, 1184, 177, 40, 2894)
)

# accessible color palette
ggplot(raceeth_data, aes(x = RaceEthnicity, y = Count, fill = RaceEthnicity)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.7) + 
  theme_minimal() +
  labs(title = "Distribution of Students by Race/Ethnicity",
       y = "Count") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank())

# Reading Score Histogram 
ggplot(data, aes(x = readingScore)) +
  geom_histogram(binwidth = 30, fill = "lightblue", color = "black") +  
  theme_minimal() +
  scale_x_continuous(breaks = seq(200, 800, by = 100)) +  # Set x breaks from 200 to 800
  labs(title = "Distribution of Reading Scores",
       x = "Reading Score",
       y = "Count")
# # Reading score boxplot by race 
# ggplot(data, aes(x = raceeth, y = readingScore, fill = raceeth)) +
#   geom_boxplot() +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels for better readability
#         legend.position = "none") +  # Hide legend if it's not needed
#   labs(title = "Reading Scores by Race/Ethnicity",
#        x = "Race/Ethnicity",
#        y = "Reading Score")
## Missing data analysis
# Create a summary table of missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values <- data.frame(Variable = names(missing_values), MissingValues = missing_values)
missing_values <- missing_values[missing_values$MissingValues > 0, ]

# Missing values chart
ggplot(missing_values, aes(x = Variable, y = MissingValues)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variable", y = "Number of Missing Values", title = "Missing Data in Each Variable")

# Mean imputation for specified numerical variables
data <- data %>%
  mutate(minutesPerWeekEnglish = ifelse(is.na(minutesPerWeekEnglish), mean(minutesPerWeekEnglish, na.rm = TRUE), minutesPerWeekEnglish),
         studentsInEnglish = ifelse(is.na(studentsInEnglish), mean(studentsInEnglish, na.rm = TRUE), studentsInEnglish),
         schoolSize = ifelse(is.na(schoolSize), mean(schoolSize, na.rm = TRUE), schoolSize))

# Investigate the missing pattern of parental information
# Convert columns to binary indicators for missingness (1 for missing, 0 for not missing)
pisa_missing <- data %>%
  mutate_at(vars(fatherHS, fatherBachelors, fatherWork, motherHS, motherBachelors, motherWork),
            ~as.numeric(ifelse(is.na(.), 1, 0)))

Cross-tabulation
# For father's variables
table(pisa_missing$fatherHS, pisa_missing$fatherBachelors)
table(pisa_missing$fatherHS, pisa_missing$fatherWork)
table(pisa_missing$fatherBachelors, pisa_missing$fatherWork)

# For mother's variables
table(pisa_missing$motherHS, pisa_missing$motherBachelors)
table(pisa_missing$motherHS, pisa_missing$motherWork)
table(pisa_missing$motherBachelors, pisa_missing$motherWork)

# Chi-square test for independence
chisq.test(pisa_missing$fatherHS, pisa_missing$fatherBachelors)
chisq.test(pisa_missing$motherHS, pisa_missing$motherBachelors)
### These two parental education history are significantly associated. 
# Check if there's actually any observations whose father completed college, but with missing values for fatherHigh
# also check for mother variables
count <- sum(data$fatherBachelors == 1 & is.na(data$fatherHS), na.rm = TRUE)
print(count)
#count is 14
count <- sum(data$motherBachelors == 1 & is.na(data$motherHS), na.rm = TRUE)
count
# count is 11 

#if fatherHS is missing, but fatherBachelors is 1,  impute fatherHS as 1 as well. 
# same for motherHS and motherBachelors. 
data$fatherHS[is.na(data$fatherHS) & data$fatherBachelors == 1] <- 1
data$motherHS[is.na(data$motherHS) & data$motherBachelors == 1] <- 1

# For the remaining missing values, I decided to create a NA category 
# Function to handle NA values 
handle_na <- function(column) {
  if ((is.integer(column) || is.numeric(column)) && all(column %in% c(0, 1, NA))) {
    # Replace NA in binary integer or numeric columns with a temporary placeholder
    column[is.na(column)] <- -1
    # Convert binary integers or numerics to factor and add "Missing" for the placeholder
    column <- factor(column, levels = c(0, 1, -1), labels = c("0", "1", "Missing"))
  } else if (is.character(column)) {
    # Replace NA in character columns with "Missing"
    column[is.na(column)] <- "Missing"
  }
  return(column)
}

# Apply the function to each column in the dataframe
data <- as.data.frame(lapply(data, handle_na))

colSums(is.na(data))
## missing data analysis is completed 

## Scale the numerical variable 
# Standardizing columns using the scale function
data$schoolSize <- scale(data$schoolSize, center = TRUE, scale = TRUE)
data$minutesPerWeekEnglish <- scale(data$minutesPerWeekEnglish, center = TRUE, scale = TRUE)
# Standardizing columns using the scale function
data$schoolSize <- scale(data$schoolSize, center = TRUE, scale = TRUE)
data$studentsInEnglish <- scale(data$studentsInEnglish, center = TRUE, scale = TRUE)

write.csv(data,"pisa_data_cleaned.csv",row.names = FALSE)

data<- read.csv("pisa_data_cleaned.csv")

## Split the data into training and testing
## randomly split data to training and testing
set.seed(1234)
n<- nrow(data)
index<- sample(n, 0.7*n)
train<- data[index,]
test<- data[-index, ]

## Start with linear regression 
# Fit a linear model using lm
library(caret)
# Set up 10 fold cross-validation
control <- trainControl(method = "cv", number = 10)

# Fit a linear model using cross-validation
set.seed(1234)
model_linear <- train(readingScore ~ ., data = data, method = "lm", trControl = control)

# Make predictions using the model on the testing data
predictions <- predict(model_linear, newdata = test)

# Extract coefficients from the model
coefficients <- coef(model_linear$finalModel)
# Create a data frame for plotting
coef_df <- data.frame(
  Variable = names(coefficients),
  Coefficient = coefficients
)

library(ggplot2)
coef_df <- coef_df[coef_df$Variable != "(Intercept)",]

# Add a new column for the absolute values of the coefficients
coef_df$AbsCoefficient <- abs(coef_df$Coefficient)

# Sort the data frame by the absolute values of the coefficients in descending order
coef_df <- coef_df[order(coef_df$AbsCoefficient, decreasing = TRUE),]

# Coefficient plot with sorted coefficients
ggplot(coef_df, aes(x = Variable, y = Coefficient)) +
  geom_col() +
  coord_flip() +  # Flips the axes for a horizontal plot
  theme_minimal() +
  labs(title = "Coefficient Plot",
       x = "Variables",
       y = "Coefficients")


library(Metrics)

# Calculating various performance metrics
mse <- mse(test$readingScore, predictions)
rmse <- sqrt(mse)
mae <- mae(test$readingScore, predictions)

# Print the metrics
cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("Mean Absolute Error:", mae, "\n")

coef_summary <- summary(model_linear)$coefficients
coef_summary <- coef_summary[order(abs(coef_summary[, "Estimate"]), decreasing = TRUE), ]
top_predictors <- head(coef_summary, 5)
print(top_predictors)
## LASSO
# Load necessary libraries
library(glmnet)

# Prepare the data: Exclude the response variable
X <- train[, colnames(train) != 'readingScore']

# Convert categorical variables to dummy variables
X <- model.matrix(~ . - 1, data = X)

# Response variable
Y <- train$readingScore

# Standardize the predictors
X_standardized <- scale(X)

# Fit LASSO model
set.seed(123)  # for reproducibility
cv_fit <- cv.glmnet(X_standardized, Y, alpha = 1) # nfold = 10 by default

# Display the lambda value that gives minimum mean cross-validated error
print(cv_fit$lambda.min)

# Fit the final model using this lambda
lasso_model <- glmnet(X_standardized, Y, alpha = 1, lambda = cv_fit$lambda.min)

# View the coefficients
coef(lasso_model)

# evaluate LASSO using testing data
# Prepare the predictors for the test set
X_test <- test[, colnames(test) != 'readingScore']
X_test <- model.matrix(~ . - 1, data = X_test)
X_test_standardized <- scale(X_test)
Y <- train$readingScore

# Make predictions on the test set
predictions <- predict(lasso_model, newx = X_test_standardized, s = cv_fit$lambda.min)


Y_test <- test$readingScore
# Calculate MSE, RMSE, and MAE
mse_lasso <- mse(Y_test, predictions)
rmse_lasso <- sqrt(mse_lasso)
mae_lasso <- mae(Y_test, predictions)

# Print the metrics
cat("LASSO Mean Squared Error:", mse_lasso, "\n")
cat("LASSO Root Mean Squared Error:", rmse_lasso, "\n")
cat("LASSO Mean Absolute Error:", mae_lasso, "\n")


## CART
# Fit CART model with cross-validation
library(rpart)
library(caret)

data_matrix <- model.matrix(readingScore ~ ., data = data)

# Convert the response variable to a vector
response_vector <- data$readingScore

# Split the data into training and testing
set.seed(123)  # for reproducibility

train_data <- data_matrix[index, ]
train_response <- response_vector[index]
test_data <- data_matrix[-index, ]
test_response <- response_vector[-index]

# Fit CART model with cross-validation
train_control <- trainControl(method = "cv", number = 10)
cart_model_cv <- train(train_data, train_response, method = "rpart",
                       trControl = train_control, tuneLength = 10)

# Best model based on cross-validation
best_cart_model <- cart_model_cv$finalModel

# Print the best complexity parameter
print(cart_model_cv$bestTune)

# Convert test_data back to a data.frame
test_data_df <- as.data.frame(test_data)

# Make predictions using the best model
cart_predictions_cv <- predict(best_cart_model, newdata = test_data_df)


# Calculate performance metrics
cart_mse_cv <- mse(test_response, cart_predictions_cv)
cart_rmse_cv <- sqrt(cart_mse_cv)
cart_mae_cv <- mae(test_response, cart_predictions_cv)

# Print the metrics
cat("CART CV Mean Squared Error:", cart_mse_cv, "\n")
cat("CART CV Root Mean Squared Error:", cart_rmse_cv, "\n")
cat("CART CV Mean Absolute Error:", cart_mae_cv, "\n")



## Random forest
library(randomForest)
# Set up cross-validation and control for Random Forest
set.seed(123)  # for reproducibility
train_control <- trainControl(method = "cv", number = 10)

# Tune the Random Forest model
tuned_rf_model <- train(readingScore ~ ., data = train, method = "rf", 
                        trControl = train_control, tuneLength = 5)

# Best model based on cross-validation
best_rf_model <- tuned_rf_model$finalModel

# Make predictions using the best model
rf_predictions_cv <- predict(best_rf_model, newdata = as.data.frame(test_data))

# Calculate performance metrics
rf_mse_cv <- mse(test$readingScore, rf_predictions_cv)
rf_rmse_cv <- sqrt(rf_mse_cv)
rf_mae_cv <- mae(test$readingScore, rf_predictions_cv)

# Print the metrics
cat("Random Forest CV Mean Squared Error:", rf_mse_cv, "\n")
cat("Random Forest CV Root Mean Squared Error:", rf_rmse_cv, "\n")
cat("Random Forest CV Mean Absolute Error:", rf_mae_cv, "\n")

# Print the best hyperparameters
print(tuned_rf_model$bestTune)

# Variable importance
importance <- varImp(best_rf_model)
varImpPlot(best_rf_model)
## Support Vector Machine 
library(e1071)
library(kernlab)

# Set up cross-validation and control for SVM
set.seed(123)  # for reproducibility
train_control <- trainControl(method = "cv", number = 10)

# Fit SVM model with cross-validation
tuned_svm_model <- train(readingScore ~ ., data = train, method = "svmRadial",
                         trControl = train_control, tuneLength = 5)
print(tuned_svm_model$bestTune)

best_sigma <- 0.02463624
best_C <- 0.25

# Train the SVM model using the best parameters
# convert sigma to gamma, as the svm() function uses gamma. 
# The relationship is gamma = 1 / (2 * sigma^2)
best_gamma <- 1 / (2 * best_sigma^2)

svm_model <- svm(readingScore ~ ., data = train, cost = best_C, gamma = best_gamma)

# Predict on test data
predictions <- predict(svm_model, test)

# Calculate performance metrics
svm_mse_cv <- mse(test$readingScore, predictions)
svm_rmse_cv <- sqrt(svm_mse_cv)
svm_mae_cv <- mae(test$readingScore, predictions)

# Print the metrics
cat("SVM CV Mean Squared Error:", svm_mse_cv, "\n")
cat("SVM CV Root Mean Squared Error:", svm_rmse_cv, "\n")
cat("SVM CV Mean Absolute Error:", svm_mae_cv, "\n")




