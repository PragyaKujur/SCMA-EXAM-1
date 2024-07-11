# Load necessary libraries
library(readr)
library(dplyr)
library(caret)

# Load the dataset
cancer_data <- read_csv("C:\\Users\\Home\\Downloads\\cancer_reg.csv", locale = locale(encoding = "ISO-8859-1")) 

# Handle missing values by removing columns with significant missing values and imputing others
cancer_data <- cancer_data %>% select(-PctSomeCol18_24, -PctPrivateCoverageAlone)

# Impute missing values
preProcess_missingdata_model <- preProcess(cancer_data, method='medianImpute')
cancer_data <- predict(preProcess_missingdata_model, newdata = cancer_data)

# Convert categorical variables to numeric (if any)
cancer_data <- cancer_data %>% mutate(binnedInc = as.numeric(as.factor(binnedInc)),
                                      Geography = as.numeric(as.factor(Geography)))

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(cancer_data$TARGET_deathRate, p = .8, list = FALSE, times = 1)
cancerTrain <- cancer_data[trainIndex,]
cancerTest  <- cancer_data[-trainIndex,]

# Build the multivariate OLS regression model
model <- lm(TARGET_deathRate ~ ., data = cancerTrain)

# Summarize the model
summary(model)

# Make predictions on the test set
predictions <- predict(model, cancerTest)

# Evaluate the model
r_squared <- summary(model)$r.squared
rmse <- sqrt(mean((predictions - cancerTest$TARGET_deathRate)^2))

# Print R-squared and RMSE
print(paste("R-squared: ", r_squared))
print(paste("RMSE: ", rmse))

