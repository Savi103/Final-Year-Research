# Load the required packages
library(caret)
library(readxl)
library(ggplot2)
library(dplyr)
library(randomForest)
library(factoextra)

# Prepare the data
# Assuming you have a data frame called 'data' with the NPI values and corresponding labels
ODI_Player <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/ODI Player.xlsx")

ODI = subset(ODI_Player,ODI_Player$SC > 0.2)

#Clustering part

# Create some sample data
data <- data.frame(NPI = ODI_Player$`Final NPI`, SC = ODI_Player$`Final SC`)
  
  # Perform K-means clustering
  kmeans_result <- kmeans(data, centers = 3)

# Get the cluster labels for each data point
labels <- kmeans_result$cluster

# Get the centroids of the clusters
centroids <- kmeans_result$centers

# Print the cluster labels and centroids
print("Cluster Labels:")
print(labels)
print("Cluster Centroids:")
print(centroids)

# Plot the clusters
fviz_cluster(kmeans_result, data = data, geom = "point", frame.type = "norm")

# Plot the data points with cluster assignments
plot(data, col = labels, pch = 19, main = "K-means Clustering")





#Classification part

# Example dataframe
df <- data.frame(Classification = ODI$Classification, NPI = ODI$`Final NPI`, SC = ODI$`Final SC`, BU = ODI$`Final BU`)
df$Classification <- as.factor(df$Classification)

# Load and prepare the data
response_var <- df$Classification
predictor_vars <- df[, -ncol(df)]  # Exclude the response variable from predictors

# Split the data into training and testing sets
train_index <- sample(1:nrow(df), nrow(df) * 0.7)  # 70% for training
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Fit the Random Forest model
model <- randomForest(train_data$Classification ~ ., data = train_data, ntree = 100)
str(train_data)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)


# Calculate accuracy
accuracy <- sum(predictions == test_data$Classification) / nrow(test_data)

# Print the accuracy
print(accuracy)

new_data <- data.frame(NPI = 2.00, SC = 0.5, BU = 0.2)  # Replace 'var2' and 'var3' with actual variable names

# Make predictions using the random forest model
predictions <- predict(model, newdata = new_data)

# Print the predictions
print(predictions)

