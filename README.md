# 📦 Load necessary libraries
install.packages(c("dplyr", "ggplot2", "randomForest"))
library(dplyr)
library(ggplot2)
library(randomForest)

# 📥 Step 1: Load the dataset
library(readr)
aq_data <- read.csv("city_day.csv")

# 📊 Step 2: Select relevant columns and clean data
aq_data_clean <- na.omit(aq_data[, c("PM2.5", "PM10", "NO2", "SO2", "O3", "AQI_Bucket")])
aq_data_clean$AQI_Bucket <- as.factor(aq_data_clean$AQI_Bucket)

table(aq_data_clean$AQI_Bucket)  # Check class counts

# Remove rare classes (<10 samples)
aq_data_clean <- aq_data_clean %>%
  group_by(AQI_Bucket) %>%
  filter(n() > 10) %>%
  ungroup()

# 🧹 Step 3: Ensure AQI_Bucket is a categorical variable
#aq_data_clean$AQI_Bucket <- as.factor(aq_data_clean$AQI_Bucket)
#sum(is.na(aq_data_clean))

aq_data_clean <- aq_data_clean %>%
  filter(!is.na(AQI_Bucket) & AQI_Bucket != "")
aq_data_clean$AQI_Bucket <- droplevels(as.factor(aq_data_clean$AQI_Bucket))


# 📈 Step 4: Split data into training and testing (80/20)
set.seed(123)
train_index <- sample(1:nrow(aq_data_clean), 0.8 * nrow(aq_data_clean))
train_data <- aq_data_clean[train_index, ]
test_data  <- aq_data_clean[-train_index, ]

# ⚠️ Ensure both train & test have same levels
train_data$AQI_Bucket <- droplevels(train_data$AQI_Bucket)
test_data$AQI_Bucket <- factor(test_data$AQI_Bucket, levels = levels(train_data$AQI_Bucket))



# 🌲 Step 5: Train the Random Forest Model
library(randomForest)
rf_model <- randomForest(
  AQI_Bucket ~ PM2.5 + PM10 + NO2 + SO2 + O3,
  data = train_data,
  ntree = 200,
  mtry = 3,
  importance = TRUE
)




# 🧾 Step 6: Print model summary
print(rf_model)

# 📊 Step 7: Predict on test data
pred <- predict(rf_model, test_data)

# 🧮 Step 8: Calculate accuracy
accuracy <- sum(pred == test_data$AQI_Bucket) / nrow(test_data) * 100
cat("Accuracy:", round(accuracy, 2), "%\n")

# 🔍 Step 9: Show confusion matrix
table(Predicted = pred, Actual = test_data$AQI_Bucket)

# 🌟 Step 10: Feature importance visualization
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance in AQI Prediction",
    x = "Pollutant",
    y = "Importance (Mean Decrease in Gini)"
  )
