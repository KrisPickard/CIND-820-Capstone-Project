#CIND 820 Capstone Project - Code as of Literature Review stage

#Install some packages and load the libraries
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("class")
library(class)
install.packages("caret")
library(caret)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

#Set the correct working directory, and read the data file obtained from the public database
setwd("D:/Kris/Documents/Ryerson/CIND 820")
getwd()
full_data<-read.csv("D:/Kris/Documents/Ryerson/CIND 820/Potential Datasets/estimate_hist_metric.csv")
data<-full_data

#Some basic data exploration
head(data)
summary(data)
str(data)

#Cleaning the data
print(colnames(data))
colnames(data)<-c("Year","Harv_area_ha","Production_000bu","Yield_per_ha","Price_per_bu","Total_value_000dollars")
print(colnames(data))

#Since the goal is to predict the CURRENT YEAR'S planted acres and yield based on LAST YEARS' price, create a new column which is just the previous observation of the value for Price
data <- data %>% mutate(Lagged_Price_per_bu = lead(Price_per_bu))
data$Harv_area_ac <- data$Harv_area_ha*2.47105
data$Yield_per_ac <- round(data$Production_000bu / data$Harv_area_ac *1000,2)
data <- data %>% mutate(Lagged_Yield_per_ac = lead(Yield_per_ac))
data <- data %>% mutate(Lagged_Production_000bu = lead(Production_000bu))
data <- data %>% mutate(Lagged_Harv_area_ac = lead(Harv_area_ac))
data$Year <- as.numeric(data$Year)
data_summary_rows <- data[c(1:5),] #The first 4 rows of data are aggregate averages over different time spans, not required for this purpose, the 5th row has missing values
data_less_first_rows <- data[-c(1:5),]
data <- data_less_first_rows
str(data)
data_less_last_row <- data[-c(42),] #Since we care about the last year's value to make the next year's prediction, removing the last row, since it now has some NA values
data <- data_less_last_row

#Now that the data is cleaned, splitting the data into a train set and test set for modelling
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_set <- data[train_index,]
test_set <- data[-train_index,]

#Setting up boxplot parameters
par(mfrow = c(1, 3)) 

#Boxplot for Harvested Area
boxplot(data$Harv_area_ac,
        main = "Boxplot of Harvested Area",
        ylab = "Harvested Area (ac)",
        col = "lightblue",
        outline = TRUE)

#Boxplot for Price per bu
boxplot(data$Price_per_bu,
        main = "Boxplot of Price per Unit",
        ylab = "Price per Unit (bu)",
        col = "lightgreen",
        outline = TRUE)

#Boxplot for Production
boxplot(data$Production_000bu,
        main = "Boxplot of Production",
        ylab = "Production ('000bu)",
        col = "red",
        outline = TRUE)

#Reset the plotting area
par(mfrow = c(1, 1))

#Identify outliers for each variable of interest
outliers_harvested_area <- boxplot.stats(data$Harv_area_ac)$out
outliers_price_per_bu <- boxplot.stats(data$Price_per_bu)$out
outliers_production <- boxplot.stats(data$Production_000bu)$out

#Print outliers for each variable
print("Outliers in Harvested Area (acres):")
print(outliers_harvested_area)

print("Outliers in Price per Unit (bushels, previous year):")
print(outliers_price_per_bu)

print("Outliers in Production:")
print(outliers_production)

#Remove outliers from the data and re-plot
data_outliers_only <- data[1:2,]
data_outliers_removed <- data[!(data$Lagged_Price_per_bu %in% outliers_price_per_bu), ]
data <- data_outliers_removed

#Overall data representation
agg_plot <- plot(data)

#Visualization of the correlation between Harvested Area and Price using a plot
refined_plot <- plot(data$Lagged_Price_per_bu, data$Harv_area_ac,
     main = "Grain Corn Price vs Harvested Area",
     ylab = "Harvested Area (acres)",
     xlab = "Price per Unit ($/bushel)",
     pch = 19, col = "blue")

#Modelling 1:Simple Linear Regression - Price as a predictor of Harvested Acres
simple_lr_model_ac <- lm(Harv_area_ac ~ Lagged_Price_per_bu, data = data)
abline(simple_lr_model_ac, col = "red", lwd = 2)
summary(simple_lr_model_ac)

#Visualization of the correlation between Production and Price using a plot
refined_plot <- plot(data$Lagged_Price_per_bu, data$Production_000bu,
                   main = "Grain Corn Price vs Total Production",
                   ylab = "Total Production ('000bu)",
                   xlab = "Price per Unit ($/bushel)",
                   pch = 19, col = "blue")

#Modelling 1:Simple Linear Regression - Price as a predictor of Total Production
simple_lr_model_prod <- lm(Production_000bu ~ Lagged_Price_per_bu, data = data)
abline(simple_lr_model_prod, col = "green", lwd = 2)
summary(simple_lr_model_prod)

#Modelling 2. Multiple Linear Regression
multiple_lr_model_ac <- lm(Harv_area_ac~Lagged_Price_per_bu+Lagged_Production_000bu+Lagged_Yield_per_ac, data=data)
summary(multiple_lr_model_ac)

multiple_lr_model_prod <- lm(Production_000bu~Lagged_Price_per_bu+Lagged_Harv_area_ac+Lagged_Yield_per_ac, data=data)
summary(multiple_lr_model_prod) #This one is not a good comparison - of course the overall production would be a good predictor of the yield per acre

#~~~ Additional models - KNN regression for example, or regression trees, random forest - need 3 in total - have linear regression already which counts as 1
#~~~ Design - run the models, then compare via R^2 etc.
#~~~ Consider cross-validation on the training set, 10fold cross-validation for example (on the training set), and report the cross-validation results on the training set, final results on the test set
#~~~ The results include the p-value, LR vs. decision trees vs. KNN - Evaluation measures - RMSE (root-mean-square-error) - AIC - R^2 - use at least one for this submission

#Create a plot with dual y-axes, in order to demonstrate the relative correlations
ggplot(data, aes(x = Lagged_Price_per_bu)) +
  geom_point(aes(y = Harv_area_ac, color = "Harvested Area"), size = 3) +
  geom_point(aes(y = Production_000bu*10, color = "Production"), size = 3) + 
  geom_abline(intercept = coef(simple_lr_model_ac)[1], slope = coef(simple_lr_model_ac)[2], color = "red") +
  geom_abline(intercept = coef(simple_lr_model_prod)[1]*10, slope = coef(simple_lr_model_prod)[2]*10, color = "blue") +
  scale_y_continuous(
    name = "Harvested Area (acres)",
    sec.axis = sec_axis(~./10, name = "Production ('000bu)"),
    limits = c(150000, 4500000),  
    expand = c(0, 0)  
  ) +
  labs(
    title = "Grain Corn Price vs. Harvested Area and Total Production",
    x = "Price per Unit ($/bushel, previous year)",
    color = "Variable"
  ) +
  theme_minimal()

#Use the simple LR models to predict the next year's production and planted area, using the known prices
prediction_data <- rbind(data_summary_rows[5,],data_outliers_only[1,],data[1:2,])
prediction_data$predicted_harv_area <- predict(simple_lr_model_ac,prediction_data)
prediction_data$predicted_production <- predict(simple_lr_model_prod,prediction_data)
prediction_data$predicted_multi_LR <- predict(multiple_lr_model_ac,prediction_data)
prediction_data$projection_variance_area <- prediction_data$predicted_harv_area/prediction_data$Harv_area_ac*100
prediction_data$projection_variance_prod <- prediction_data$predicted_production/prediction_data$Production_000bu*100
prediction_data$projection_variance_multi <- prediction_data$predicted_multi_LR/prediction_data$Harv_area_ac*100
residuals_area <- prediction_data$Harv_area_ac - prediction_data$predicted_harv_area
residuals_prod <- prediction_data$Production_000bu - prediction_data$predicted_production
residuals_multi <- prediction_data$Harv_area_ac - prediction_data$predicted_multi_LR
rmse_area <- sqrt(mean(residuals_area^2))
rmse_prod <- sqrt(mean(residuals_prod^2))
rmse_multi <- sqrt(mean(residuals_multi^2))
prediction_data <- prediction_data %>% mutate(Lagged_Harv_area_ac = lead(Harv_area_ac))
prediction_data <- prediction_data %>% mutate(Lagged_production = lead(Production_000bu))
prediction_data <- prediction_data[1:3,]

#Print the results
print("Predicted Harvested Area for 2023, 2022, and 2021, respectively (in acres):")
print(as.numeric(prediction_data$predicted_harv_area), quote = FALSE)
print("Variance from actual (as a percentage):")
print(as.numeric(prediction_data$projection_variance_area))
print("Root Mean Squared Error:")
print(rmse_area)

print("Predicted Production for 2023, 2022, and 2021, respectively (in '000bu):")
print(as.numeric(prediction_data$predicted_production), quote = FALSE)
print("Variance from actual (as a percentage):")
print(as.numeric(prediction_data$projection_variance_prod))
print("Root Mean Squared Error:")
print(rmse_prod)

print("Predicted Harvested Area by multi-LR model for 2023, 2022, and 2021, respectively (in acres):")
print(as.numeric(prediction_data$predicted_multi_LR), quote = FALSE)
print("Variance from actual (as a percentage):")
print(as.numeric(prediction_data$projection_variance_multi))
print("Root Mean Squared Error:")
print(rmse_multi)

#Since we know that both the 2023 and 2022 prices are outliers, let's re-examine, by using the simple LR models, predict the prices based on known planting area
#This will have the effect to reinforce the correlation coefficients and may increase the accuracy of the predictions
#In order to accomplish this, we use the coefficients calculated by the LR models for both area and production, and average the resulting price
# y = a + bx
# x = (y - a)/b
prediction_data$price_prediction <- round((((prediction_data$Lagged_Harv_area_ac - 1474022)/104878)+((prediction_data$Lagged_production - 57928)/48442))/2,2)

#Now overwrite the original price values (outliers) with the prices predicted by the LR model, and then re-run the predicted area and production using non-outlier prices
prediction_data$Price_per_bu <- prediction_data$price_prediction

prediction_data$predicted_harv_area_revised <- predict(simple_lr_model_ac,prediction_data)
prediction_data$predicted_production_revised <- predict(simple_lr_model_prod,prediction_data)
#~~~These transmutations did not work... why not?

#Setting up for KNN modelling
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

train_data_norm <- as.data.frame(lapply(train_set[, -8], normalize))
test_data_norm <- as.data.frame(lapply(test_set[, -8], normalize))
train_labels <- train_set$Harv_area_ac
test_labels <- test_set$Harv_area_ac

k <- 1 
knn_model1 <- knnreg(x = train_data_norm, y = train_labels, k = k)
knn_pred1 <- predict(knn_model1, test_data_norm)
summary(knn_pred)

#Assess the model
rmse_knn1 <- sqrt(mean((knn_pred1 - test_labels)^2))
print(paste("Root Mean Squared Error:", rmse_knn))

k <- 2 
knn_model2 <- knnreg(x = train_data_norm, y = train_labels, k = k)
knn_pred2 <- predict(knn_model2, test_data_norm)
summary(knn_pred)

#Assess the model
rmse_knn2 <- sqrt(mean((knn_pred2 - test_labels)^2))
print(paste("Root Mean Squared Error:", rmse_knn))

k <- 3 
knn_model3 <- knnreg(x = train_data_norm, y = train_labels, k = k)
knn_pred3 <- predict(knn_model3, test_data_norm)
summary(knn_pred)

#Assess the model
rmse_knn3 <- sqrt(mean((knn_pred3 - test_labels)^2))
print(paste("Root Mean Squared Error:", rmse_knn))

k <- 4 
knn_model4 <- knnreg(x = train_data_norm, y = train_labels, k = k)
knn_pred4 <- predict(knn_model4, test_data_norm)
summary(knn_pred)

#Assess the model
rmse_knn4 <- sqrt(mean((knn_pred4 - test_labels)^2))
print(paste("Root Mean Squared Error:", rmse_knn))

k <- 5 
knn_model5 <- knnreg(x = train_data_norm, y = train_labels, k = k)
knn_pred5 <- predict(knn_model5, test_data_norm)
summary(knn_pred)

#Assess the model
rmse_knn5 <- sqrt(mean((knn_pred5 - test_labels)^2))
print(paste("Root Mean Squared Error:", rmse_knn))

#Elbow method to select best k value
rmse_values <- c(rmse_knn1, rmse_knn2, rmse_knn3, rmse_knn4, rmse_knn5)
k_values <- 1:5
plot_knn_elbow <- data.frame(k = k_values, RMSE = rmse_values)
ggplot(plot_knn_elbow, aes(x = k, y = rmse_values)) +
  geom_line(color = "blue") +
  labs(title = "KNN Regression Results", x = "Actual Harv Area (ac)", y = "Predicted Harv Area (ac)")+
  theme_minimal()

#k = 3 gives the best results
k <- 3
knn_model <- knnreg(x = train_data_norm, y = train_labels, k = k)
knn_pred <- predict(knn_model, test_data_norm)
summary(knn_pred)

#Visualize the results
plot_knn <- data.frame(Actual = test_labels, Predicted = knn_pred)
ggplot(plot_knn, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  labs(title = "KNN Regression Results", x = "Actual Harv Area (ac)", y = "Predicted Harv Area (ac)")+
  theme_minimal()

#Regression Tree Analysis
control_params <- rpart.control(cp = 0.01, minsplit = 5)
tree_area <- rpart(Harv_area_ac ~ Lagged_Price_per_bu+Lagged_Production_000bu+Lagged_Yield_per_ac, data = train_set, method = "anova", control = control_params)
rpart.plot(tree_area)

predictions_tree <- predict(tree_area, test_set)
rmse_tree <- sqrt(mean(predictions_tree - test_set$Harv_area_ac)^2)
summary(tree_area)
print("Root Mean Squared Error of Tree Regression:")
print(rmse_tree)

#Some observations and conclusions from the data:
#The slope of the LR model line for production is greater than that of area. This could indicate that there are other, external factors that also influence the relationship
  #An example could be the increasingly powerful practices employed by farmers to produce more crop on the same amount of land, leading to greater production per acre overall
  #Indeed, this trend is noted in the data, the average yield per acre increases over time
ggplot(data, aes(x = Year, y = Yield_per_ac)) +
  geom_point(color = "blue") +   # Add points
  geom_line(color = "blue") +    # Add lines
  labs(
    title = "Yield per Acre over Time",
    x = "Year",
    y = "Yield per Acre (bu/ac)"
  ) +
  theme_minimal()
#Since the coefficient of the line of best fit is larger for production, we would expect the variance to be wider when there is a large value outlier
  #Since the coefficient for area is lower, the variance is less when predicting based on large value outliers

#Overall Assessment
assessment_data <- data.frame(matrix(ncol=2, nrow=5))
colnames(assessment_data) <- c("Model_Name","RMSE")
assessment_data$RMSE <- c(rmse_area, rmse_prod, rmse_multi, rmse_knn, rmse_tree)
assessment_data$Model_Name <- c("Single LR - Area", "Single LR - Prod", "Multiple LR", "KNN (k=3)", "Regression Tree")
#Based on the RMSE comparison, the regression tree has the greatest predictability out of all of the models tested

