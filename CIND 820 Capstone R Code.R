#CIND 820 Capstone Project - Code as of Literature Review stage

#Install some packages and load the libraries
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

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
data$Year <- as.numeric(data$Year)
data$Harv_area_ac <- data$Harv_area_ha*2.47105
data$Yield_per_ac <- round(data$Production_000bu / data$Harv_area_ac *1000,2)
data_summary_rows <- data[c(1:5),] #The first 4 rows of data are aggregate averages over different time spans, not required for this purpose, the 5th row has missing values
data_less_first_rows <- data[-c(1:5),]
data <- data_less_first_rows
str(data)

#This section of coding is mostly for future use - setting a train set and test set for linear regression modelling
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train.set <- data[train_index,]
test.set <- data[-train_index,]

#Setting up boxplot parameters
par(mfrow = c(1, 3)) 

#Boxplot for Harvested Area
boxplot(data$Harv_area_ac,
        main = "Boxplot of Harvested Area",
        ylab = "Harvested Area (acres)",
        col = "lightblue",
        outline = TRUE)

#Boxplot for Price per bu
boxplot(data$Price_per_bu,
        main = "Boxplot of Price per Unit (previous year)",
        ylab = "Price per Unit (bushel)",
        col = "lightgreen",
        outline = TRUE)

#Boxplot for Production
boxplot(data$Production_000bu,
        main = "Boxplot of Price per Unit (previous year)",
        ylab = "Price per Unit (bushel)",
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
multiple_lr_model <- lm(Harv_area_ac~Lagged_Price_per_bu+Production_000bu, data=data)
summary(multiple_lr_model)

#Create a plot with dual y-axes, in order to demonstrate the relative correlations
ggplot(data, aes(x = Lagged_Price_per_bu)) +
  geom_point(aes(y = Harv_area_ac, color = "Harvested Area"), size = 3) +
  geom_point(aes(y = Production_000bu, color = "Production"), size = 3) + 
  geom_abline(intercept = coef(simple_lr_model_ac)[1], slope = coef(simple_lr_model_ac)[2], color = "red") +
  geom_abline(intercept = coef(simple_lr_model_prod)[1], slope = coef(simple_lr_model_prod)[2], color = "blue") +
  scale_y_continuous(
    name = "Harvested Area (acres)",
    sec.axis = sec_axis(~./10, name = "Production ('000bu)"),
    limits = c(100000, 2500000),  
    expand = c(0, 0)  
  ) +
  labs(
    title = "Grain Corn Price vs. Harvested Area and Total Production",
    x = "Price per Unit ($/bushel, previous year)",
    color = "Variable"
  ) +
  theme_minimal()

#Use the simple LR models to predict the next year's production and planted area, using the known prices
#normalized_data<- as.data.frame(scale(data_less_first_rows)) #This step is required because we know that 2022 and 2023 prices are outliers
prediction_data <- rbind(data_summary_rows[5,],data_outliers_only[1,],data[1:2,])

prediction_data$predicted_harv_area <- predict(simple_lr_model_ac,prediction_data)
prediction_data$predicted_production <- predict(simple_lr_model_prod,prediction_data)
prediction_data$projection_variance_area <- prediction_data$predicted_harv_area/prediction_data$Harv_area_ac*100
prediction_data$projection_variance_prod <- prediction_data$predicted_production/prediction_data$Production_000bu*100
prediction_data <- prediction_data %>% mutate(Lagged_Harv_area_ac = lead(Harv_area_ac))
prediction_data <- prediction_data %>% mutate(Lagged_production = lead(Production_000bu))
prediction_data <- prediction_data[1:3,]

#Print the results
print("Predicted Harvested Area for 2023, 2022, and 2021, respectively (in acres):")
print(as.numeric(prediction_data$predicted_harv_area), quote = FALSE)
print("Variance from actual (as a percentage):")
print(as.numeric(prediction_data$projection_variance_area))

print("Predicted Production for 2023, 2022, and 2021, respectively (in '000bu):")
print(as.numeric(prediction_data$predicted_production), quote = FALSE)
print("Variance from actual (as a percentage):")
print(as.numeric(prediction_data$projection_variance_prod))

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
