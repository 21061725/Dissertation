# Load required libraries
library(tidyverse)
library(ggplot2)

# Load data from CSV files
nutrition_data <-Nutrient_Houshold_Filtered
expenditure_data <- Expenditure_NoAlcohol_InHouse
obesity_data <- obesity_diagnosis_england_clean

# Data preprocessing and merging

# Convert comma-separated numbers to numeric format
obesity_data$Total <- as.numeric(gsub(",", "", obesity_data$Total))
obesity_data$Male <- as.numeric(gsub(",", "", obesity_data$Male))
expenditure_data$ExpenditurePerYearPounds <- as.numeric(gsub(",", "", expenditure_data$ExpenditurePerYearPounds))

# Merge data using a full join to retain all rows from all datasets
merged_data <- full_join(nutrition_data, expenditure_data, by = "YearRange")
merged_data <- full_join(merged_data, obesity_data, by = "YearRange")

View(merged_data)
# Data analysis and visualization

# Summary statistics
summary(merged_data)

# Visualize trends over the years



# Calculate average intake for each year
average_intake <- nutrition_data %>%
  group_by(YearRange) %>%
  summarize(AverageIntake = mean(IntakeYear))

#Box plot of Average Nutrient Intake 
ggplot(average_intake, aes(x = YearRange, y = AverageIntake)) +
  geom_point(color = 'red', size = 3) +  # Customize color and size of dots
  labs(title = "Average Nutrient Intake Over the Years",
       x = "Year Range",
       y = "Average Intake") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# box plot of Expenditure over the years
ggplot(expenditure_data, aes(x = YearRange, y = ExpenditurePerYearPounds)) +
  geom_point(color = 'red', size = 3) +
  labs(title = "Expenditure Over the Years",
       x = "Year Range",
       y = "Expenditure (Pounds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar plot of Obesity cases over the years
ggplot(obesity_data, aes(x = YearRange, y = Total, fill = PrimaryDiagnosis)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Obesity Cases Over the Years",
       x = "Year Range",
       y = "Total Obesity Cases") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# Hypothesis testing

# Correlation between Expenditure and Obesity
correlation_result <- cor.test(merged_data$ExpenditurePerYearPounds, merged_data$Total)

print(correlation_result)

# Visualize correlation
ggplot(merged_data, aes(x = ExpenditurePerYearPounds, y = Total)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation of Expenditure and Obesity",
       x = "Expenditure (Pounds)",
       y = "Obesity Total") +
  theme_minimal()

# Print correlation test result
print(correlation_result)

#Correlation between Nutrition and Obesity
correlation_result_2 <- cor.test(merged_data$IntakeYear, merged_data$Total)

print(correlation_result_2)
#Visualise Correlation Nutrition and Obesity
ggplot(merged_data, aes(x = IntakeYear, y = Total)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation of Nutrition intake and Obesity",
       x = "Nutrition",
       y = "Obesity Total") +
  theme_minimal()

print(correlation_result_2)

correlation_result_3 <- cor.test(merged_data$IntakeYear, merged_data$ExpenditurePerYearPounds)
print(correlation_result_3)
# Visualize correlation
ggplot(merged_data, aes(x = IntakeYear, y = ExpenditurePerYearPounds)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation of Expenditure and Nutrition Intake",
       x = "Nutrition",
       y = "Expenditure (Pounds)") +
  theme_minimal()


# Summary statistics
summary(merged_data)

# Filter data for recession (2008/9) and COVID (2018/19) periods
recession_period <- c("2008/9", "2009/10")
covid_period <- "2018/19"

recession_obesity <- merged_data %>% 
  filter(YearRange %in% recession_period) %>%
  select(YearRange, Total) %>%
  summarise(mean_obesity_recession = mean(Total))

covid_obesity <- merged_data %>% 
  filter(YearRange == covid_period) %>%
  select(YearRange, Total) %>%
  summarise(mean_obesity_covid = mean(Total))


# Hypothesis test: Compare means of obesity during recession and COVID
recession_data <- merged_data %>% filter(YearRange %in% recession_period) 
covid_data <- merged_data %>% filter(YearRange == covid_period) 

hypothesis_test_result <- t.test(recession_data$Total, covid_data$Total)

# Print hypothesis test result
print(hypothesis_test_result)

# Linear regression model: Predict obesity based on expenditure
linear_model <- lm(Total ~ ExpenditurePerYearPounds, data = merged_data)

# Visualize the linear regression model
ggplot(merged_data, aes(x = ExpenditurePerYearPounds, y = Total)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Expenditure vs. Obesity with Linear Regression",
       x = "Expenditure (Pounds)",
       y = "Obesity Total") +
  theme_minimal()



# Print linear regression model summary
summary(linear_model)

# Extend YearRange up to 2025
extended_years <- c(unique(merged_data$YearRange), "2020/21", "2021/22", "2022/23", "2023/24", "2024/25")

# Convert YearRange to numeric values
year_numeric <- as.numeric(substring(prediction_data$YearRange, 1, 4))

# Calculate predicted obesity values using linear model coefficients
predicted_obesity <- intercept + slope * year_numeric

# Create a new data frame for predicted values
predicted_data <- data.frame(YearRange = prediction_data$YearRange, PredictedObesity = predicted_obesity)

# Plot the linear regression model with predicted values
ggplot() +
  geom_line(data = predicted_data, aes(x = year_numeric, y = PredictedObesity), color = "red") +
  labs(title = "Predicted Obesity based on Expenditure",
       x = "Expenditure (Pounds)",
       y = "Predicted Obesity") +
  theme_minimal()




# Extract coefficients from linear model
intercept <- coef(linear_model)[1]
slope <- coef(linear_model)[2]

# Convert YearRange to numeric values
year_numeric <- as.numeric(substring(prediction_data$YearRange, 1, 4))

# Calculate predicted obesity values using linear model coefficients
prediction_data$PredictedObesity <- intercept + slope * year_numeric

# Create a new data frame for predicted values
predicted_data <- data.frame(YearRange = prediction_data$YearRange, PredictedObesity = predicted_obesity)

# Plot the linear regression model with predicted values
ggplot() +
  geom_point(data = merged_data, aes(x = ExpenditurePerYearPounds, y = Total)) +
  geom_smooth(data = merged_data, method = "lm", aes(x = ExpenditurePerYearPounds, y = Total), color = "blue", se = FALSE) +
  geom_line(data = predicted_data, aes(x = YearRange, y = PredictedObesity), color = "red") +
  labs(title = "Expenditure vs. Obesity with Linear Regression and Predictions",
       x = "Expenditure (Pounds)",
       y = "Obesity Total") +
  theme_minimal()

# Create a new data frame for predicted expenditure and obesity
predicted_data <- data.frame(YearRange = prediction_data$YearRange,
                             PredictedExpenditure = predicted_expenditure,
                             PredictedObesity = predicted_obesity)

# Plot predicted expenditure over years
ggplot(predicted_data, aes(x = as.numeric(substring(YearRange, 1, 4)), y = PredictedExpenditure)) +
  geom_line(color = "blue") +
  labs(title = "Predicted Expenditure Over Years",
       x = "Year",
       y = "Predicted Expenditure (Pounds)") +
  theme_minimal()

# Plot predicted obesity over years
ggplot(predicted_data, aes(x = as.numeric(substring(YearRange, 1, 4)), y = PredictedObesity)) +
  geom_line(color = "red") +
  labs(title = "Predicted Obesity Over Years",
       x = "Year",
       y = "Predicted Obesity") +
  theme_minimal()





