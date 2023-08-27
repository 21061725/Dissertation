library(dplyr)
library(ggplot2)
library(lmtest)

nutrition_df <- Nutrition_HouseHold_Whole
obesity_df <- obesity_diagnosis_england_clean
expenditure_df <- ExpenditurePerHouseHold


# Combine the data frames based on YearRange
combined_data <- inner_join(expenditure_df, nutrition_df, by = "YearRange", suffix = c("_expenditure", "_nutrition"))
combined_data <- inner_join(combined_data, obesity_df, by = "YearRange", suffix = c("_combined", "_obesity"), relationship = "many-to-many")

View(combined_data)

# Convert YearRange to a numeric variable for plotting
combined_data$Year <- as.numeric(gsub("/", ".", combined_data$YearRange))

# Correlation Analysis
correlation_nutrition_obesity <- cor(combined_data$Count, combined_data$Total)
correlation_expenditure_obesity <- cor(combined_data$ExpenditurePerYearPounds, combined_data$Total)

print(correlation_expenditure_obesity)
print(correlation_nutrition_obesity)

print('There is no correlation between expenditure and obesity and nutrition and obesity')

# Linear Regression Analysis
nutrition_obesity_lm <- lm(Total ~ Count, data = combined_data)
expenditure_obesity_lm <- lm(Total ~ ExpenditurePerYearPounds, data = combined_data)

print(nutrition_obesity_lm)
print(expenditure_obesity_lm)

#as nutrition decreases, obesity decreases as expenditure increases, obesity increases. 
# Visualization
ggplot(combined_data, aes(x = Count, y = Total)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Obesity vs. Nutrition Intake",
       x = "Nutrition Intake",
       y = "Obesity Rates") +
  theme_minimal()

ggplot(combined_data, aes(x = ExpenditurePerYearPounds, y = Total)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Obesity vs. Expenditure",
       x = "Expenditure Per Year (£)",
       y = "Obesity Rates") +
  theme_minimal()

#Shows slight correlation between expentture and obesity rates. 

# Hypothesis Testing (T-Tests) - Years of Recession and COVID-19
recession_years <- c("2008/9", "2009/10")
covid_years <- c("2017/18", "2018/19")

recession_data <- combined_data %>% filter(YearRange %in% recession_years)
covid_data <- combined_data %>% filter(YearRange %in% covid_years)

t_test_recession_nutrition <- t.test(recession_data$Count, recession_data$Total)
t_test_covid_nutrition <- t.test(covid_data$Count, covid_data$Total)

print(t_test_recession_nutrition)
print(t_test_covid_nutrition)

t_test_recession_expenditure <- t.test(recession_data$ExpenditurePerYearPounds, recession_data$Total)
t_test_covid_expenditure <- t.test(covid_data$ExpenditurePerYearPounds, covid_data$Total)

print(t_test_covid_expenditure)
print(t_test_recession_expenditure)

#We can reject the null hypothesis


# Predictive Analysis for COVID-19 (Using Linear Regression)
pre_covid_data <- combined_data %>% filter(!(YearRange %in% covid_years))

nutrition_pred_lm <- lm(Total ~ Count, data = pre_covid_data)
expenditure_pred_lm <- lm(Total ~ ExpenditurePerYearPounds, data = pre_covid_data)

new_data <- data.frame(
  Count = predict(nutrition_pred_lm, newdata = covid_data),
  ExpenditurePerYearPounds = predict(expenditure_pred_lm, newdata = covid_data)
)

# Visualization Prediction
ggplot(covid_data, aes(x = Count, y = Total)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Predicted Obesity Rates vs. Nutrition Intake (COVID-19 Years)",
       x = "Predicted Nutrition Intake",
       y = "Obesity Rates") +
  theme_minimal()

ggplot(covid_data, aes(x = ExpenditurePerYearPounds, y = Total)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Predicted Obesity Rates vs. Expenditure (COVID-19 Years)",
       x = "Predicted Expenditure Per Year (£)",
       y = "Obesity Rates") +
  theme_minimal()


# Convert YearRange to a numeric variable for plotting
combined_data$Year <- as.numeric(gsub("/", ".", combined_data$YearRange))

# Perform linear regression to predict obesity rates for future years
obesity_pred_lm <- lm(Total ~ Year, data = combined_data)

# Generate a sequence of years for prediction (e.g., up to 2025)
future_years <- seq(from = max(combined_data$Year), to = 2025, by = 1)
future_data <- data.frame(Year = future_years, Total = predict(obesity_pred_lm, newdata = data.frame(Year = future_years)))

# Visualization of Predicted Obesity Rates
ggplot(future_data, aes(x = Year, y = Total)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Predicted Obesity Rates up to 2025",
       x = "Year",
       y = "Predicted Obesity Rates") +
  theme_minimal()


# Perform linear regression to predict obesity rates for future years
obesity_pred_lm <- lm(Total ~ Year, data = combined_data)

# Generate a sequence of years for prediction (e.g., up to 2025)
future_years <- seq(from = max(combined_data$Year), to = 2025, by = 1)
future_data <- data.frame(Year = future_years,
                          All = predict(obesity_pred_lm, newdata = data.frame(Year = future_years)))

# Create linear regression models for expenditure and nutrition
expenditure_pred_lm <- lm(All ~ ExpenditurePerYearPounds, data = combined_data)
nutrition_pred_lm <- lm(All ~ Count, data = combined_data)

