library(dplyr)
library(ggplot2)
library(lmtest)

nutrition_df <- nutrition_england_clean
obesity_df <- obesity_diagnosis_england_clean
expenditure_df <- expenditure_england_clean


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

# Scatter plot for Expenditure vs. Obesity
ggplot(combined_data, aes(x = ExpenditurePerYearPounds, y = Total)) +
  geom_point() +
  labs(title = "Expenditure vs. Obesity",
       x = "Expenditure (Pounds)",
       y = "Obesity Cases")
# Scatter plot for Nutritional Factors vs. Obesity
ggplot(combined_data, aes(x = Count, y = Total)) +
  geom_point() +
  labs(title = "Nutritional Factors vs. Obesity",
       x = "Nutritional Factors",
       y = "Obesity Cases")




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







