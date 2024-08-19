# Install and load necessary packages
install.packages("tidyverse")
library(tidyverse)
install.packages('dplyr')
library(dplyr)
install.packages('ggplot2')
library(ggplot2)

# Read the salary data
salary_data <- read.csv("C:/Users/abbin/Desktop/data.csv")

#Summary of salary data
summary(salary_data)
head(salary_data)

correlation <- cor(salary_data$hourly_rate, salary_data$regular_pay)
correlation


model <- lm(regular_pay ~ hourly_rate + overtime_pay, data = salary_data)
summary(model)

ggplot(salary_data, aes(x = hourly_rate, y = regular_pay)) +
  geom_point() +
  labs(x = "Hourly Rate", y = "Regular Pay")

#to Evaluate the fairness and equity of salary distribution

# Descriptive statistics
summary(salary_data$regular_pay) # Summary statistics of regular pay

summary(salary_data$regular_pay[salary_data$gender == "male"])
summary(salary_data$regular_pay[salary_data$gender == "female"])

average_salary <- salary_data %>%
  group_by(gender) %>%
  summarize(avg_salary = mean(regular_pay))

# Create a bar graph to compare average salary by gender
ggplot(average_salary, aes(x = gender, y = avg_salary, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Average Salary", title = "Average Salary by Gender") +
  theme_bw()

# t-test for salary disparities based on gender
t_test <- t.test(regular_pay ~ gender, data = salary_data)
t_test


range(salary_data$regular_pay)   # Range of regular pay

# Detect and analyze outliers
outliers <- boxplot.stats(salary_data$regular_pay)$out
outliers

# Distribution of salaries
ggplot(salary_data, aes(x = regular_pay)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(x = "Regular Pay", y = "Frequency", title = "Distribution of Salaries")

# Boxplot of salaries by department
ggplot(salary_data, aes(x = department, y = regular_pay)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(x = "Department", y = "Regular Pay", title = "Salary Distribution by Department")

#  computing average compensation by job_title and full_part_time.
compensation_summary <- employee_data %>%
  group_by(job_title, full_part_time) %>%
  summarise(avg_compensation = mean(regular_pay + overtime_pay + other_pay))

# Print the summary
print(compensation_summary)

# Generate the compensation report
write.csv(compensation_summary, "compensation_report.csv", row.names = FALSE)


