# Load necessary libraries
library(tidyverse) # For data manipulation and visualization
library(readxl)    # For reading Excel files

# Clear workspace
rm(list = ls())

# Read Excel files into data frames
Data1 <- read_excel("C:/Users/1/OneDrive/Desktop/Module 2/Dataset2.xls")
Data2 <- read_excel("C:/Users/1/OneDrive/Desktop/Module 2/TrainExer25.xls")
Data3 <- read_excel("C:/Users/1/OneDrive/Desktop/Module 2/TestExer2-GPA-round2.xls")

# View the contents of the data frames
View(Data1)
View(Data2)
View(Data3)

# Print column names
names(Data1)
names(Data2)
names(Data3)

# Print structure of Data3
str(Data3)

# Exploratory Data Analysis

# Create a pairs plot to visualize relationships between variables in Data1
pairs(Data1[, 2:ncol(Data1)], pch = 21, bg = c("red"))

# Linear Regression Models

# Model 1: Predict LogWage based on Female variable in Data1
Model1 <- lm(LogWage ~ Female, Data1)
summary(Model1)

# Model 2: Exploring relationship between residuals of Model1 and Educ variable in Data1
Model2 <- lm(Model1$residuals ~ Educ, Data1)
summary(Model2)

# Model 3: Exploring relationship between residuals of Model1 and Parttime variable in Data1
Model3 <- lm(Model1$residuals ~ Parttime, Data1)
summary(Model3)

# Model 4: Predict LogWage based on Female, Age, Educ, and Parttime variables in Data2
Model4 <- lm(LogWage ~ Female + Age + Educ + Parttime, Data2)
summary(Model4)

# Model Diagnostics

# Create a quantile-quantile plot to check the normality assumption of Model4 residuals
qqnorm(Model4$residuals, main = 'Normal Probability Plot of Residuals')
qqline(Model4$residuals)

# Data Visualization

# Create a scatter plot of Age vs. LogWage in Data1, faceted by Female variable
Data1 %>% 
  ggplot(aes(Age, LogWage)) +
  geom_point(alpha = 0.5,
             aes(size = Educ, colour = Parttime)) +
  geom_smooth(method = lm) +
  facet_wrap(~Female) +
  theme_bw() +
  labs(title = 'Wage distribution')

# Further Regression Analysis

# Model 5: Exploring relationship between residuals of Model4 and DE2, DE3, DE4 variables in Data2
Model5 <- lm(Model4$residuals ~ DE2 + DE3 + DE4, Data2)
summary(Model5)

# Model 6: Predict FGPA based on SATV variable in Data3
Model6 <- lm(FGPA ~ SATV, Data3)
summary(Model6)

# Compute confidence intervals for Model6 regression coefficients
conf_intervals1 <- confint(Model6)
print(conf_intervals1)

# Histograms and Correlation

# Create histograms to visualize the distributions of SATV, SATM, and FGPA variables in Data3
par(mfrow = c(3, 1))
hist(Data3$SATV, main = 'Distribution of SATV')
hist(Data3$SATM, main = 'Distribution of SATM')
hist(Data3$FGPA, main = 'Distribution of SATV')

# Create a pairs plot to visualize relationships between variables in Data3
pairs(Data3[, 2:ncol(Data3)], pch = 21, bg = c("red"))

# Model 7: Predict FGPA based on SATV, SATM, and FEM variables in Data3
Model7 <- lm(FGPA ~ SATV + SATM + FEM, Data3)
summary(Model7)

# Create a quantile-quantile plot to check the normality assumption of Model7 residuals
par(mfrow = c(1, 1))
qqnorm(Model7$residuals, main = 'Normal Probability Plot of Residuals')
qqline(Model7$residuals)

# Plot actual FGPA values against fitted values from Model7
Data3$Fitval <- Model7$fitted.values
plot(Data3$FGPA, Data3$Fitval, main = 'Actual V.s Fitted Values')

# Compute confidence intervals for Model7 regression coefficients
conf_intervals2 <- confint(Model7)
print(conf_intervals2)

# Calculate correlation matrix for variables in Data3
cor(Data3[, 2:ncol(Data3)])

# Model 8: Predict FGPA based on SATM and FEM variables in Data3
Model8 <- lm(FGPA ~ SATM + FEM, Data3)
summary(Model8)
