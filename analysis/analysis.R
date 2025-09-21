library(dplyr)
library(tidyverse)
library(ggplot2)
water_use <- read.csv("../data/total_water_use_clean.csv")
ggplot(water_use, aes(WUS_URBAN, WUS_AGRICULTURAL)) +
  geom_point() +
  labs(title = "Agricultural Water Usage against Urban Water Usage", x = "Urban Water Use (acre-feet)", y = "Agricultural Water Use (acre-feet)")

water_model <- lm(WUS_AGRICULTURAL ~ WUS_URBAN, water_use)
summary(water_model)
b_0 = water_model$coefficients[1]
b_1 = water_model$coefficients[2]

results <-data.frame(
  X = water_use$WUS_URBAN,
  Y_observed = water_use$WUS_AGRICULTURAL,
  Y_predicted = b_0 + b_1*water_use$WUS_URBAN,
  year = water_use$REPORT_YEAR
)
results <- results %>%
  mutate(
    residuals = Y_observed - Y_predicted
  )

#Diagnostic Plots
ggplot(results, aes(X, residuals)) + 
  geom_point() + 
  labs(title = "Residuals against WUS_URBAN", x = "Predictor Variable", y = "Residuals")

ggplot(results, aes(X, abs(residuals))) + 
  geom_point() + 
  labs(title = "Absolute Value of Residuals against WUS_URBAN", x = "Predictor Variable", y = "Residuals")

#residuals against time
ggplot(results, aes(year, residuals)) + 
  geom_point() + 
  labs(title = "Residuals against Time", x = "Report Year", y="Residuals")

boxplot(results$residuals, horizontal = TRUE, ylab = "Residuals", main = "Box Plot of Residuals")

qqnorm(results$residuals)
qqline(results$residuals)
