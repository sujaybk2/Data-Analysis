# University of the Pacific
# Professor Julia Olivieri
# Davis Zhang and Sujay Bhaskar Kashyap

#From Homework 2
# Introduction
# Risk Factors for Cardiovascular Heart Disease Data Analysis
#Link: https://www.kaggle.com/datasets/thedevastator/exploring-risk-factors-for-cardiovascular-diseas?resource=download 
#Each row (observation) represents the data (information) of each sample individual

#Variables
# Age: Age of the individual. (Integer)
# Gender: Gender of the individual. (String)
# Height: Height of the individual in centimeters. (Integer)
# Weight: Weight of the individual in kilograms. (Integer)
# Ap_hi: Systolic blood pressure reading. (Integer)
# Ap_lo : Diastolic blood pressure reading. (Integer)
# Cholesterol : Cholesterol level of the individual. (Integer)
# Gluc : Glucose level of the individual. (Integer)
# smoke: Smoking status of the individual. (Boolean)
# alco: Alcohol consumption status of the individual. (Boolean)
# active: Physical activity level of the individual. (Boolean)
# cardio: Presence or absence of cardiovascular disease. (Boolean)

#Purpose of the dataset: to understand and to share with peers the correlation between risk factors and cardiovascular heart disease. Data is available to show how smoking and drinking may increase chances of cardiovascular disease, therefore it is important for users to be responsible and take care of their bodies. 

#From Homework 3
getwd()
library(tidyverse)
#Change working directory
setwd("/Users/NansKashyap/downloads")
#Load heart_data
#load(tidyverse)
heart <- read_csv("heart_data.csv")
View(heart)

#The number of rows (70,000) and columns (14)
str(heart)
#Summarize variables
summary(heart)

#From Homework 4
heart %>%
  ggplot(aes(height, weight)) +
  geom_point(color="dodgerblue")
ggsave("scatterplot_HtWt.png")

heart %>%
  mutate(gender2 = gender == 1) %>%
  ggplot(aes(gender2, age)) +
  geom_boxplot(color="dodgerblue")
  


#Please note that due to the provided dataset, age is given in days and not years. 15000 days approximates to 41 years of age and 20000 days approximates to 55 years of age.
#Please note that the weight is provided in kilograms.

#Challenge Problem
heart %>%
  ggplot(aes(age, weight)) +
  geom_point(color="dodgerblue", alpha = 0.05)

ggsave("scatterplot_AgeWt.png")
#Based on the above scatter plot, there is a higher likelihood that anyone over the age of 38 and weighing between 38 and 125 kg is susceptible to cardiovascular heart disease.

#Additional Analyses
heart %>%
  select(ap_hi, ap_lo) %>%
  drop_na() %>%
  cor(method="spearman")
#Based on the above analysis, there is a positive Spearman correlation (0.74) between ap_lo and ap_hi (blood pressure). 
#This makes sense because a healthy heart requires a balanced blood pressure reading. The Systolic (ap_hi) number is a telltale indicator of how much pressure your blood is exerting against your artery walls when the heart beats.   

# Linear regression line to a plot in ggplot
ggplot(heart, aes(height, weight)) +
  geom_point() +
  geom_smooth(method=lm)

heartLM <- lm(height ~ weight, data = heart)
summary(heartLM)

#Please note that height is measured in centimeters and weight is measured in kilograms
#For the above linear regression, height and weight appear to have a positive correlation, meaning that a taller and heavier person is more susceptible to cardiovascular disease than a shorter person who weighs less.

#T-test 
t.test(cholesterol ~ cardio, data = heart)
#For the t-test a simple check was performed to see if cholesterol levels (independent) had caused cardiovascular diseases (dependent)
#We can reject the null hypothesis that there is a true difference in means between both variables.

#Challenge Problem
heart %>%
  select(age,smoke) %>%
  drop_na() %>%
  cor(method="spearman")
#Based on the above analysis, there is a negative Spearman correlation (-0.05) between smoke and age
#This came up as a surprise, as there is not much of a relationship between smokers and their age. This implies that cardiovascular heart diseases can happen among smokers of any age. 



