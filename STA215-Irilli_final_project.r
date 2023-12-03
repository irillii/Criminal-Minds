## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data - Sheet1 (1).csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################

# EXAMINE QUANT_VAR1
table(data$Number_of_Victims)
mean(data$Number_of_Victims)
sd(data$Number_of_Victims)
describe(data$Number_of_Victims)
summary(data$Number_of_Victims)

# EXAMINE QUANT_VAR2
table(data$Days_Spent_Investigating)
mean(data$Days_Spent_Investigating)
sd(data$Days_Spent_Investigating)
describe(data$Days_Spent_Investigating)
summary(data$Days_Spent_Investigating)

# EXAMINE QUANT_VAR3
table(data$Offender_Age)
mean(data$Offender_Age)
sd(data$Offender_Age)
describe(data$Offender_Age)
summary(data$Offender_Age)

# EXAMINE QUAL_VAR1
table(data$`Victim/Offender_Relationship `)
describe(data$`Victim/Offender_Relationship `)

# EXAMINE QUAL_VAR2
table(data$Crime_Committed)
describe(data$Crime_Committed)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$`Victim/Offender_Relationship `,data$Crime_Committed)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$`Victim/Offender_Relationship `,data$Crime_Committed))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################

anova_adapted <- aov(data$`Victim/Offender_Relationship ` ~ data$Number_of_Victims data= data)
summary(anova_adapted)

anova_adapted <- aov(data$Crime_Committed ~ Offender_Age, data= data)
summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
linear_plot <- plot(data$Offender_Age, data$Number_of_Victims)
print(linear_plot)
cor(data$Offender_Age, data$Number_of_Victims)
##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(data$Number_of_Victims~ data$Offender_Age, data=data)
summary(linear_relationship)
abline(linear_relationship, col= "red")
##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
#Add linear regression line to the scatter plot
abline(h=3.155556)
abline(v=32)
##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Offender_Age, residuals(linear_relationship))
abline(h=0, col = "red")
