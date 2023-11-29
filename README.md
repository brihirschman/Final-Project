# Final-Project
data for final project for FRIENDS
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
data <- read_delim("raw_data.csv")


##################################################################################
#################### Examine descriptive statistics                 ####################   
##################################################################################

# EXAMINE humor
table(data$humor)
mean(data$humor)
sd(data$humor)
summary(data$humor)

# EXAMINE rating
table(data$rating)
mean(data$rating)
sd(data$rating)

# EXAMINE location
table(data$location)

# EXAMINE topic
table(data$topic)

# EXAMINE monica
table(data$monica)

# EXAMINE rachel
table(data$rachel)

# EXAMINE chandler
table(data$chandler)

# EXAMINE joey
table(data$joey)

# EXAMINE pheobe
table(data$phoebe)

# EXAMINE ross
table(data$ross)

table(data$topic)
table(data$location)

##################################################################################
#################### Step 1                 ####################   
##################################################################################

mean(data$topic)
mean(data$rating)
mean(data$location)
mean(data$humor)
mean(data$monica)
mean(data$rachel)
mean(data$chandler)
mean(data$joey)
mean(data$phoebe)
mean(data$ross)

sd(data$topic)
sd(data$rating)
sd(data$location)
sd(data$humor)
sd(data$monica)
sd(data$rachel)
sd(data$chandler)
sd(data$joey)
sd(data$phoebe)
sd(data$ross)

describe(data$topic)
describe(data$rating)
describe(data$location)
describe(data$humor)
describe(data$monica)
describe(data$rachel)
describe(data$chandler)
describe(data$joey)
describe(data$phoebe)
describe(data$ross)

summary(data$topic)
summary(data$rating)
summary(data$location)
summary(data$humor)
summary(data$monica)
summary(data$rachel)
summary(data$chandler)
summary(data$joey)
summary(data$phoebe)
summary(data$ross)

##################################################################################
#################### Step 2                 ####################   
##################################################################################

table(data$joey,data$chandler)

##################################################################################
#################### Step 3                 ####################   
##################################################################################

chisq.test(table(data$monica,data$ross))

##################################################################################
#################### Step 4                 ####################   
##################################################################################

aov(humor ~ chandler, data = data)

##################################################################################
#################### Step 5                 ####################   
##################################################################################

cor(data$location, data$topic)

##################################################################################
#################### Step 6                 ####################   
##################################################################################

linear_relationship <-lm(data$humor~data$rating, data= data)

##################################################################################
#################### Step 7                  ####################   
##################################################################################

plot(data$humor, data$rating)
abline (linear_relationship, col = "pink")
mean (data$humor)
mean (data$rating)
abline(a=NULL, b=NULL, h=6.028571 , v=6.2 ,col= "lightblue")

##################################################################################
#################### Step 8                 ####################   
##################################################################################

plot(data$humor,residuals(linear_relationship))
abline(h=0, col = "orange")
