library(readr)
data <- read_csv("Desktop/college/semester4/DANA4840/Project/CleanedDataN.csv")
ibrary(readr)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(olsrr)
View(data)
colnames(data)
str(data)
table(data$Position)
filteredData<-data %>%
  filter(Position != 'GK')
str(data)
summary(filteredData)

library(car)

filteredData <- data[, c("Wage","Skill Moves","Value", "Age", "Overall", "Potential", "Height", "Weight", "Crossing", 
                         "Finishing", "HeadingAccuracy", "ShortPassing", "Volleys", "Dribbling", "Curve",
                         "FKAccuracy", "LongPassing", "BallControl", "Acceleration", "SprintSpeed", "Agility",
                         "Reactions", "Balance", "ShotPower", "Jumping", "Stamina", "Strength", "LongShots",
                         "Aggression", "Interceptions", "Positioning", "Vision", "Penalties", "Composure",
                         "Marking", "StandingTackle", "SlidingTackle"
)]
goalkeeper<-data[, c("Wage","Skill Moves","Value", "Age", "Overall", "Potential", "Height", "Weight", "Crossing", 
                     "Finishing", "HeadingAccuracy", "ShortPassing", "Volleys", "Dribbling", "Curve",
                     "FKAccuracy", "LongPassing", "BallControl", "Acceleration", "SprintSpeed", "Agility",
                     "Reactions", "Balance", "ShotPower", "Jumping", "Stamina", "Strength", "LongShots",
                     "Aggression", "Interceptions", "Positioning", "Vision", "Penalties", "Composure",
                     "Marking", "StandingTackle", "SlidingTackle","GKDiving","GKHandling","GKKicking","GKPositioning","GKReflexes"
)]

library(GGally)
ggcorr(filteredData, label=T)
sum(is.na(filteredData))

#splitting the data in training and testing data
splitdata = sort(sample(nrow(filteredData), nrow(filteredData)*.7))
train<-filteredData[splitdata,]
View(train)
str(train)
str(filteredData)
test<-filteredData[-splitdata,]
View(test)
# Fit the full model on training data
full.model <- lm(Wage ~ ., data = train, na.action = na.omit)
vif(full.model)
summary(full.model)
#Fit the model with the training data

model <- lm(Wage ~ ., data = train)
ols_step_both_p(model, details = TRUE)
#Applying stepwise on testing data 
model_test<-lm(Wage ~ ., data = test)
ols_step_both_p(model_test, details = TRUE)

###As we see that there is no significant changes in the R2 values , the best model suggested by the model is the one


