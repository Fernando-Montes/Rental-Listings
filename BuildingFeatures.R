# Load packages and data
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")
library(jsonlite)
library(dplyr)
library(purrr)
library(knitr)
library(stringr)
library(data.table) 

train <- fromJSON("Data/train.json")
# Set all variable names except `photos`` and `feactures`` into an array
column <- setdiff(names(train), c("photos", "features"))
# Unlist every variable except `photos` and `features` and convert to tibble
train <- map_at(train, column, unlist) %>% tibble::as_tibble(.)

diffbuilding <- unique(train$building_id)

building.features <- data.frame(building_id = character(0), 
                                buildingNoLis = numeric(0),
                                buildingLoPer = numeric(0),
                                buildingMePer = numeric(0),
                                buildingHiPer = numeric(0),
                                buildingSkill = numeric(0), stringsAsFactors=FALSE)

# Loop over all different buildings
i <- 1
while ( i<=length(diffbuilding)  ) {
  building.features[i,1] = diffbuilding[i]
  building.features$buildingNoLis[i] = dim(train[train$building_id == diffbuilding[i],])[1]
  building.features$buildingLoPer[i] = dim(train[train$building_id == diffbuilding[i] & train$interest_level == "low",])[1]/dim(train[train$building_id == diffbuilding[i] ,])[1]
  building.features$buildingMePer[i] = dim(train[train$building_id == diffbuilding[i] & train$interest_level == "medium",])[1]/dim(train[train$building_id == diffbuilding[i] ,])[1]
  building.features$buildingHiPer[i] = dim(train[train$building_id == diffbuilding[i] & train$interest_level == "high",])[1]/dim(train[train$building_id == diffbuilding[i] ,])[1]
  building.features$buildingSkill[i] = building.features$buildingMePer[i] + 2.0*building.features$buildingHiPer[i]
  i <- i+1
}
# Add percertanges for the average manager
building.features[i,1] = "Average"
building.features$buildingLoPer[i] = dim(train[train$interest_level == "low",])[1]/dim(train)[1]
building.features$buildingMePer[i] = dim(train[train$interest_level == "medium",])[1]/dim(train)[1]
building.features$buildingHiPer[i] = dim(train[train$interest_level == "high",])[1]/dim(train)[1]
building.features$buildingSkill[i] = building.features$buildingMePer[i] + 2.0*building.features$buildingHiPer[i]

# Saving manager.features into an .RData file
save(building.features, file = "RData/buildingFeatures.RData")