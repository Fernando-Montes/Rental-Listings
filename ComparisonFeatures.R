# Load packages and data
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")
library(jsonlite)
library(dplyr)
library(purrr)
library(knitr)
library(stringr)
library(data.table) 

# Loading fillDataFrame function to create data frame after feature engineering
source("FillDataFrame.R")  

# Preparing train data ---------------------
train <- fromJSON("Data/train.json")
# Set all variable names except `photos`` and `feactures`` into an array
column <- setdiff(names(train), c("photos", "features"))
# Unlist every variable except `photos` and `features` and convert to tibble
train <- map_at(train, column, unlist) %>% tibble::as_tibble(.)

t1 <- fillDataFrame(train)

load(file = "RData/neighFeaturesTrain.RData")
t1$neighborhood <- neigh.features$neighborhood
t1$political <- neigh.features$political

# Preparing test data ---------------------
test <- fromJSON("Data/test.json")
# Set all variable names except `photos`` and `feactures`` into an array
column <- setdiff(names(train), c("photos", "features"))
# Unlist every variable except `photos` and `features` and convert to tibble
test <- map_at(test, column, unlist) %>% tibble::as_tibble(.)

s1 <- fillDataFrame(test)

load(file = "RData/neighFeaturesTest.RData")
s1$neighborhood <- neigh.features$neighborhood
s1$political <- neigh.features$political

# Combining train and test data
AllData <- rbind(t1, s1)

AllData$neighborhood <- as.factor(AllData$neighborhood)
AllData$political <- as.factor(AllData$political)

# Saving data
save(t1, file = "RData/TrainProcessed.RData")
save(s1, file = "RData/TestProcessed.RData")
save(AllData, file = "RData/AllData.RData")

# Preparing comparison data ----------------

for (i in 1:dim(AllData)[1]) {
#for (i in 1:3) {
 
  # Adding ratio of price per average price in the neighborhood for same number of bedrooms
  AllData$price.perNeigh[i] <- 
    AllData$price[i]/mean(AllData[(AllData$neighborhood == AllData$neighborhood[i] & AllData$bedrooms == AllData$bedrooms[i]) %in% TRUE,]$price)
  AllData$price.perPolit[i] <- 
    AllData$price[i]/mean(AllData[(AllData$political == AllData$political[i] & AllData$bedrooms == AllData$bedrooms[i]) %in% TRUE,]$price)
  
  # Adding ratio of number of features per average number of features in the neighborhood for same number of bedrooms
  AllData$countFeatures.perNeigh[i] <- 
    AllData$countFeatures[i]/mean(AllData[(AllData$neighborhood == AllData$neighborhood[i] & AllData$bedrooms == AllData$bedrooms[i]) %in% TRUE,]$countFeatures)
  AllData$countFeatures.perPolit[i] <- 
    AllData$countFeatures[i]/mean(AllData[(AllData$political == AllData$political[i] & AllData$bedrooms == AllData$bedrooms[i]) %in% TRUE,]$countFeatures)
  
  # Number of days (inclusive) the add may have stayed up: 1 -------------------
  temp.numListings.perNeigh <- c()
  temp.price.perNeigh <- c()
  temp.countFeatures.perNeigh <- c()
  temp.numListings.perPolit <- c()
  temp.price.perPolit <- c()
  temp.countFeatures.perPolit <- c()
  for( k in 0:1 ) {   # Loop over the all days
    
      hiDay <- ifelse(AllData$yday[i] + k > 181, 181, AllData$yday[i] + k)
      loDay <- ifelse(AllData$yday[i] + k - 1 < 92, 92, AllData$yday[i] + k - 1)
      
      temp <- AllData[(AllData$neighborhood == AllData$neighborhood[i] & AllData$bedrooms == AllData$bedrooms[i] & 
                         hiDay >= AllData$yday & loDay <= AllData$yday) %in% TRUE, ]
      temp.numListings.perNeigh <- c(temp.numListings.perNeigh, dim(temp)[1]/(hiDay-loDay)) # Add number of listings/day
      temp.price.perNeigh <- c(temp.price.perNeigh, unlist(temp$price))                             # Add price list
      temp.countFeatures.perNeigh <- c(temp.countFeatures.perNeigh, unlist(temp$countFeatures))     # Add countFeature list
      
      temp <- AllData[(AllData$political == AllData$political[i] & AllData$bedrooms == AllData$bedrooms[i] & 
                         hiDay >= AllData$yday & loDay <= AllData$yday) %in% TRUE, ]
      temp.numListings.perPolit <- c(temp.numListings.perPolit, dim(temp)[1]/(hiDay-loDay)) # Add number of listings/day
      temp.price.perPolit <- c(temp.price.perPolit, unlist(temp$price))                             # Add price list
      temp.countFeatures.perPolit <- c(temp.countFeatures.perPolit, unlist(temp$countFeatures))     # Add countFeature list
      
      
  }   
  AllData$numListings.perNeigh.0[i] <- mean(temp.numListings.perNeigh)
  AllData$price.perNeigh.0[i] <- AllData$price[i]/mean(temp.price.perNeigh)
  AllData$countFeatures.perNeigh.0[i] <- AllData$countFeatures[i]/mean(temp.countFeatures.perNeigh)
  AllData$numListings.perPolit.0[i] <- mean(temp.numListings.perPolit)
  AllData$price.perPolit.0[i] <- AllData$price[i]/mean(temp.price.perPolit)
  AllData$countFeatures.perPolit.0[i] <- AllData$countFeatures[i]/mean(temp.countFeatures.perPolit)
  
  # Number of days (inclusive) the add may have stayed up: 5 -------------------
  temp.numListings.perNeigh <- c()
  temp.price.perNeigh <- c()
  temp.countFeatures.perNeigh <- c()
  temp.numListings.perPolit <- c()
  temp.price.perPolit <- c()
  temp.countFeatures.perPolit <- c()
  for( k in 0:5 ) {   # Loop over the all days
    
    hiDay <- ifelse(AllData$yday[i] + k > 181, 181, AllData$yday[i] + k)
    loDay <- ifelse(AllData$yday[i] + k - 5 < 92, 92, AllData$yday[i] + k - 5)
    
    temp <- AllData[(AllData$neighborhood == AllData$neighborhood[i] & AllData$bedrooms == AllData$bedrooms[i] & 
                       hiDay >= AllData$yday & loDay <= AllData$yday) %in% TRUE, ]
    temp.numListings.perNeigh <- c(temp.numListings.perNeigh, dim(temp)[1]/(hiDay-loDay)) # Add number of listings/day
    temp.price.perNeigh <- c(temp.price.perNeigh, unlist(temp$price))                             # Add price list
    temp.countFeatures.perNeigh <- c(temp.countFeatures.perNeigh, unlist(temp$countFeatures))     # Add countFeature list
    
    temp <- AllData[(AllData$political == AllData$political[i] & AllData$bedrooms == AllData$bedrooms[i] & 
                       hiDay >= AllData$yday & loDay <= AllData$yday) %in% TRUE, ]
    temp.numListings.perPolit <- c(temp.numListings.perPolit, dim(temp)[1]/(hiDay-loDay)) # Add number of listings/day
    temp.price.perPolit <- c(temp.price.perPolit, unlist(temp$price))                             # Add price list
    temp.countFeatures.perPolit <- c(temp.countFeatures.perPolit, unlist(temp$countFeatures))     # Add countFeature list
    
    
  }   
  AllData$numListings.perNeigh.5[i] <- mean(temp.numListings.perNeigh)
  AllData$price.perNeigh.5[i] <- AllData$price[i]/mean(temp.price.perNeigh)
  AllData$countFeatures.perNeigh.5[i] <- AllData$countFeatures[i]/mean(temp.countFeatures.perNeigh)
  AllData$numListings.perPolit.5[i] <- mean(temp.numListings.perPolit)
  AllData$price.perPolit.5[i] <- AllData$price[i]/mean(temp.price.perPolit)
  AllData$countFeatures.perPolit.5[i] <- AllData$countFeatures[i]/mean(temp.countFeatures.perPolit)
    
  # Number of days (inclusive) the add may have stayed up: 10 -------------------
  temp.numListings.perNeigh <- c()
  temp.price.perNeigh <- c()
  temp.countFeatures.perNeigh <- c()
  temp.numListings.perPolit <- c()
  temp.price.perPolit <- c()
  temp.countFeatures.perPolit <- c()
  for( k in 0:10 ) {   # Loop over the all days
    
    hiDay <- ifelse(AllData$yday[i] + k > 181, 181, AllData$yday[i] + k)
    loDay <- ifelse(AllData$yday[i] + k - 10 < 92, 92, AllData$yday[i] + k - 10)
    
    temp <- AllData[(AllData$neighborhood == AllData$neighborhood[i] & AllData$bedrooms == AllData$bedrooms[i] & 
                       hiDay >= AllData$yday & loDay <= AllData$yday) %in% TRUE, ]
    temp.numListings.perNeigh <- c(temp.numListings.perNeigh, dim(temp)[1]/(hiDay-loDay)) # Add number of listings/day
    temp.price.perNeigh <- c(temp.price.perNeigh, unlist(temp$price))                             # Add price list
    temp.countFeatures.perNeigh <- c(temp.countFeatures.perNeigh, unlist(temp$countFeatures))     # Add countFeature list
    
    temp <- AllData[(AllData$political == AllData$political[i] & AllData$bedrooms == AllData$bedrooms[i] & 
                       hiDay >= AllData$yday & loDay <= AllData$yday) %in% TRUE, ]
    temp.numListings.perPolit <- c(temp.numListings.perPolit, dim(temp)[1]/(hiDay-loDay)) # Add number of listings/day
    temp.price.perPolit <- c(temp.price.perPolit, unlist(temp$price))                             # Add price list
    temp.countFeatures.perPolit <- c(temp.countFeatures.perPolit, unlist(temp$countFeatures))     # Add countFeature list
    
    
  }   
  AllData$numListings.perNeigh.10[i] <- mean(temp.numListings.perNeigh)
  AllData$price.perNeigh.10[i] <- AllData$price[i]/mean(temp.price.perNeigh)
  AllData$countFeatures.perNeigh.10[i] <- AllData$countFeatures[i]/mean(temp.countFeatures.perNeigh)
  AllData$numListings.perPolit.10[i] <- mean(temp.numListings.perPolit)
  AllData$price.perPolit.10[i] <- AllData$price[i]/mean(temp.price.perPolit)
  AllData$countFeatures.perPolit.10[i] <- AllData$countFeatures[i]/mean(temp.countFeatures.perPolit)
  
  print(i)  
}

save(AllData, file = "RData/AllData.RData")

# Adding additional features I had not thought about before -------------

# Using building features created in BuildingFeatures.R
load(file = "RData/buildingFeatures.RData")

# Using manager features created in ManagerFeatures.R
load(file = "RData/ManagerFeatures.RData")

for (i in 1:dim(AllData)[1]) {
  
  # Adding building_id score
  ifelse (i <= 49352, AllData$building_id[i] <- train$building_id[i], AllData$building_id[i] <- test$building_id[i-49352] )
  
  if ( AllData$building_id[i] %in% building.features$building_id) {
    if ( building.features[building.features$building_id == AllData$building_id[i], 2] >= 10 ) {
      AllData$buildingNoLis[i] = building.features[building.features$building_id == AllData$building_id[i], 2]
      AllData$buildingLoPer[i] = building.features[building.features$building_id == AllData$building_id[i], 3]
      AllData$buildingMePer[i] = building.features[building.features$building_id == AllData$building_id[i], 4]
      AllData$buildingHiPer[i] = building.features[building.features$building_id == AllData$building_id[i], 5]
      AllData$buildingSkill[i] = building.features[building.features$building_id == AllData$building_id[i], 6]
    }
    else {
      AllData$buildingNoLis[i] = 0
      AllData$buildingLoPer[i] = building.features[7586, 3]
      AllData$buildingMePer[i] = building.features[7586, 4]
      AllData$buildingHiPer[i] = building.features[7586, 5]
      AllData$buildingSkill[i] = building.features[7586, 6]
    }
  }
  else {
    AllData$buildingNoLis[i] = 0
    AllData$buildingLoPer[i] = building.features[7586, 3]
    AllData$buildingMePer[i] = building.features[7586, 4]
    AllData$buildingHiPer[i] = building.features[7586, 5]
    AllData$buildingSkill[i] = building.features[7586, 6]
  }
  
  # Adding manager_id score
  ifelse (i <= 49352, AllData$manager_id[i] <- train$manager_id[i], AllData$manager_id[i] <- test$manager_id[i-49352] )
  
  if ( AllData$manager_id[i] %in% manager.features$manager_id) {
    if ( manager.features[manager.features$manager_id == AllData$manager_id[i], 2] >= 10 ) {
      AllData$managerNoLis[i] = manager.features[manager.features$manager_id == AllData$manager_id[i], 2]
      AllData$managerLoPer[i] = manager.features[manager.features$manager_id == AllData$manager_id[i], 3]
      AllData$managerMePer[i] = manager.features[manager.features$manager_id == AllData$manager_id[i], 4]
      AllData$managerHiPer[i] = manager.features[manager.features$manager_id == AllData$manager_id[i], 5]
      AllData$managerSkill[i] = manager.features[manager.features$manager_id == AllData$manager_id[i], 6]
    }
    else {
      AllData$managerNoLis[i] = 0
      AllData$managerLoPer[i] = manager.features[3482, 3]
      AllData$managerMePer[i] = manager.features[3482, 4]
      AllData$managerHiPer[i] = manager.features[3482, 5]
      AllData$managerSkill[i] = manager.features[3482, 6]
    }
  }
  else {
    AllData$managerNoLis[i] = 0
    AllData$managerLoPer[i] = manager.features[3482, 3]
    AllData$managerMePer[i] = manager.features[3482, 4]
    AllData$managerHiPer[i] = manager.features[3482, 5]
    AllData$managerSkill[i] = manager.features[3482, 6]
  }
  
  print(i)
}

save(AllData, file = "RData/AllData.RData")
load("RData/AllData.RData")