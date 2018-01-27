# Program that creates file that has website information for N photos along with zeroed features

# Load packages and data
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")
library(jsonlite)
library(dplyr)
library(purrr)
library(knitr)
library(stringr)

train <- fromJSON("train.json")
# Set all variable names except `photos`` and `feactures`` into an array
column <- setdiff(names(train), c("photos", "features"))
# Unlist every variable except `photos` and `features` and convert to tibble
train <- map_at(train, column, unlist) %>% tibble::as_tibble(.)

# Creating dataframe of the images to use in model
photos.info <- data.frame(link = character(0), 
                          # light = character(0),
                          # spacious = character(0),
                          # GoodKitchen = character(0),
                          # GoodBathroom = character(0),
                          # wow = character(0),
                          # layout = character(0), 
                          interest = character(0), stringsAsFactors=FALSE)

# Loop over all train data to fill websites into photos data frame
k <- 1
for ( i in 1:dim(train)[1] ) {
  if (length(train$photos[i][[1]]) > 0) {
    for ( j in 1:length(train$photos[i][[1]]) ) {
      photos.info[k, 1] =  train$photos[i][[1]][j]  # website
      # photos.info[k, 2] =  0  # light
      # photos.info[k, 3] =  0  # spacious
      # photos.info[k, 4] =  0  # GoodKitchen
      # photos.info[k, 5] =  0  # GoodBathroom
      # photos.info[k, 6] =  0  # wow
      # photos.info[k, 7] =  0  # layout
      photos.info[k, 2] =  train$interest_level[i]  # light
      k <- k+1
    }
  }
}

# Saving CSV file with website info
write.csv(file="PhotoTraining.csv", photos.info)

# Saving new.variables into an .RData file
save(photos.info, file = "photosInfo.RData")