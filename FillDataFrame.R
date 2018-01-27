# Loading function to create empty data frame 
source("CreateDataFrame.R")

# Using new features identified and created in RelevantFeaturesIdentification.R
load(file = "RData/newFeatures.RData")

fillDataFrame <- function(rawData) {

  data.features <- createFrame()
  
  # Loop over all rawData data to fill data.features dataframe
  i <- 1
  while ( i<=dim(rawData)[1]  ) {

    if (!is.na(rawData$listing_id[i])) data.features[i,1] <- rawData$listing_id[i]
    else data.features[i,1] = "0"
    
    if (!is.na(rawData$bathrooms[i])) data.features$bathrooms[i] = rawData$bathrooms[i]
    else data.features$bathrooms[i] = 0
    
    if (!is.na(rawData$bedrooms[i])) data.features$bedrooms[i] = rawData$bedrooms[i]
    else data.features$bedrooms[i] = 0
    
    if (!is.na(rawData$created[i])) {
      data.features$created[i] = as.factor(rawData$created[i])
      data.features$yday[i] = yday(rawData$created[i])
      data.features$month[i] = month(rawData$created[i])
      data.features$mday[i] = mday(rawData$created[i])
      data.features$wday[i] = wday(rawData$created[i])
      data.features$hour[i] = hour(rawData$created[i])
    }
    else {
      data.features$created[i] = as.factor(0)
      data.features$yday[i] = 0
      data.features$month[i] = 0
      data.features$mday[i] = 0
      data.features$wday[i] = 0
      data.features$hour[i] = 0
    }
    
    if (!is.na(rawData$photos[i])) data.features$nphotos[i] = length(rawData$photos[i][[1]])
    else data.features$nphotos[i] = 0
    
    if (!is.na(rawData$description[i])) data.features$ndescription[i] = nchar(rawData$description[i])
    else data.features$ndescription[i] = 0
    
    if (!is.na(rawData$latitude[i])) data.features$latitude[i] = rawData$latitude[i]
    else data.features$latitude[i] = 0
    
    if (!is.na(rawData$longitude[i])) data.features$longitude[i] = rawData$longitude[i]
    else data.features$longitude[i] = 0
    
    if (!is.na(rawData$price[i])) data.features$price[i] = rawData$price[i]
    else data.features$price[i] = 0
    
    if ("interest_level" %in% colnames(rawData)) {
      if (!is.na(rawData$interest_level[i])) data.features$interest[i] = rawData$interest_level[i]
      else data.features$interest[i] = "NG"
    }
    else data.features$interest[i] = "NG"
    
    # Assume no features/description are given
    data.features[i,22:66] = as.factor(0)
    
    # Add features given in the add
    temp <- tolower(unlist(rawData[i,]$features))
    count.features = 0
    j <- 1
    while (j <= dim(new.features)[1] & !is_empty(temp)) {   # loop over new features
      if ( sum(sapply(str_detect(temp, new.features[j,1]) & !str_detect(temp, new.features[j,2]), "+")) > 0) {
        data.features[i, 21+j] = as.factor(1)
        count.features = count.features + 1
      }
      j <- j+1
    }
    
    # Add description given in the add
    temp <- tolower(unlist(rawData[i,]$description))
    j <- 1
    while (j <= dim(new.features)[1] & !is_empty(temp)) {   # loop over new features
      if ( sum(sapply(str_detect(temp, new.features[j,1]) & !str_detect(temp, new.features[j,2]), "+")) > 0) {
        if(data.features[i, 21+j] == 0) {
          count.features = count.features + 1
          data.features[i, 21+j] = as.factor(1)
        }
      }
      j <- j+1
    }
    
    data.features$countFeatures[i] = count.features
    
    print(i)
    i <- i+1
  }
  
  # Converting to factors
  data.features$created <- as.factor(data.features$created)
  data.features$manager_id <- as.factor(data.features$manager_id)
  data.features$interest <- as.factor(data.features$interest)
  data.features$listing_id <- as.factor(data.features$listing_id)
  for (i in 22:66) {data.features[,i] <- as.factor(data.features[,i])}
  
  for (i in 1:dim(data.features)[1]) {
    ifelse(data.features$bedrooms[i] > 0, data.features$price.bedrooms[i] <- data.features$price[i]/data.features$bedrooms[i],
           data.features$price.bedrooms[i] <- data.features$price[i])
    ifelse(data.features$countFeatures[i] > 0, data.features$price.countFeatures[i] <- data.features$price[i]/data.features$countFeatures[i],
           data.features$price.countFeatures[i] <- data.features$price[i])
  }
  
  return(data.features)
}
