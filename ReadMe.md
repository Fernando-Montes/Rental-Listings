Machine learning project created with the goal to predict the number of inquiries a new listing receives based on information such as apartment description, photos, number of bedrooms, price, listing's date, etc. The data comes from Kaggle competition (<https://www.kaggle.com/c/two-sigma-connect-rental-listing-inquiries/data>). The model is not as complete as I would like it to be since for example model ensembling is still missing and some work related to picture processing is not complete.

-   **RelevantFeaturesIdentification.R**: File used to identify common features in the description field (mostly) for all apartments. These common features become variables (has/has not) in the model. It creates newFeatures.RData.

-   **CreateDataFrame.R**: Helper function that returns empty data frame with some features to be used in model.
-   **FillDataFrame.R**: Helper function fillDataFrame that fills data frame with raw data and some engineered features.

-   **Geography.R**: Source to add features from the google maps api: neighboorhod and political district of each apartment. It creates neighFeaturesTrain.RData.
-   **ManagerFeatures.R**: Source to add features classifying how effective the listing property manager is. It creates ManagerFeatures.RData.
-   **BuildingFeatures.R**: Source to add features classifying how much interest apartments in the same building have attracted. It creates buildingFeatures.RData.
-   **ComparisonFeatures.R**: Source to add features from neighFeaturesTrain.RData, ManagerFeatures.RData, buildingFeatures.RData and some additional engineered features (i.e. number of features in a given apartment compared to other apartments in the same neighborhood) into a data frame that includes all the features to be included in the model (AllData.RData).
-   **h2o-model.R**: File to run a single gbm model with different feature sets using the h20 package.
-   **h2o-model-optimization.R**: File to find and optimize hyper-parameters of the model.
-   **ModelAnalysis.R**: File to run a random forest, generalized linear model and a boosted regression model on the data. The implementation is done using the caret R package (ranger, glmnet, gbm, methods respectively). Some hyper-parameter optimization has been done but further optimization is necessary.

-   **Visual.R**: File to visualize data.
-   **CreatePhotoDataFrame.R**: Helper file to create CSV file with website information where to find photos for a given apartment.
-   **ProcessingImage.R**: Incomplete file that uses a CNN to identify features in a photo.
