#WiFi based Locationing - LM and KNN
#Minhaz 
#V2


# Calling Packages --------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(caret)
library(ggplot2)



# Initial Dataframe -------------------------------------------------------

Initial_Data_test <- read.csv("UJIndoorLoc/trainingData.csv")

Initial_Data_validation <- read.csv("UJIndoorLoc/validationData.csv")



# Create subset of building0 and Floor0 -----------------------------------

Init_DF_Test_B0_F0 <- Initial_Data_test %>%
  filter(BUILDINGID == 0, FLOOR == 0)

Init_DF_Valid_B0_F0 <- Initial_Data_validation %>%
  filter(BUILDINGID == 0, FLOOR == 0)

# Create a model for New Dataframe(Building 0 and Floor 0) -----------------

# Setting randomization

set.seed(121)

# Define Cross Validation

WiFi_fitControl<- 
  trainControl(method = "repeatedcv", number = 10, repeats = 10)


# Run an LM Model Building 0 and floor 0

B0_F0_LM_Longitude <- train(LONGITUDE~. 
                          -LATITUDE -FLOOR -BUILDINGID -SPACEID -RELATIVEPOSITION
                          -USERID -PHONEID -TIMESTAMP,
                           data = Init_DF_Test_B0_F0,
                           method = "lm", trControl = WiFi_fitControl)

B0_F0_LM_Latitude <- train(LATITUDE~. 
                           -LONGITUDE -FLOOR -BUILDINGID -SPACEID -RELATIVEPOSITION
                           -USERID -PHONEID -TIMESTAMP,
                           data = Init_DF_Test_B0_F0,
                           method = "lm", trControl = WiFi_fitControl)

# Make Prediction on Validation set

B0_F0_LM_Latitude_Pred <- predict(B0_F0_LM_Latitude, Init_DF_Valid_B0_F0)

B0_F0_LM_Longitude_Pred <- predict(B0_F0_LM_Longitude, Init_DF_Valid_B0_F0)

postResample(B0_F0_LM_Latitude_Pred, Init_DF_Valid_B0_F0$LATITUDE)

postResample(B0_F0_LM_Longitude_Pred, Init_DF_Valid_B0_F0$LONGITUDE)


# Create Data frame with predicted and real latitude and longitude from LM --------

NDF <- data.frame(B0_F0_LM_Longitude_Pred, B0_F0_LM_Latitude_Pred, 
                  Init_DF_Valid_B0_F0$LONGITUDE, Init_DF_Valid_B0_F0$LATITUDE)

NDF1 <- data.frame(B0_F0_LM_Longitude_Pred, B0_F0_LM_Latitude_Pred, rep("predicted"))

NDF2 <- data.frame(Init_DF_Valid_B0_F0$LONGITUDE, Init_DF_Valid_B0_F0$LATITUDE, rep("actual"))

names(NDF1) <- c("B0_F0_Longitude", "B0_F0_Latitude", "actual_Predicted")

names(NDF2) <- c("B0_F0_Longitude", "B0_F0_Latitude", "actual_Predicted")

NDF3 <- rbind(NDF1, NDF2)

# visualize

Visual_B0_F0 <- ggplot(NDF3, aes(B0_F0_Latitude, B0_F0_Longitude,
                                 colour = actual_Predicted)) + 
  geom_point() + xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("LM result for Building 0 and Floor 0")


# Run KNN Model for Buliding 0 and Floor 0 --------------------------------

# Run an KNN Model Building 0 and floor 0

B0_F0_KNN_Longitude <- train(LONGITUDE~. 
                            -LATITUDE -FLOOR -BUILDINGID -SPACEID -RELATIVEPOSITION
                            -USERID -PHONEID -TIMESTAMP,
                            data = Init_DF_Test_B0_F0,
                            method = "knn", trControl = WiFi_fitControl)

B0_F0_KNN_Latitude <- train(LATITUDE~. 
                           -LONGITUDE -FLOOR -BUILDINGID -SPACEID -RELATIVEPOSITION
                           -USERID -PHONEID -TIMESTAMP,
                           data = Init_DF_Test_B0_F0,
                           method = "knn", trControl = WiFi_fitControl)

# Make Prediction on Validation set

B0_F0_knn_Latitude_Pred <- predict(B0_F0_KNN_Latitude, Init_DF_Valid_B0_F0)

B0_F0_knn_Longitude_Pred <- predict(B0_F0_KNN_Longitude, Init_DF_Valid_B0_F0)

postResample(B0_F0_knn_Latitude_Pred, Init_DF_Valid_B0_F0$LATITUDE)

postResample(B0_F0_knn_Longitude_Pred, Init_DF_Valid_B0_F0$LONGITUDE)


# Create Data frame with predicted and real latitude and longitude --------


NDF_KN_Predicted <- data.frame(B0_F0_knn_Longitude_Pred, 
                               B0_F0_knn_Latitude_Pred, 
  rep("predicted"))

NDF_KN_Actual <- data.frame(Init_DF_Valid_B0_F0$LONGITUDE, 
                            Init_DF_Valid_B0_F0$LATITUDE, 
  rep("actual"))

names(NDF_KN_Predicted) <- c("B0_F0_Longitude", "B0_F0_Latitude", 
                             "actual_Predicted")

names(NDF_KN_Actual) <- c("B0_F0_Longitude", "B0_F0_Latitude",
                          "actual_Predicted")

NDF_KNN <- rbind(NDF_KN_Predicted, NDF_KN_Actual)


# Visualize KNN result  

Visual_KNN_B0_F0 <- ggplot(NDF_KNN, aes(B0_F0_Latitude, B0_F0_Longitude,
                                 colour = actual_Predicted)) + 
  geom_point() + xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("KNN result for Building 0 and Floor 0")
