 ## ---------------------------
 ##
 ## Script name: caret fit RF
 ##
 ## Purpose of script:Like building a category model in the class, creating a regression model of mpg as target and others as features (from a built-in dataset of mtcars) using random forest algorithm with caret package, and writing the code.
 ##
 ## Author: Weishan Tu
 ##
 ## Date Created: 2024-03-29
 ##
 ## Copyright (c) Timothy Farewell, 2024
 ## Email: weishan@mail.ustc.edu.cn
 ##
 ## ---------------------------
 ##
 ## Notes:
 ## The code should include data preparation and pre-process, 
 # feature selection and visualization, training and turning the model, as
 # well as finally evaluating the performance of the model
 ##
 ## ---------------------------
 
 cat("\014") #clears rhe console
 rm(list=ls()) #remove all variales
 
 ## load up the packages we will need:  (uncomment as required)
 
 library(caret)

 ## ---------------------------
 # 1.	The code should include data preparation and pre-process, 
 # feature selection and visualization, training and turning the model, as
 # well as finally evaluating the performance of the model 
 
 # load mtcars
 data(mtcars)
 ######################
 ### 1 data preparation
 ######################
 
 # Structure of the dataframe
 str(mtcars)
 ##check have NA
 anyNA(mtcars)
 ##no NA; no categorical column
 
 # Create the training and test datasets
 set.seed(100)
 
 # Step 1: Get row numbers for the training data
 trainRowNumbers <- createDataPartition(mtcars$mpg, p=0.8, list=FALSE)
 
 # Step 2: Create the training  dataset
 trainData <- mtcars[trainRowNumbers,]
 
 # Step 3: Create the test dataset
 testData <- mtcars[-trainRowNumbers,]
 
 # Store X and Y for later use.
 x = trainData[, 2:11]
 y = trainData$mpg

 library(skimr)
 skimmed <- skim_to_wide(trainData)
 skimmed

 ######################
 #2 pre-process
 ######################
 ##check have NA
 # anyNA(trainData)
 # data have NA
 # # Create the knn imputation model on the training data
 # preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
 # preProcess_missingdata_model
 # # Use the imputation model to predict the values of missing data points
 # library(RANN)  # required for knnInpute
 # trainData <- predict(preProcess_missingdata_model, newdata = trainData)
 # anyNA(trainData)
 # data have categorical variable
 # # One-Hot Encoding
 # # Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
 # dummies_model <- dummyVars(Purchase ~ ., data=trainData)
 # 
 # # Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
 # trainData_mat <- predict(dummies_model, newdata = trainData)
 # 
 # # # Convert to dataframe
 # trainData <- data.frame(trainData_mat)
 # 
 # # See the structure of the new dataset
 # str(trainData)
 
 # preprocess to transform the data
 preProcess_range_model <- preProcess(trainData, method="range")
 trainData <- predict(preProcess_range_model, newdata = trainData)
 
 # Append the Y variable
 trainData$mpg <- y
 
 apply(trainData[, 2:11], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})
 
 ######################
 #3 visualization
 ######################
 featurePlot(x = trainData[, 2:11],
             y = trainData$mpg)
 ######################
 #4 feature selection
 ######################

 set.seed(100)
 options(warn=-1)
 
 subsets <- c(2:11)
 
 ctrl <- rfeControl(functions = rfFuncs,
                    method = "repeatedcv",
                    repeats = 5,
                    verbose = FALSE)
 
 lmProfile <- rfe(x=trainData[, 2:11], y=trainData$mpg,
                  sizes = subsets,
                  rfeControl = ctrl)
 
 lmProfile

 # The top 4 variables (out of 4):
 #   disp, wt, cyl, hp
 
 ## ####################
 #5 visualization AND turning the model
 ######################
 modelLookup('rf')
 # Define the training control
 fitControl <- trainControl(
   method = 'cv',                   # k-fold cross validation
   number = 5                      # number of folds
 ) 
 # Train the model using randomForest and predict on the training data itself.
 # Hyper Parameter Tuning using tuneLength
 model_cars = train(mpg ~ ., data=trainData, method='rf', 
                    tuneLength=5, trControl = fitControl)
 # model_cars
 print(model_cars)
 fitted <- predict(model_cars)
 plot(model_cars, main="Model Accuracies with MARS")

 varimp_mars <- varImp(model_cars)
 plot(varimp_mars, main="Variable Importance with MARS")

 # # Step 1: Impute missing values 
 # testData2 <- predict(preProcess_missingdata_model, testData)  
 # 
 # # Step 2: Create one-hot encodings (dummy variables)
 # testData3 <- predict(dummies_model, testData2)
 
 # Step 3: Transform the features to range between 0 and 1
 testData2 <- predict(preProcess_range_model, testData)
 
 # View
 head(testData2[, 1:11])
 
 # Predict on testData
 predicted <- predict(model_cars, testData2)
 head(predicted)
 ######################
 #6 evaluating the performance
 ######################
 observed <- testData$mpg
 names(observed) <- rownames(testData)
 # Measures for Regression
 postResample(pred = predicted, obs = observed)
 # RMSE  Rsquared       MAE 
 # 1.6668401 0.9448153 1.4822593 
 
 # Measures for Predicted Classes
 # 不适合mtcar预测mpg
 # Compute the confusion matrix
 confusionMatrix(reference = observed, data = predicted,mode='everything') # mode='everything', positive='MM'

  