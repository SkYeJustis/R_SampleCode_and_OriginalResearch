######################################################################
# Jawbone Data - Compare and select models
# Goal: Find the best model for predicting more REM sleep (over 30 minutes)
#       All models are default
# Author: Skyejustis
#
# Dataset: Author's Jawbone data
# Column descriptions here: https://jawbone.com/support/articles/000011483/historical-data
# Another multiple model selection ex: http://machinelearningmastery.com/compare-models-and-select-the-best-using-the-caret-r-package/
######################################################################

library(caret)

data = read.csv(file="2016_jawbone_edited_num.csv", header=T)
inTraining <- createDataPartition(data$Num.s_rem, p = .75, list = FALSE)
train = data[inTraining, ] 
test = data[-inTraining, ]

# Cross Validation setting to assess model performance on future unseen data
fitControl <- trainControl(method = "repeatedcv",
                           ## 10-fold CV
                           number = 10,
                           ## repeated ten times
                           repeats = 10)


formula = Num.s_rem ~ .

library(rpart)
modelCTREE <- train(formula, 
               data = train, 
               method = "ctree2",  
               trControl = fitControl
               )


modelKNN <- train(formula, 
                    data = train, 
                    method = "knn",  
                    trControl = fitControl
)

library(RWeka)
modelM5 <- train(formula, 
                  data = train, 
                  method = "M5",  
                  trControl = fitControl
)






library(randomForest)
modelrf <- train(formula, 
                 data = train, 
                 method = "rf",  
                 trControl = fitControl
)

modellm <- train(formula, 
                 data = train, 
                 method = "lm",  
                 trControl = fitControl
)


modelNN <- train(formula, 
                 data = train, 
                 method = "neuralnet",  
                 trControl = fitControl
)

library("pls")
modelPCR <- train(formula, 
                 data = train, 
                 method = "pcr",  
                 trControl = fitControl
)

options(scipen=999)

# collect resamples
results <- resamples(list(CTREE=modelCTREE, KNN=modelKNN, LM=modellm,
                          M5=modelM5, RForest=modelrf, NeuralN=modelNN,
                          PCR=modelPCR))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)


#Based on the models attempted, the model with the 
#lowest average RMSE (root mean squared error) and highest R^2 
#is the Neural Network model below.

plot(modelNN$finalModel)

#Next steps:
#Tweaking the parameters of the model
#   to see what the parameters for prediction would be.
#Use the 'test' set to check the model's 
#   performance on unseen data


