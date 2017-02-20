library(h2o)
h2oServer = h2o.init(nthreads = -1, max_mem_size = "7G")

train = h2o.importFile(path='otto/train.csv')
test = h2o.importFile(path='otto/test.csv')

indep = h2o.describe(train)["Label"]

indep = paste(indep[ 2:94 ,1], sep=",")

dep = "target"

rf =  h2o.randomForest(y = dep, 
                       x= indep,
                       training_frame = train,
                       ntrees = 1000,
                       nfolds = 10,
                       fold_assignment = "Stratified",
                       stopping_metric = "AUC",
                       mtries = 3,
                       max_depth=4,
                       seed=100)

h2o.performance(rf)

h2o.describe(test)

predict_rf = as.data.frame(h2o.predict(rf, test))

predict_rf$predict = NULL

predict_rf = as.matrix(predict_rf)

for (row in 1:nrow(predict_rf)) {
  #get MAX index and change to 1
  z = which(predict_rf[row,1:9] == max(predict_rf[row,1:9]), arr.ind = TRUE)
  predict_rf[row,z] = 1
  
  #get not MAX indexes and change to 0
  z = which(predict_rf[row,1:9] != max(predict_rf[row,1:9]), arr.ind = TRUE)
  predict_rf[row, c(z)] = 0
}

predict_rf = as.data.frame(predict_rf)
id = as.data.frame(test$id)

subm_rf = cbind(id, predict_rf)

write.csv(subm_rf, file = "h2o_rf_submission.csv", row.names = F)

