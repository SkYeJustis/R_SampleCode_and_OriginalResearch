library(h2o)
h2oServer = h2o.init(nthreads = -1, max_mem_size = "7G")

train = h2o.importFile(path='AllState_Claims/train.csv')
test = h2o.importFile(path='AllState_Claims/test.csv')

dim(train)
# Rows: 188318    Cols: 132

h2o.describe(train) 
# Obtain numeric and categorical variables list

numericVar = h2o.describe(train)[c("Label", "Type")]
categVar = h2o.describe(train)[c("Label", "Type")]

#Add row numbers these lists of variables
numericVar$row = as.numeric(rownames(numericVar))
categVar$row = as.numeric(rownames(categVar))

# Define the list into a list of: Numeric Variables and Categorical Variables
numericVar = subset(numericVar, subset = Type == "real")
categVar = subset(categVar, subset = Type == "enum")

#View the list of variables that will be manipulated later
paste(categVar[1:116, 1], sep=",")
paste(numericVar[1:14, 1], sep=",")

# Close h2o instance to save RAM space
h2o.shutdown()

#######################################################
# Load data for Principal Components Analysis creation 
#######################################################
library(data.table)

train = fread('AllState_Claims/train.csv', header = TRUE)
test = fread('AllState_Claims/test.csv', header = TRUE)

# Conversions to ensure that data is of appropriate type in train and test data 
# Categorical data should be factors
# Numeric data should be numeric

row = NULL
for (row in 1:nrow(categVar)) {
  train[[categVar[row,3]]] = as.factor(train[[categVar[row,3]]])
}
row = NULL
for (row in 1:nrow(numericVar)) {
  train[[numericVar[row,3]]] = as.numeric(train[[numericVar[row,3]]])
}
row = NULL
for (row in 1:nrow(categVar)) {
  test[[categVar[row,3]]] = as.factor(test[[categVar[row,3]]])
}
row = NULL
for (row in 1:(nrow(numericVar)-1) ) { #Exclude the numeric target variable that was in the train set 
  test[[numericVar[row,3]]] = as.numeric(test[[numericVar[row,3]]])
}

# Ensure that train and test data are still data.frames
train = as.data.frame(train)
test = as.data.frame(test)



library(reshape2)
library(ggplot2)

trainNum = NULL

trainNum = melt(train[,-c(categVar[1:14, 1])])
ggplot(trainNum,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()

summary(train)

# Save the id columns for creating the submission file
trainid = as.data.frame(train$id)
testid = as.data.frame(test$id)

####################################
# Principal Components Analysis
####################################
library(dummies) # Turn categorical variables into numerics

test$loss = 1 #Account for the predictor variable to create PCA features

combine = rbind(train, test)


#create a dummy data frame
pca.train <- subset(train, select = -c(id))



# Combined data - train and test  - to create an numeric version of cateogrical variables
cbm = dummy.data.frame(combine, 
                       names = c(paste(categVar[1, 1], sep=","),
                                 paste(categVar[2, 1], sep=","),
                                 paste(categVar[3, 1], sep=","),
                                 paste(categVar[4, 1], sep=","),
                                 paste(categVar[5, 1], sep=","),
                                 paste(categVar[6, 1], sep=","),
                                 paste(categVar[7, 1], sep=","),
                                 paste(categVar[8, 1], sep=","),
                                 paste(categVar[9, 1], sep=","),
                                 paste(categVar[10, 1], sep=","),
                                 
                                 paste(categVar[11, 1], sep=","),
                                 paste(categVar[12, 1], sep=","),
                                 paste(categVar[13, 1], sep=","),
                                 paste(categVar[14, 1], sep=","),
                                 paste(categVar[15, 1], sep=","),
                                 paste(categVar[16, 1], sep=","),
                                 paste(categVar[17, 1], sep=","),
                                 paste(categVar[18, 1], sep=","),
                                 paste(categVar[19, 1], sep=","),
                                 paste(categVar[20, 1], sep=","),
                                 
                                 paste(categVar[21, 1], sep=","),
                                 paste(categVar[22, 1], sep=","),
                                 paste(categVar[23, 1], sep=","),
                                 paste(categVar[24, 1], sep=","),
                                 paste(categVar[25, 1], sep=","),
                                 paste(categVar[26, 1], sep=","),
                                 paste(categVar[27, 1], sep=","),
                                 paste(categVar[28, 1], sep=","),
                                 paste(categVar[29, 1], sep=","),
                                 paste(categVar[30, 1], sep=","),
                                 
                                 paste(categVar[31, 1], sep=","),
                                 paste(categVar[32, 1], sep=","),
                                 paste(categVar[33, 1], sep=","),
                                 paste(categVar[34, 1], sep=","),
                                 paste(categVar[35, 1], sep=","),
                                 paste(categVar[36, 1], sep=","),
                                 paste(categVar[37, 1], sep=","),
                                 paste(categVar[38, 1], sep=","),
                                 paste(categVar[39, 1], sep=","),
                                 paste(categVar[40, 1], sep=","),
                                 
                                 paste(categVar[41, 1], sep=","),
                                 paste(categVar[42, 1], sep=","),
                                 paste(categVar[43, 1], sep=","),
                                 paste(categVar[44, 1], sep=","),
                                 paste(categVar[45, 1], sep=","),
                                 paste(categVar[46, 1], sep=","),
                                 paste(categVar[47, 1], sep=","),
                                 paste(categVar[48, 1], sep=","),
                                 paste(categVar[49, 1], sep=","),
                                 paste(categVar[50, 1], sep=","),
                                 
                                 paste(categVar[51, 1], sep=","),
                                 paste(categVar[52, 1], sep=","),
                                 paste(categVar[53, 1], sep=","),
                                 paste(categVar[54, 1], sep=","),
                                 paste(categVar[55, 1], sep=","),
                                 paste(categVar[56, 1], sep=","),
                                 paste(categVar[57, 1], sep=","),
                                 paste(categVar[58, 1], sep=","),
                                 paste(categVar[59, 1], sep=","),
                                 paste(categVar[60, 1], sep=","),
                                 
                                 paste(categVar[61, 1], sep=","),
                                 paste(categVar[62, 1], sep=","),
                                 paste(categVar[63, 1], sep=","),
                                 paste(categVar[64, 1], sep=","),
                                 paste(categVar[65, 1], sep=","),
                                 paste(categVar[66, 1], sep=","),
                                 paste(categVar[67, 1], sep=","),
                                 paste(categVar[68, 1], sep=","),
                                 paste(categVar[69, 1], sep=","),
                                 paste(categVar[70, 1], sep=","),
                                 
                                 paste(categVar[71, 1], sep=","),
                                 paste(categVar[72, 1], sep=","),
                                 paste(categVar[73, 1], sep=","),
                                 paste(categVar[74, 1], sep=","),
                                 paste(categVar[75, 1], sep=","),
                                 paste(categVar[76, 1], sep=","),
                                 paste(categVar[77, 1], sep=","),
                                 paste(categVar[78, 1], sep=","),
                                 paste(categVar[79, 1], sep=","),
                                 paste(categVar[80, 1], sep=","),
                                 
                                 paste(categVar[81, 1], sep=","),
                                 paste(categVar[82, 1], sep=","),
                                 paste(categVar[83, 1], sep=","),
                                 paste(categVar[84, 1], sep=","),
                                 paste(categVar[85, 1], sep=","),
                                 paste(categVar[86, 1], sep=","),
                                 paste(categVar[87, 1], sep=","),
                                 paste(categVar[88, 1], sep=","),
                                 paste(categVar[89, 1], sep=","),
                                 paste(categVar[90, 1], sep=","),
                                 
                                 paste(categVar[91, 1], sep=","),
                                 paste(categVar[92, 1], sep=","),
                                 paste(categVar[93, 1], sep=","),
                                 paste(categVar[94, 1], sep=","),
                                 paste(categVar[95, 1], sep=","),
                                 paste(categVar[96, 1], sep=","),
                                 paste(categVar[97, 1], sep=","),
                                 paste(categVar[98, 1], sep=","),
                                 paste(categVar[99, 1], sep=","),
                                 paste(categVar[100, 1], sep=","),
                                 
                                 paste(categVar[101, 1], sep=","),
                                 paste(categVar[102, 1], sep=","),
                                 paste(categVar[103, 1], sep=","),
                                 paste(categVar[104, 1], sep=","),
                                 paste(categVar[105, 1], sep=","),
                                 paste(categVar[106, 1], sep=","),
                                 paste(categVar[107, 1], sep=","),
                                 paste(categVar[108, 1], sep=","),
                                 paste(categVar[109, 1], sep=","),
                                 paste(categVar[110, 1], sep=","),
                                 
                                 paste(categVar[111, 1], sep=","),
                                 paste(categVar[112, 1], sep=","),
                                 paste(categVar[113, 1], sep=","),
                                 paste(categVar[114, 1], sep=","),
                                 paste(categVar[115, 1], sep=","),
                                 paste(categVar[116, 1], sep=",")
                       ))


str(combine, list.len=ncol(combine))
str(cbm, list.len=ncol(cbm))
summary(cbm)

#Save RAM space
rm(cbm)
rm(combine)


#Divide the new data
pca.train = cbm[1:nrow(train),]
pca.test = cbm[-(1:nrow(train)),]

str(pca.train, list.len=ncol(pca.train))
str(pca.test, list.len=ncol(pca.test))

pca.train$id = NULL
pca.train$loss = NULL

#principal component analysis
prin_comp = prcomp(pca.train, center = T)
names(prin_comp)

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

#compute standard deviation of each principal component
std_dev = prin_comp$sdev

#compute variance
pr_var = std_dev^2

#proportion of variance explained
prop_varex = pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


#add a training set with principal components
train.data = data.frame(id = train$id, 
                        prin_comp$x)

#we are interested in first 102 PCAs
train.data <- train.data[,1:101]

str(train.data, list.len=ncol(train.data))

##############################################
## H2o Gradient Boosted Machine (GBM)       ##
##############################################
library(h2o)
h2oServer = h2o.init(nthreads = -1, max_mem_size = "7G")

train = h2o.importFile(path='AllState_Claims/train.csv')
test = h2o.importFile(path='AllState_Claims/test.csv')

pca.train  = as.data.frame(pca.train)
colnames(pca.train)
as.h2o(pca.train)

pcaTrain = cbind(train$loss, pca.train) 
colnames(pcaTrain)
colnames(pcaTrain)[1] = "loss"

indep = h2o.describe(pcaTrain)["Label"]
dep = "loss"

# 102 PCA variables
indep = paste(indep[2:104,1], sep=",")


gbm = h2o.gbm(x = indep,
              y = dep,
              training_frame = pcaTrain)

gbm.performance()


h2o.saveModel(gbm, path = "AllState_Claims/", force = TRUE)


id = as.data.frame(test$id)
predict_gbm = as.data.frame(h2o.predict(gbm, test))

sub_nn = cbind(id, predict_gbm)

colnames(sub_gbm) = c("id", "loss")

write.csv(sub_gbm, file = "AllState_Claims/sub_gbm.csv", row.names = F)


