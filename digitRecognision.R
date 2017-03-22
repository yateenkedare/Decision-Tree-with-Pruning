library(tree)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
# get Data from from git directory
DigitRecog_data = read.csv(file="https://raw.githubusercontent.com/yateenkedare/Decision-Tree-with-Pruning/master/Data/DigitRecognition/optdigits_raining.csv",header = FALSE)

#Split training data in order to create a dummy test dataset
set.seed(5)
train = sample(1:nrow(DigitRecog_data), 0.8*nrow(DigitRecog_data))

training_data = DigitRecog_data[train, ]
testing_data = DigitRecog_data[-train, ]

#seperate features from target
#training_features = training_data[,c(1:64)]
#training_target = training_data[65]

#seperate testing features from testing target
#testing_features = testing_data[,c(1:64)]
testing_target = testing_data[65]

#fit a decision tree model using training data
tree_model = rpart(V65~., data = training_data, method = "class",parms = list(split = 'information'), 
                   minsplit = 2, minbucket = 1)
plot(tree_model)
text(tree_model, pretty = 8)

#Check how well the tree is doing using the dummy test data
tree_predict = c(predict(tree_model, testing_data, type = "class"))
mean(testing_target != tree_predict) #98.03% accurate

#Pruning the Tree
#first finding out the best pruning paramenters
#for this we'll find the complexity parameter 
printcp(tree_model) #will display complexity parameters for all splits 
plotcp(tree_model) #will plot a complexity parameter graph of relative error to complexity parameter
min_cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"] #will find out the min cp from above generated table

#now we prun our tree using the above values
pruned_tree_model <- prune(tree_model, cp= min_cp)
plot(pruned_tree_model)
text(pruned_tree_model, pretty = 8)

#testing the pruned tree with dummy test data
tree_predict = c(predict(pruned_tree_model, testing_data, type = "class"))
tree_predict = as.character(tree_predict)
mean(testing_target != tree_predict) #98.03% accurate

#testing using test data
DigitRecog_test_data = read.csv(file="https://raw.githubusercontent.com/yateenkedare/Decision-Tree-with-Pruning/master/Data/DigitRecognition/optdigits_test.csv",header = FALSE)
test_data_target = DigitRecog_test_data[65]
tree_predict = c(predict(pruned_tree_model, DigitRecog_test_data, type = "class"))
mean(test_data_target != tree_predict) #accuracy97.88%
DigitRecog_test_data$Output <- tree_predict
DigitRecog_test_data <- DigitRecog_test_data[, c(66, 1:65)]
write.csv(DigitRecog_test_data, file = "test_result.csv")