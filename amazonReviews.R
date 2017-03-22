library(plyr)
library(twitteR)
library(stringr)
library(tree)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
#function to clean data
cleanTweets = function(tweets)
{
  tweets_cl = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",tweets)
  tweets_cl = gsub("http[^[:blank:]]+", "", tweets_cl)
  tweets_cl = gsub("@\\w+", "", tweets_cl)
  tweets_cl = gsub("[ \t]{2,}", "", tweets_cl)
  tweets_cl = gsub("^\\s+|\\s+$", "", tweets_cl)
  tweets_cl = gsub("[[:punct:]]", " ", tweets_cl)
  tweets_cl = gsub("[^[:alnum:]]", " ", tweets_cl)
  tweets_cl <- gsub('\\d+', '', tweets_cl)
  return(tweets_cl)
}
#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word
    sentence = cleanTweets(sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}

#get the data
AmazonData <- read.csv('amazon_baby_train.csv', header = TRUE)
AmazonTest <- read.csv('amazon_baby_test.csv',header = TRUE)
reviews = as.character(AmazonData$review)
reviews_test = as.character(AmazonTest$review)


#load pos,neg statements
afinn_list <- read.delim(file='https://raw.githubusercontent.com/iHub/sentiment-analysis-using-R/master/data/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)
#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")   




#Calculate score on each review and make Train_data
SentiResult_train <- as.data.frame(sentimentScore(reviews, vNegTerms, negTerms, posTerms, vPosTerms))
SentiResult_train$'2' = as.numeric(SentiResult_train$'2')
SentiResult_train$'3' = as.numeric(SentiResult_train$'3')
SentiResult_train$'4' = as.numeric(SentiResult_train$'4')
SentiResult_train$'5' = as.numeric(SentiResult_train$'5')
counts = c(sum(SentiResult_train$'2')/140,sum(SentiResult_train$'3')/140,(sum(SentiResult_train$'4')+sum(SentiResult_train$'5'))/140)
names = c("Negative","Neutral","Positive")
mr = list(counts,names)
colors = c("red", "yellow", "green")
barplot(mr[[1]], main="Amazon Reviews", xlab="Ratings",legend=mr[[2]],col=colors)

train_X = ifelse(SentiResult_train$'2' < SentiResult_train$'3', '3','1')
train_X = ifelse(SentiResult_train$'3' < SentiResult_train$'4', '5',train_X)
train_X = ifelse(SentiResult_train$'4' < SentiResult_train$'5', '5',train_X)

Train_data = cbind(AmazonData$rating, train_X)
Train_data = data.frame(Train_data)

#Calculate score on each review and make test Data
SentiResult_test <- as.data.frame(sentimentScore(reviews_test, vNegTerms, negTerms, posTerms, vPosTerms))
SentiResult_test$'2' = as.numeric(SentiResult_test$'2')
SentiResult_test$'3' = as.numeric(SentiResult_test$'3')
SentiResult_test$'4' = as.numeric(SentiResult_test$'4')
SentiResult_test$'5' = as.numeric(SentiResult_test$'5')

test_Y = ifelse(SentiResult_test$'2' < SentiResult_test$'3', '3','1')
test_Y = ifelse(SentiResult_test$'3' < SentiResult_test$'4', '5',test_Y)
test_Y = ifelse(SentiResult_test$'4' < SentiResult_test$'5', '5',test_Y)

Test_data = cbind(AmazonTest$rating, test_Y)
Test_data = data.frame(Test_data)
###Decision Tree and Prediction###############3
# get Data from from git directory


#Split training data in order to create a dummy test dataset
set.seed(5)
train = sample(1:nrow(Train_data), 0.8*nrow(Train_data))

training_data = Train_data[train, ]
testing_data = Train_data[-train, ]

#seperate features from target
#training_features = training_data[,c(1:64)]
#training_target = training_data[65]

#seperate testing features from testing target
#testing_features = testing_data[,c(1:64)]
testing_target = testing_data[1]

#fit a decision tree model using training data
tree_model = rpart(V1~., data = training_data, method = "class",parms = list(split = 'information'), 
                   minsplit = 2, minbucket = 1)

#Check how well the tree is doing using the dummy test data
tree_predict = c(predict(tree_model, testing_data, type = "class"))
mean(testing_target != tree_predict) #41.52

#Pruning the Tree
#first finding out the best pruning paramenters
#for this we'll find the complexity parameter 
printcp(tree_model) #will display complexity parameters for all splits 
plotcp(tree_model) #will plot a complexity parameter graph of relative error to complexity parameter
min_cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"] #will find out the min cp from above generated table

#now we prun our tree using the above values
pruned_tree_model <- prune(tree_model, cp= min_cp)

#testing the pruned tree with dummy test data
tree_predict = c(predict(pruned_tree_model, testing_data, type = "class"))
mean(testing_target != tree_predict) #98.03% accurate

#testing using test data
test_data_target = Test_data[1]
tree_predict = c(predict(pruned_tree_model, Test_data, type = "class"))
mean(test_data_target != tree_predict) #41.58
AmazonTest$Output <- tree_predict
AmazonTest <- AmazonTest[ , c(4, 1:3)]
write.csv(AmazonTest, file = "test_result.csv")