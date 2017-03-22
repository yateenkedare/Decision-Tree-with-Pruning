# Decision-Tree-with-Pruning
In this project i'm implimenting decision tree algorithm with post pruning in R.   
I have used 'rpart' library in order to create the tree model.  
I get approximately 98% accuracy when testing on the dummy testing dataset  
In order to prune I needed to find out the Complexity Parameter – CP from cross validation so that I can optimize the tree to its best.  
By running the command printcp(tree_model) I get the complexity parameter for all the splits  
I Use the prune commmand to prune the tree with the min complexity parameter  
