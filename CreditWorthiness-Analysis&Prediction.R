
#Install readxl package from CRAN and load it into the current R session for reading excel files
install.packages("readxl")
library(readxl)

#Install tidyverse package for interacting with data in a sophisticated manner
install.packages("tidyverse")
#Load the tidyverse package into the current R session
library(tidyverse)

#a)

#Exploratory Data Analysis
#Read the training and test data from excel file
trainSet <- read_xlsx("Credit_Risk6_final.xlsx",sheet=2)
testSet <- read_xlsx("Credit_Risk6_final.xlsx",sheet=1)

#Convert the tibbles into dataframes
trainSet <- data.frame(trainSet)
testSet <- data.frame(testSet)

#View the train and test dataframes
View(trainSet)
View(testSet)

#Find the column names in the training set which contains NA values
colnames(trainSet)[colSums(is.na(trainSet))>0]

#To check how many NA cells each of the above column contains
sum(is.na(trainSet$Housing))
sum(is.na(trainSet$Employment))
sum(is.na(trainSet$Personal.Status))

#Renaming Residence Time (In current district) column in training set as Residence Time to match with the test set
#column
colnames(trainSet)[colnames(trainSet)=="Residence.Time..In.current.district."] <- "Residence.Time"

#Going with mode imputation of the missing values in Employemnt, Housing and Personal Status columns, 
#I believe this won't introduce much bias in the model as it's only 4%,0.6% and 0.7% of the respective feature's data going to be imputed 

#User Defined Function for finding the mode of the variable
getMode <- function(vector) {
  uniqueValues <- unique(vector)
  uniqueValues[which.max(tabulate(match(vector, uniqueValues)))]
}

#Find the mode of the Employment column and print the same
(Employment.mode <- getMode(trainSet$Employment))
#Find the mode of the Housing column and print the same
(Housing.mode <- getMode(trainSet$Housing))
#Find the mode of the Personal Status column and print the same
(PersonalStatus.mode <- getMode(trainSet$Personal.Status))

#Assigning the mode values found above to missing value cells of respective columns  
trainSet$Employment[which(is.na(trainSet$Employment))] <- Employment.mode
trainSet$Housing[which(is.na(trainSet$Housing))] <- Housing.mode
trainSet$Personal.Status[which(is.na(trainSet$Personal.Status))] <- PersonalStatus.mode

factorColumns <- c("Checking.Acct","Credit.History","Loan.Reason","Savings.Acct","Employment","Personal.Status","Housing","Job.Type","Foreign.National","Credit.Standing")

#Factoring the above columns in Training Set
trainSet[factorColumns] <- lapply(trainSet[factorColumns], factor)

#Factoring the columns in the Testing set
testSet[factorColumns[factorColumns!="Credit.Standing"]] <- lapply(testSet[factorColumns[factorColumns!="Credit.Standing"]], factor)

#GBM only suits regression, so for classification convert the dependent variable to numeric as below (label encoding)
trainSet$Credit.Standing.Class <-as.numeric(trainSet$Credit.Standing) 
trainSet$Credit.Standing.Class <- trainSet$Credit.Standing.Class-1

#Now test the structure of the trainingset data frame
str(trainSet)

#Now test the structure of the testingset data frame
str(testSet)

#Analysing LoanReason,Employment Type and Credit Standing variable using ftable,it could be observed that
#People who got educational loan and are unemployed have a bad credit standing
threeVariableTable <- ftable(trainSet$Loan.Reason,trainSet$Employment,trainSet$Credit.Standing)
#Rounding off the decimals to 2 places
round(prop.table(threeVariableTable,margin = 1),2)

#Visualising the proportions of good and bad credit standing
#More than 50% of the customers in Kate's financial institution had only good loans
ggplot(trainSet, aes(Credit.Standing)) + geom_bar(aes(fill=Credit.Standing))

#Examining the relationship between Credit.Standing and Credit.History
windows(10,10)
#People having critical credit history hardly had a good credit standing
ggplot(trainSet, aes(Credit.History)) + geom_bar(aes(fill = Credit.Standing),position = "dodge")

#Examining the relationship between Loan.Reason and Credit.History
#Only people who have got loans for educational reason has a higher percentage of bad credit standing
ggplot(trainSet, aes(Loan.Reason)) + geom_bar(aes(fill = Credit.Standing),position = "dodge")

#Examining the relationship between Job.Type and Credit.History
#There is not a huge relationship between them as in all job types, the number of good and bad loans don't have
#a huge difference
ggplot(trainSet, aes(Job.Type)) + geom_bar(aes(fill = Credit.Standing),position = "dodge")

#Boxplot for Credit Standing against Residence time
#No relationship observed. There are only a few outliers in the plots
boxplot(trainSet$Residence.Time~trainSet$Credit.Standing) 

#Without outliers,let's see whether there is any difference among them
boxplot(trainSet$Residence.Time~trainSet$Credit.Standing,outline=F) 

#Boxplot for Credit Standing against Months.since.Checking.Acct.opened
#No relationship observed. There are only a few outliers in the plots
boxplot(trainSet$Months.since.Checking.Acct.opened~trainSet$Credit.Standing) 

#Without outliers,let's see whether there is any difference among them
boxplot(trainSet$Months.since.Checking.Acct.opened~trainSet$Credit.Standing,outline=F) 

#Boxplot for Credit Standing against Age
#No relationship observed. There are only a few outliers in the plots
boxplot(trainSet$Age~trainSet$Credit.Standing) 

#Without outliers,let's see whether there is any difference among them
boxplot(trainSet$Age~trainSet$Credit.Standing,outline=F) 

#Performing chi-square test to analyse the reltionship between Foreign National and Credit.Standing

#Null Hypothesis: No relationship exists between Foreign National and Credit Standing
#Alternate Hypothesis: Foreign National and Credit Standing has relationship between them

chisq.test(trainSet$Foreign.National,trainSet$Credit.Standing,correct = F)

#p-value=0.6939 which is greater than 0.05(5% significance level), so failed to reject the null hypothesis.
#Therefore, there is not enough evidence to say that there is a relationship between Foreign National and Credit Standing.
#Thus, there is no relationship between them


#Performing chi-square test to analyse the reltionship between Credit.History and Credit.Standing

#Null Hypothesis: No relationship exists between Credit.History and Credit Standing
#Alternate Hypothesis: Credit.History and Credit Standing has relationship between them

chisq.test(trainSet$Credit.History,trainSet$Credit.Standing,correct = F)

#p-value<2.2*10^-16 which is very less than 0.05(5% significance level), so rejecting the null hypothesis.
#Therefore, with 95% confidence, we could say that there is a strong relationship between Credit.History and Credit Standing

#Performing chi-square test to analyse the reltionship between Personal.Status and Credit.Standing

#Null Hypothesis: No relationship exists between Personal.Status and Credit Standing
#Alternate Hypothesis: Personal.Status and Credit Standing has relationship between them

chisq.test(trainSet$Personal.Status,trainSet$Credit.Standing,correct = F)

#p-value is 0.02822 which is lesser than 0.05,so rejecting the null hypothesis but the significance is poor.

#Performing chi-square test to analyse the reltionship between Housing and Credit.Standing

#Null Hypothesis: No relationship exists between Housing and Credit Standing
#Alternate Hypothesis: Housing and Credit Standing has relationship between them

chisq.test(trainSet$Housing,trainSet$Credit.Standing,correct = F)

#p-value is 0.0099 which is lesser than 0.05,so rejecting the null hypothesis but the significance is poor.


#With the help of all the Exploratory Data Analysis done above, we can observe that, Residence.Time,Foreign National,Months.Since.Checking.Acct.Opened,
#Personal.Status,Job.Type,Housing and Age are not so significant in predicting the result. So,this could introduce noise in the model if included which might lead to
#low bias(more accuracy in training) and high variance(bad predictions with unseen data) in the model i.e., overfitting
#Thus, removing those features while building the model

# b) Decision Tree

#Decision tree is a simple but easily interpretable machine learning model
#The tree starts splitting from the root node (most important node) and further splits into the internal nodes
#until it reaches the leaf nodes or label nodes. The nodes chosen for splitting could be found either using 
#information gain or gini index.
#The variable having the higest information gain is selected as the root node. Info Gain is found by subtracting the 
#entropy (impurity or uncertainity) of the independent variable from the entropy of the target variable
#Lower the probablity of a variable, higher the entropy and higher its probability, lesser the entropy

#installing the rpart package for fitting a Classification Tree
install.packages("rpart")
library(rpart)

#installing caret package for implementing cross-fold validation in the rpart model
install.packages("caret")
library(caret)

?rpart

#Our baseline rate is 59%.We need to find whether our model performs better than this
table(trainSet$Credit.Standing)/nrow(trainSet)

#Split the training data into train and validation for checking the accuracy (70:30 split)
#Setting the seed to get reproducible results (seed value - last three digits of my ID number)
set.seed(599)
train=sample(1:nrow(trainSet),546)

#Buiding the decision tree initially with train data as the value for subset parameter.
#Removing the ID,Residence.Time,Job.Type,Age,Personal.Status,Housing,Foreign.National,Months.Since.Checking.Acct.Opened and Credit.Standing.Class columns
#in model building as it isn't influential in decision making
decisionTreeModel <- rpart(Credit.Standing~ Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment,trainSet,subset = train,method="class")
#Printing the decision tree built
print(decisionTreeModel)
windows(10,10)
#To look the decision tree visually
plot(decisionTreeModel,margin = 0.1)
#To see the decision tree with text labels
text(decisionTreeModel,pretty = 1)
#To get the overall summary of the decision tree built
#Credit History is the root node(most important variable) and then Employment,Age,Loan Reason and Savings Account are
#the other important variables
summary(decisionTreeModel)

#Predict the class variable for the validation set with the decision tree built above
prediction <- predict(decisionTreeModel,trainSet[-train,],type = "class")
#View the prediction results
prediction
#The accuracy is (122+44)/234=0.709 i.e., 70.9%
table(prediction,trainSet[-train,]$Credit.Standing)

#Let's check if the accuracy of the model is increased if we tune the parameters. With Cross-Fold
#validation, we will check which tree size has the minimum deviance of error and prune the tree
#supplying the best parameter obtained.

#Fine Tuning the parameters using cross fold validation
set.seed(599)

#Number of Folds chosen is 10 and repeated 3 times
#Cross-fold validation is used to avoid model overfitting and improvise the accuracy
crossValidationControl <-trainControl(method = "repeatedcv",number = 10,repeats = 3)
#running the cross validations with the above set paameters
crossValidationTree <- train(Credit.Standing~ Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment, 
                  data = trainSet[train,],
                  method = "rpart",
                  trControl = crossValidationControl,
                  tuneLength = 15)
#Best Tree would be selected with the maximum accuracy obtained for the training set
crossValidationTree

#predicting on cross validated tree
tree.pred<-predict(crossValidationTree,trainSet[-train,],type="raw")

#The accuracy is now 72.6% (126+44)/234 for the validation set after cross validating.
table(tree.pred,trainSet[-train,]$Credit.Standing)

#c)

#So, now predicting the class for the actual scoring or test data using the cross-validated model built.
#We can be 72.6% sure that the model predicts correctly whether the manager should consider the new
#customers good or bad prospective loans
prediction <- predict(crossValidationTree,testSet,type = "raw")
#View the prediction results
prediction

#We take 5 records to explain to Kate, how the decision tree works and predicts the result for the unseen data

#In General, Decision Tree is a basic conditional tree which predicts the class of the record by parsing down the 
#conditions among the attributes of that record. If the condition is evaluated true, the tree parse down the left and if the
#condition is false, the tree parses down the right. 

#1 For ID 781,first the decision tree checks for the root node whether the customer's credit history is critical or not.
#In this case it's not critical (i.e., false), so the tree goes down the right hand side and checks whether the credit history
#is within Current,delay or not. In our case, it's not (false),so the decision tree goes down the right hand side 
#and predicts this as a Good Loan

#2 For ID 791,the decision tree checks for the root node whether the customer's credit history is critical or not.
#In this case it's critical (i.e., true), so the decision tree goes down the left hand side 
#and predicts this as a Bad Loan

#3 Again, For ID 792,the decision tree checks for the root node whether the customer's credit history is critical or not.
#In this case it's critical (i.e., true), so the decision tree goes down the left hand side 
#and predicts this as a Bad Loan

#4 For ID 788,first the decision tree checks for the root node whether the customer's credit history is critical or not.
#In this case it's not critical (i.e., false), so the tree goes down the right hand side and checks whether the credit history
#is within Current,delay or not. In our case, it's not (false),so the decision tree goes down the right hand side 
#and predicts this as a Good Loan

#5 For ID 793,first the decision tree checks for the root node whether the customer's credit history is critical or not.
#In this case it's not critical (i.e., false), so the tree goes down the right hand side and checks whether the credit history
#is within Current,delay or not. In our case, it's Current (true),so the decision tree goes down the left hand side 
#and checks whether the Employment is short or not, in our case, it's true (Short), so it goes down the left hand side
#and predicts this as a Bad Loan

#The accuracies of these being a good/bad loan is calculated by the use of constructing a confusion matrix.
#The accuracy is calculated by dividing the sum of True Positives and True negatives by the sum of True
#Positives,False Positives,False Negatives and True Negatives.

#True Positive - Actual and predicted results are the same where the results are positive like Good,Yes...
#True Negative - Actual and predicted results are the same where the results are negative like Bad,No...
#False Positive - Actual and predicted results are different where the actual result is negative but the
#predicted result is positive 
#False Negative - Actual and predicted results are different where the actual result is positive but the
#predicted result is negative. This could be more dangerous

#In our case, TP=126,TN=44,FP=23 and FN=41, so accuracy=(126+44)/(126+23+41+44)=170/234=72.64%

#d) 

#Trying to improve my model performance using Random Forest (Bagging), GBM, AdaBoosting
#(Boosting)

#Random Forest - Ensemble technique (combination of two or more models) where independent trees are grown and 
#the prediction results are averaged in case of regression problem or majority voting is used for classification problems
install.packages("randomForest")
library(randomForest)

set.seed(599)

#For getting the best value of mtry at which the accuracy is maximum
#Creating an array which contains accuracies of the model at different values of mtry
accuracies=c()
#Loop for running the random forest model for mtry values 2 to 5 to find which suits best
for(i in 2:6)
{
set.seed(599)
randomForestModel <- randomForest(Credit.Standing~ Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment,data=trainSet[train,],mtry = i,  
                                  importance=T)
randomforestPrediction <- predict(randomForestModel,trainSet[-train,],type="class")
accuracies[i-1] <- mean(randomforestPrediction==trainSet[-train,]$Credit.Standing)
}
#We can see that the model best performs when mtry is 2
accuracies

#Taking the optimized mtry value and building the randomforest model
set.seed(599)
randomForestModel <- randomForest(Credit.Standing~ Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment,data=trainSet[train,],mtry = 2,  
                                  importance=T)
randomForestModel

#Check the variable importance score
importance(randomForestModel)

#Run the prediction for the validation set
randomforestPrediction <- predict(randomForestModel,trainSet[-train,],type="response")
randomforestPrediction

#Accuracy is (116+57)/234=73.9 percent on the validation set which is better than decision tree which was 72.6%
table(randomforestPrediction,trainSet[-train,]$Credit.Standing)

#Boosting

#Using AdaBoost - Ensemble technique where trees (weak learners) are grown sequentially one after the other giving
#more weight to the misclassified data points from the preceeding trees and reducing the misclassification rate
#to get a strong learner after rounds of iterations.
install.packages("adabag")
library(adabag)

set.seed(599)
#Run the cross validation
#v is for number of folds of cross validation
Loan.adabag.cv <- boosting.cv(Credit.Standing~ Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment, data = trainSet[train,],v=18, boos = TRUE, mfinal=8,control=rpart.control(maxdepth=5))
Loan.adabag.cv
# When tuning parameters is finished, check on test set; here best parameters are mfinal =30, maxdepth = 5.
Loan.adabag <- boosting(Credit.Standing~ Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment, data = trainSet[train,], boos = TRUE, mfinal=30, control=rpart.control(maxdepth=5) )
Loan.adabag

#Run the prediction on the validation set
prediction.adabag=predict(Loan.adabag,newdata=trainSet[-train,],type = "response")

prediction.adabag #73.5% (122+50)/234 accuracy on validation set, less than GBM & RandomForest

#Gradient Boosting Machine
#GBM boosting algorithm tries to find the minimum loss function using gradient descent
#It uses the learning rate which when given a higher value might skip the minima or when given a very lower value,
#might increase the accuracy by exactly finding the minima but might a very long time to find that thereby
#increasing the time taken to build the model
install.packages("gbm")
library(gbm)

set.seed(599)

train_gbm=sample(1:nrow(trainSet),nrow(trainSet))
trainset_gbm=trainSet[train_gbm,]

#Grid Search for Hypertuning of parameters
#Grid search is one of the method for hypertuning of algorithm parameters. It runs the algorithm through all the 
#combinations of given parameters in the search space from which we could get the best possible parameters 
#which increases the accuracy of our model
gridSearch_gbm <- expand.grid(
  shrinkage = c(.01, .001, .0001), #learning rate
  interaction.depth = c(1, 3, 5),  #maximum depth of each tree
  n.minobsinnode = c(5, 10, 15),   #minimum no of observations in the leaf nodes
  bag.fraction = c(.5, .7, 1),     #fraction of the training set observations selected for the consequent tree 
  optimal_trees = 0,               #To find the optimal tree
  min_RMSE = 0                     #To find the minimum Root Mean Square Error
)

for(i in 1:nrow(gridSearch_gbm)) {
  
  # reproducibility
  set.seed(599)
  
  # train model
  gbm.tune <- gbm(
    formula = Credit.Standing.Class~ Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment,
    distribution = "gaussian", #same as bernoulli
    data = trainset_gbm,
    n.trees = 5000, # Number of Trees
    interaction.depth = gridSearch_gbm$interaction.depth[i], #tree depth
    shrinkage = gridSearch_gbm$shrinkage[i],  #Learning Rate 
    n.minobsinnode = gridSearch_gbm$n.minobsinnode[i],
    bag.fraction = gridSearch_gbm$bag.fraction[i], 
    train.fraction = 0.7, #70% of whole training data used for model building
    n.cores = NULL, # will use all cores by default
    verbose = FALSE #Not to print progress indicators
  )
  
  # add min training error and trees to grid
  gridSearch_gbm$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  gridSearch_gbm$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

#Get the top 5 best parameter combinations for gbm (in terms of minimum RMSE) from grid search results
gridSearch_gbm %>% 
  arrange(min_RMSE) %>%
  head(5)

set.seed(599)

#The best parameter combination found in grid search is learning rate at 0.01, tree depth at 3, no.of trees at 1938
#and all the data is taken from the given dataset (bag.fraction is 1)
loan.gbmBooster=gbm(Credit.Standing.Class~ Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment,data=trainSet[train,],distribution="bernoulli",n.trees=1938,
                    interaction.depth=3,bag.fraction = 1, shrinkage = 0.01)
summary(loan.gbmBooster)

par(mfrow=c(1,2))

#Credit.History and Loan.Reason have high relative importance.
#i.e.,Higher the proportion of Credit.History being all paid,the higher it is classified as good loan and the higher it is critical,
#the higher it is classified as bad loan. 
plot(loan.gbmBooster,i="Credit.History")
#Higher the loan reason is for education, the higher it is highly classified as bad whereas the higher it's for retraining the 
#higher it is classified as good
plot(loan.gbmBooster,i="Loan.Reason")

#Run the prediction for the validation set
gbm.prediction=predict(loan.gbmBooster,newdata=trainSet[-train,],n.trees=1938,type = "response")
gbm.prediction

#Run the prediction for the scoring set
gbm.prediction=predict(loan.gbmBooster,newdata=testSet,n.trees=1938,type = "response")

#Histogram to visualize the prediction results
hist(gbm.prediction)

#Do label decoding 
predict_class <- ifelse(gbm.prediction<0.5,"Bad","Good")
predict_class

#Confusion matrix for validation set
table(predict_class,trainSet[-train,]$Credit.Standing)
(126+53)/234 #76.5% accuracy i.e, 2.6 % more than random forest and 3% more than Adabag


### With Decision Tree (72.6%), RandomForest (73.9%), Adabag (73.5%) and Gradient Boosting (76.5%) models built,validated 
### and hyper tuned the parameters, GBM stands out to be the better model amongst them


#e)

#To find the suspiciously incorrect pattern in the training set data which contains the details of the 
#customer's loan, let's apply the best model obtained above for the training set and predict the labels
#and compare the results with the actual labels.

best.prediction=predict(loan.gbmBooster,newdata=trainSet,n.trees=1938,type = "response")
best.prediction

#Assign class labels by means of the threshold
best.predict_class <- ifelse(best.prediction<0.5,"Bad","Good")

#Here we are getting 633 values correctly classified in the training set out of 780 records
#So,around 147 records have suspiciously incorrect pattern.
table(best.predict_class,trainSet$Credit.Standing)

mismatch.vectors <- NULL

#To Create a vector which contains the IDs' of the 147 mismatching records
#If the predicted label and the actual label doesn't match, then that record's ID will be appended into the mismatch vector
for(row in 1:nrow(trainSet))
{
  if(trainSet[row,]$Credit.Standing!=best.predict_class[row])
  {
    mismatch.vectors <- c(mismatch.vectors,trainSet[row,]$ID)
  }  
}

#To see, where we are getting breaks in consecutive IDs'
Groups <- c(0, which(diff(mismatch.vectors) != 1), length(mismatch.vectors))

Groups

#To get the actual groups of consecutive IDs'  
result <- sapply(seq(length(Groups) - 1),function(i) mismatch.vectors[(Groups[i] + 1):Groups[i+1]])

#To print the consecutive customer IDs falling in the maximum range (here 8)
cat("Suspiciously Incorrect Pattern found in these 8 consecutive customer ID's:",result[[which.max(lengths(result))]])

       