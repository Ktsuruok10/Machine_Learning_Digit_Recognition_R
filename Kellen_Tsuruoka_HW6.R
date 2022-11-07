##Kellen Tsuruoka HW6 
##
#Reading in the train data set from kaggle
#I'm going to split this data set into training/testing
#to save computing power...
##
library(ggplot2)
library(e1071)
library(caret)

# Reading in training data
digit_data <- read.csv("Downloads/train.csv")


# creating histogram with the frequency of numbers 
hist(digit_data$label)

# creating ggplot bar graph to show frequency of numbers in data
ggplot(digit_data) + geom_bar(aes(x=label), fill='blue') +xlab('Digits') +scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle('Full Data Distribution')  

## Creating ggplot bar graph to show freqency of numbers in training data
ggplot(Digit_Train) + geom_bar(aes(x=label), fill='dark green') +xlab('Digits') + ggtitle("Training Data Distribution")


#Separating data into training and testing set 
X = 3   ## This will create a 1/3, 2/3 split.
## Of course, X can be any number.
(every_X_index<-seq(1,nrow(digit_data),X))

## Use these X indices to make the Testing and then
## Training sets:

(Digit_Test<-digit_data[every_X_index, ])
(Digit_Train<-digit_data[-every_X_index, ])


## Make sure label is factor type
str(Digit_Test)

## Changing labels to factor

Digit_Train$label <- as.factor(Digit_Train$label)
Digit_Test$label <- as.factor(Digit_Test$label)

## Double checking label is a factor
str(Digit_Train$label)
str(Digit_Test$label)

## Copy the Labels

(Test_Labels <- Digit_Test[,1])
str(Test_Labels)
## Remove the labels
(Digit_Test_NO_LABEL <- Digit_Test[,-c(1)])

Digit_Test_NO_LABEL


## running Naive Bayes Model from e1071 package
(NB_e1071_2<-naiveBayes(Digit_Train, Digit_Train$label, laplace = 1))
NB_e1071_Pred <- predict(NB_e1071_2, Digit_Test_NO_LABEL)#NB_e1071_2
NBT1 <- table(NB_e1071_Pred,Test_Labels)
confusionMatrix(NBT1)
(NB_e1071_Pred)


plot(NB_e1071_Pred)

library(rpart)
library(rpart.plot)
library(rattle)

## Running the decision tree model 
Digit_tree1 <- rpart(Digit_Train$label ~., data = Digit_Train, method="class")
summary(Digit_tree1) 

## Text--------------------------------------
predictedTree1 = predict(Digit_tree1, Digit_Test_NO_LABEL, type="class")
DTT1 <- table(unlist(predictedTree1) ,unlist(Test_Labels))
confusionMatrix(DTT1)
## VIS..................
fancyRpartPlot(Digit_tree1)


## Testing another Naive Bayes Model to see if the accuracy improved 
library(naivebayes)
NB_object <- naive_bayes(Digit_Train$label ~., data=Digit_Train, laplace = 2)
NB_object
NB_prediction<-predict(NB_object, Digit_Test_NO_LABEL)
#NB_prediction
#head(predict(NB_object, DF_Test_NO_LABEL))
NB1 <- table(NB_prediction,Test_Labels)
confusionMatrix(NB1)
plot(NB_prediction)


## Running the decision tree model2 with different parameters
control <- rpart.control(minsplit = 5,maxdepth = 3,cp = 0.01)

Digit_tree2 <- rpart(Digit_Train$label ~., data = Digit_Train, method="class",control = control)
summary(Digit_tree2) 

## Decision Tree Prediction
predictedTree2 = predict(Digit_tree2, Digit_Test_NO_LABEL, type="class")
DTT2 <- table(unlist(predictedTree2) ,unlist(Test_Labels))
confusionMatrix(DTT2)
## VIS..................
fancyRpartPlot(Digit_tree2)


## Running the decision tree model3 with different parameters
control2 <- rpart.control(cp = 0.05)

Digit_tree3 <- rpart(Digit_Train$label ~., data = Digit_Train, method="class",control = control)
summary(Digit_tree2) 

## Decision Tree Prediction
predictedTree3 = predict(Digit_tree3, Digit_Test_NO_LABEL, type="class")
DTT3 <- table(unlist(predictedTree3) ,unlist(Test_Labels))
confusionMatrix(DTT3)
## VIS..................
fancyRpartPlot(Digit_tree3)


## Testing another Naive Bayes Model to see if the accuracy improved 
library(naivebayes)
NB_object2 <- naive_bayes(Digit_Train$label ~., data=Digit_Train, laplace = 1,usekernel=TRUE)
NB_object2
NB_prediction2 <- predict(NB_object2, Digit_Test_NO_LABEL)
#NB_prediction
#head(predict(NB_object, DF_Test_NO_LABEL))
NBT2 <- table(NB_prediction2,Test_Labels)
confusionMatrix(NBT2)
plot(NB_prediction2)

## Testing another Naive Bayes Model to see if the accuracy improved 
library(naivebayes)
NB_object3 <- naive_bayes(Digit_Train$label ~., data=Digit_Train, laplace = 1,usepoisson =TRUE)
NB_object3
NB_prediction3 <- predict(NB_object3, Digit_Test_NO_LABEL)
#NB_prediction
NBT3 <- table(NB_prediction3,Test_Labels)
confusionMatrix(NBT3)
plot(NB_prediction3)

## Testing another Naive Bayes Model to see if the accuracy improved 
library(naivebayes)
NB_object4 <- naive_bayes(Digit_Train$label ~., data=Digit_Train,usepoisson =TRUE,usekernel = TRUE)
NB_object4
NB_prediction4 <- predict(NB_object4, Digit_Test_NO_LABEL)
#NB_prediction

NBT4 <- table(NB_prediction4,Test_Labels)
confusionMatrix(NBT4)
plot(NB_prediction4)
##Same results as poisson=TRUE probably because the values are all integer where usekernal wouldn't apply

