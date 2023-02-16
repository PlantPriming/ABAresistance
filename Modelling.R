####Note on DSI ####
#DSI was used as a way to unify the heterogeneity of disease scoring in our dataset. 
#For example, Achuo et al 2006: quantified disease by “% score frequency for disease categories”, and by “% spreading lesions”.
#All DSI values are compared to the control, if ABA application was applied across 2 genotypes, plants were compared to mock (non ABA) within their genotype. If no ABA treatment was used disease score of ABA mutant plants was compared to that of WT plant values.

#Installing packages##


if (!require('RColorBrewer')) install.packages('RColorBrewer'); library('RColorBrewer')
if (!require('rattle')) install.packages('rattle'); library('rattle')
if (!require('rpart')) install.packages('rpart'); library('rpart')
if (!require('regclass')) install.packages('regclass'); library('regclass')
if (!require('caTools')) install.packages('caTools'); library('caTools')
if (!require('rpart.plot')) install.packages('rpart.plot'); library('rpart.plot')
if (!require('caret')) install.packages('caret'); library('caret')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('randomForest')) install.packages('randomForest'); library('randomForest')
if (!require('ROCR')) install.packages('ROCR'); library('ROCR')


##Random Forest 1####
RF1<-read.table("Supp table 1.csv", header = TRUE, sep = ",")

set.seed(100)  
trainingRowIndex <- sample(1:nrow(RF1), 0.70*nrow(RF1))  # row indices for training data
trainingData <- RF1[trainingRowIndex, ]  # model training data
testData  <- RF1[-trainingRowIndex, ]   # test data

RFmodel1 <- train(
  Binary ~., data = RF1, method = "rf",ntree = 400,
  trControl = trainControl("cv", number = 10),
  importance = TRUE)

RFmodel1$bestTune
RFmodel1$finalModel

predicted.classes <- RFmodel1 %>% predict(testData)
head(predicted.classes)

importance(RFmodel1$finalModel)
varImpPlot(RFmodel1$finalModel, type = 1)

varImp(RFmodel1)

#######Accuracy of RF1 for precision and recall values ############
set.seed(100)  
sample_splitRF1<- sample.split(Y = RF1$Binary, SplitRatio = 0.75)
train_setRF1 <- subset(x = RF1, sample_splitRF1 == TRUE)
test_setRF1 <- subset(x = RF1, sample_splitRF1 == FALSE)

testDataRF1 = na.omit(test_setRF1)
trainingDataRF1 = na.omit(train_setRF1)

RFp1 <- predict(RFmodel1, testDataRF1)
confusionMatrix(RFmodel1,as.factor(testDataRF1$Binary))

#######RF2 (with experimental data included)#####
RF2<-read.table("Supp table 2.csv", header = TRUE, sep = ",")

RFmodel2 <- train(
  Binary ~., data = RF2, method = "rf",ntree = 400,
  trControl = trainControl("cv", number = 10),
  importance = TRUE)

RFmodel2$finalModel
varImp(RFmodel2)

#######Accuracy of RF2 for precision and recall values ############
set.seed(100)  
sample_splitRF2<- sample.split(Y = RF2$Binary, SplitRatio = 0.75)
train_setRF2 <- subset(x = RF2, sample_splitRF2 == TRUE)
test_setRF2 <- subset(x = RF2, sample_splitRF2 == FALSE)

testDataRF2 = na.omit(test_setRF2)
trainingDataRF2 = na.omit(train_setRF2)

RFp2 <- predict(RFmodel2, testDataRF2)
confusionMatrix(RFmodel2,as.factor(testDataRF2$Binary))


######DECISION TREE 1 ##### 
DT1<-read.table("Supp table 3.csv", header = TRUE, sep = ",")
DTmodel1 <-rpart(Binary ~ Plant.species + Plant.mutant+ Plant.age..weeks. + Concentration..μm. + Pathogen.type 
+             +  Pathogen.lifestyle + Tissue, data=DT1, method = "class", minsplit = 25)
fancyRpartPlot(DTmodel1,caption= NULL)
summarize_tree(DTmodel1)

printcp(DTmodel1)
plotcp(DTmodel1) 
DTmodel1$cptable[which.min(DTmodel1$cptable[,"xerror"]),"CP"] #pruning the best tree
bestcp <- DTmodel1$cptable[which.min(DTmodel1$cptable[,"xerror"]),"CP"]
DTmodel1.pruned <- prune(DTmodel1, cp = bestcp)
fancyRpartPlot(DTmodel1.pruned,caption= NULL )


####Accuracy of DT1 ######

set.seed(100) 
sample_splitDT1 <- sample.split(Y = DT1$Binary, SplitRatio = 0.75)
train_setDT1 <- subset(x = DT1, sample_splitDT1 == TRUE)
test_setDT1 <- subset(x = DT1, sample_splitDT1 == FALSE)

testDataDT1 = na.omit(test_setDT1)
trainingDataDT1 = na.omit(train_setDT1)

predsDT1 <- predict(DTmodel1, newdata = testDataDT1, type = "class")
confusionMatrix(predsDT1,as.factor(testDataDT1$Binary))

##ROC plot
tree.preds <- predict(DTmodel1, testDataDT1, type="prob")[, 2]
predDT1=prediction(tree.preds, test_setDT1$Binary)
plot(performance(predDT1, "tpr","fpr"))
abline(0,1,lty=2)

##AUC value
auc_ROCRDT1 <- performance(predDT1, measure = "auc")
auc_ROCRDT1 <- auc_ROCRDT1@y.values[[1]]
auc_ROCRDT1


#Accuracy of DT2 (DT plus our experimental data)
DT2<-read.table("Supp table 4.csv", header = TRUE, sep = ",")

DTmodel2 <-rpart(Binary ~ Plant.species + Plant.mutant+ Plant.age..weeks. + Concentration..μm. + Pathogen.type 
+             +  Pathogen.lifestyle + Tissue, data= DT2, method = "class", minsplit = 25)

fancyRpartPlot(DTmodel2,caption= NULL)

##accuracy of DT2
sample_splitDT2 <- sample.split(Y = DT2$Binary, SplitRatio = 0.75)
train_setDT2 <- subset(x = DT2, sample_splitDT2 == TRUE)
test_setDT2 <- subset(x = DT2, sample_splitDT2 == FALSE)
testDataDT2 = na.omit(test_setDT2)
trainingDataDT2 = na.omit(train_setDT2)
predsDT2 <- predict(DTmodel2, newdata = testDataDT2, type = "class")
confusionMatrix(predsDT2,as.factor(testDataDT2$Binary))

##ROC plot for DT2
tree.preds2 <- predict(DTmodel2, testDataDT2, type="prob")[, 2]
predDT2=prediction(tree.preds2, test_setDT2$Binary)
plot(performance(predDT2, "tpr","fpr"))
abline(0,1,lty=2)

##AUC value for DT2
auc_ROCRDT2 <- performance(predDT2, measure = "auc")
auc_ROCRDT2 <- auc_ROCRDT2@y.values[[1]]
auc_ROCRDT2


###Simplified models ####

#DText (concentrations above 800uM removed from original dataset)
DT1<-read.table("Supp table 3.csv", header = TRUE, sep = ",")
DText <- subset(DT1, DT1$Concentration..μm.< 800)

DTmodelext <-rpart(Binary ~ Plant.species + Plant.mutant+ Plant.age..weeks. + Concentration..μm. + Pathogen.type 
                 +             +  Pathogen.lifestyle + Tissue, data=DText, method = "class", minsplit = 25)
fancyRpartPlot(DTmodelext,caption= NULL)
summarize_tree(DTmodelext)

printcp(DTmodelext)
plotcp(DTmodelext) 
DTmodelext$cptable[which.min(DTmodelext$cptable[,"xerror"]),"CP"] #pruning the best tree
bestcpDText <- DTmodelext$cptable[which.min(DTmodelext$cptable[,"xerror"]),"CP"]
DTmodelext.pruned <- prune(DTmodelext, cp = bestcpDText)
fancyRpartPlot(DTmodelext.pruned,caption= NULL )

####Accuracy of DTExt ######

set.seed(100)
sample_splitDText <- sample.split(Y = DText$Binary, SplitRatio = 0.75)
train_setDText <- subset(x = DText, sample_splitDText == TRUE)
test_setDText <- subset(x = DText, sample_splitDText == FALSE)

testDataDText = na.omit(test_setDText)
trainingDataDText = na.omit(train_setDText)

predsDText <- predict(DTmodelext, newdata = testDataDText, type = "class")
confusionMatrix(predsDText,as.factor(testDataDText$Binary))

##ROC plot for DText
tree.predsext <- predict(DTmodelext, testDataDText, type="prob")[, 2]
predDText=prediction(tree.predsext, test_setDText$Binary)
plot(performance(predDText, "tpr","fpr"))
abline(0,1,lty=2)

##AUC value for DText
auc_ROCRDText <- performance(predDText, measure = "auc")
auc_ROCRDText <- auc_ROCRDText@y.values[[1]]
auc_ROCRDText


##RFext - Random forest with concentrations above 800uM removed

RF1<-read.table("Supp table 1.csv", header = TRUE, sep = ",")
RFext <- subset(RFext, RF1$Concentration..μm.< 800)

set.seed(100)  
trainingRowIndexRFext <- sample(1:nrow(RFext), 0.70*nrow(RFext))  # row indices for training data
trainingDataRFext <- RFext[trainingRowIndexRFext, ]  # model training data
testDataRFext  <- RFext[-trainingRowIndexRFext, ]   # test data


RFmodelext <- train(
  Binary ~., data = RFext, method = "rf",ntree = 400,
  trControl = trainControl("cv", number = 10),
  importance = TRUE)

RFmodelext$bestTune
RFmodelext$finalModel

predicted.classesRFext <- RFmodelext %>% predict(testDataRFext)
head(predicted.classesRFext)

importance(RFmodelext$finalModel)
varImpPlot(RFmodelext$finalModel, type = 1)

varImp(RFmodelext)

#######Accuracy of RFext for precision and recall values ############
set.seed(100)  
sample_splitRFext<- sample.split(Y = RFext$Binary, SplitRatio = 0.75)
train_setRFext <- subset(x = RFext, sample_splitRFext == TRUE)
test_setRFext <- subset(x = RFext, sample_splitRFext == FALSE)

testDataRFext = na.omit(test_setRFext)
trainingDataRFext = na.omit(train_setRFext)

RFpext <- predict(RFmodelext, testDataRFext)
confusionMatrix(RFmodelext,as.factor(testDataRFext$Binary))



##DTage (Age values less than 6-weeks removed)

DT1<-read.table("Supp table 3.csv", header = TRUE, sep = ",")
DTage<-subset(DT1, DT1$Plant.age..weeks. < 6)

DTmodelage <-rpart(Binary ~ Plant.species + Plant.mutant+ Plant.age..weeks. + Concentration..μm. + Pathogen.type 
                   +             +  Pathogen.lifestyle + Tissue, data=DTage, method = "class", minsplit = 25)
fancyRpartPlot(DTmodelage,caption= NULL)
summarize_tree(DTmodelage)

printcp(DTmodelage)
plotcp(DTmodelage) 
DTmodelage$cptable[which.min(DTmodelage$cptable[,"xerror"]),"CP"] #pruning the best tree
bestcpDTage <- DTmodelage$cptable[which.min(DTmodelage$cptable[,"xerror"]),"CP"]
DTmodelage.pruned <- prune(DTmodelage, cp = bestcpDTage)
fancyRpartPlot(DTmodelage.pruned,caption= NULL )

####Accuracy of DTage ######

set.seed(100) 
sample_splitDTage <- sample.split(Y = DTage$Binary, SplitRatio = 0.75)
train_setDTage <- subset(x = DTage, sample_splitDTage == TRUE)
test_setDTage <- subset(x = DTage, sample_splitDTage == FALSE)

testDataDTage = na.omit(test_setDTage)
trainingDataDTage = na.omit(train_setDTage)

predsDTage <- predict(DTmodelage, newdata = testDataDTage, type = "class")
confusionMatrix(predsDTage,as.factor(testDataDTage$Binary))

##ROC plot for DTage
tree.predsage <- predict(DTmodelage, testDataDTage, type="prob")[, 2]
predDTage=prediction(tree.predsage, test_setDTage$Binary)
plot(performance(predDTage, "tpr","fpr"))
abline(0,1,lty=2)

##AUC value for DTage
auc_ROCRDTage <- performance(predDTage, measure = "auc")
auc_ROCRDTage <- auc_ROCRDTage@y.values[[1]]
auc_ROCRDTage

##RFage (Original RF1 with age values less than 6 weeks removed)

RF1<-read.table("Supp table 1.csv", header = TRUE, sep = ",")
RFage<-subset(RFage, RF1$Plant.age..weeks. < 6)

set.seed(100)  
trainingRowIndex_RFage <- sample(1:nrow(RFage), 0.70*nrow(RFage))  # row indices for training data
trainingDataRFage <- RFage[trainingRowIndex_RFage, ]  # model training data
testDataRFage  <- RFage[-trainingRowIndex_RFage, ]   # test data

RFmodelage <- train(
  Binary ~., data = RFage, method = "rf",ntree = 400,
  trControl = trainControl("cv", number = 10),
  importance = TRUE)

RFmodelage$bestTune
RFmodelage$finalModel

predicted.classes_RFage <- RFmodelage %>% predict(testDataRFage)
head(predicted.classes_RFage)

importance(RFmodelage$finalModel)
varImpPlot(RFmodelage$finalModel, type = 1)

varImp(RFmodelage)

#######Accuracy of RFage for precision and recall values ############
set.seed(100)  
sample_splitRFage<- sample.split(Y = RFage$Binary, SplitRatio = 0.75)
train_setRFage <- subset(x = RFage, sample_splitRFage == TRUE)
test_setRFage <- subset(x = RFage, sample_splitRFage == FALSE)

testDataRFage = na.omit(test_setRFage)
trainingDataRFage = na.omit(train_setRFage)

RFpage <- predict(RFmodelage, testDataRFage)
confusionMatrix(RFmodelage,as.factor(testDataRFage$Binary))


