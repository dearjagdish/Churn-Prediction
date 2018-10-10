setwd("D:/edwisor/Projects/Churn Reduction")
train_data <- read.csv("D:/edwisor/Projects/Churn Reduction/train_data.csv", header = TRUE, sep = ",")
test_data <- read.csv("D:/edwisor/Projects/Churn Reduction/test_data.csv", header = T, sep = ",")
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees', 'class')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#checking for missing values
missing_val = data.frame(apply(train_data,2,function(x){sum(is.na(x))}))

##Data Manupulation train data; convert string categories into factor numeric
for(i in 1:ncol(train_data)){
  
  if(class(train_data[,i]) == 'factor'){
    
    train_data[,i] = factor(train_data[,i], labels=(1:length(levels(factor(train_data[,i])))))
    
  }
}

##Data Manupulation test data; convert string categories into factor numeric
for(i in 1:ncol(test_data)){
  
  if(class(test_data[,i]) == 'factor'){
    
    test_data[,i] = factor(test_data[,i], labels=(1:length(levels(factor(test_data[,i])))))
    
  }
}


# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(train_data,is.numeric) #selecting only numeric

numeric_data = train_data[,numeric_index]

cnames = colnames(numeric_data)
 
 for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(train_data))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="Churn")+
            ggtitle(paste("Box plot of Churn for",cnames[i])))
 }
 
# ## Plotting plots together
 gridExtra::grid.arrange(gn1, gn2, gn3, gn4, gn5, gn6, gn7, gn8, nrow=4, ncol=2)
 gridExtra::grid.arrange(gn9, gn10, gn11, gn12, gn13, gn14, gn15, gn16, nrow =4, ncol=2)
 
 
 # # #Remove outliers using boxplot method
 for(i in cnames){
     print(i)
     val = train_data[,i][train_data[,i] %in% boxplot.stats(train_data[,i])$out]
     print(length(val))
     train_data = train_data[which(!train_data[,i] %in% val),]
    }
 
# #Replace all outliers with NA and impute
 for(i in cnames){
   val = train_data[,i][train_data[,i] %in% boxplot.stats(train_data[,i])$out]
   print(length(val))
   train_data[,i][train_data[,i] %in% val] = NA
 }

#created a missing value at location train_data[1,9] with Actual 110 got
 #mean  100.57, median 101 and knn 98.49 hence I fixed median method to impute
 
  
 #Mean Method
# train_data$total.day.calls[is.na(train_data$total.day.calls)] = mean(train_data$total.day.calls, na.rm = T)
 
 #Median Method
 train_data$number.vmail.messages[is.na(train_data$number.vmail.messages)] = median(train_data$number.vmail.messages, na.rm = T)
 train_data$total.eve.calls[is.na(train_data$total.eve.calls)] = median(train_data$total.eve.calls, na.rm = T)
 
 
 # kNN Imputation
# train_data = knnImputation(train_data, k = 3)
 
 #correlation plot
 corrgram(train_data[,numeric_index], order = F,
          upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
 
 ## Chi-squared Test of Independence
 factor_index = sapply(train_data,is.factor)
 factor_data = train_data[,factor_index]
 
 for (i in 1:4)
 {
   print(names(factor_data)[i])
   print(chisq.test(table(factor_data$Churn,factor_data[,i])))
 }
 
 ## Dimension Reduction
 train_data2 = subset(train_data, 
                          select = -c(total.day.charge, total.eve.charge, total.night.charge, total.intl.charge, phone.number))

 #Normalisation
 
 cnames2 = c("account.length", "area.code", "number.vmail.messages", "total.day.minutes", "total.day.calls", "total.eve.minutes", "total.eve.calls", "total.night.minutes", "total.night.calls", "total.intl.minutes", "total.intl.calls", "number.customer.service.calls")
 
 for(i in cnames2){
   print(i)
   train_data[,i] = (train_data[,i] - min(train_data[,i]))/
     (max(train_data[,i] - min(train_data[,i])))
 }
 
 #model development
 ##Decision tree for classification
 #Develop Model on training data
  DT_model = C5.0(Churn ~., train_data2, trials = 100, rules = TRUE)
 
 #write rules into disk
 write(capture.output(summary(DT_model)), "c50Rules.txt")
 
 test_data2 = subset(test_data, select = -c(4, 10, 13, 16, 19, 21))
 
 #Let us predict for test cases
 DT_Predictions = predict(DT_model, test_data2, type = "class")
 
  ##Evaluate the performance of classification model
 ConfMatrix_DT = table(test_data$Churn, DT_Predictions)
 confusionMatrix(ConfMatrix_DT)
 
 #False Negative rate
 FNR = FN/FN+TP 
 
 #Accuracy 93.5%
 #FNR 47.7%
 
  ###Random Forest
 RF_model = randomForest(Churn ~ ., train_data2, importance = TRUE, ntree = 100)
 
 #Predict test data using random forest model
 RF_Predictions = predict(RF_model, test_data2)
 
 ##Evaluate the performance of classification model
 ConfMatrix_RF = table(test_data$Churn, RF_Predictions)
 confusionMatrix(ConfMatrix_RF)
 
 #Accuracy 92.8%
 #FNR 50.8%
 
 #Logistic Regression
 logit_model = glm(Churn ~ ., data = train_data2, family = "binomial")
 
 #summary of the model
 summary(logit_model)
 
 #Fixed decision tree model for this dataset because of low FNR and high accuracy
 
 #predict using logistic regression
 logit_Predictions = predict(logit_model, newdata = test_data2, type = "response")
 
 #convert prob
 logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)
 
 
 ##Evaluate the performance of classification model
 ConfMatrix_LR = table(test_data$Churn, logit_Predictions)
 
 #Accuracy 
 sum(diag(ConfMatrix_LR))/nrow(test_data2)
 
 #Accuracy 88.4%
 #FNR = FN/FN+TP = 72.7%
 
 ##KNN Implementation
  KNN_Predictions = knn(train_data2[, 1:15], test_data2[, 1:15], train_data2$Churn, k = 3)
 
 #Confusion matrix
 Conf_matrix_knn = table(KNN_Predictions, test_data$Churn)
 
 #Accuracy 
 sum(diag(Conf_matrix_knn))/nrow(test_data2)
 
 #False Negative rate
 FNR = FN/FN+TP 
 
 #Accuracy = 88.1
 #FNR = 35.2
 
 #naive Bayes model
 NB_model = naiveBayes(Churn ~ ., data = train_data2)
 
 #predict on test cases #raw
 NB_Predictions = predict(NB_model, test_data2[,1:15], type = 'class')
 
 #Look at confusion matrix
 Conf_matrix_NB = table(observed = test_data[,21], predicted = NB_Predictions)
 confusionMatrix(Conf_matrix_NB)
 
 #Accuracy: 88.66
 #FNR: 76.33
#Fixed decision tree algorithm for this dataset due to their low FNR and high accuracy
 test_data = test_data[, 1:20]
df = data.frame(DT_Predictions) 
output_R = cbind(test_data, df)
write.csv(output_R, "output_R.csv", row.names = F)
