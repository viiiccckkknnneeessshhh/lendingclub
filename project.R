library(ModelMetrics)
library(caret)
#Naive Bayes: accuracy=0.802260776059903  #AUC & MCC
library(e1071)
folds <- createFolds(trainData$y, k = 10)
cv_results <- lapply(folds, function(x) {
  train <- trainData[-x, ]
  test <- trainData[x, ]
  actual=test$y
  model1 <- naiveBayes(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+home_ownership+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+acc_now_delinq, data=train)
  #model1 <- naiveBayes(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+acc_now_delinq, data=train)
  test_pred <- predict(model1, test)
  res= auc(actual,test_pred) #note: this is using ModelMetrics and not pROC.

  return(res)
})
nb_cv_res=str(cv_results)

library(party)
folds <- createFolds(trainData$y, k = 10)
cv_results <- lapply(folds, function(x) {
  train <- trainData[-x, ]
  test <- trainData[x, ]
  actual=test$y
  model2 <- ctree(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+home_ownership+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+acc_now_delinq, data=train)
  #model2 <- ctree(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+acc_now_delinq, data=train)
  test_pred <- predict(model2, test)
  res=ModelMetrics::auc(actual,test_pred)
  
  return(res)
})
ctree_cv_res=str(cv_results)

#Regression of amount of unpaid
#Adjusted R2=0.8455
trainData_b= #wait dont touch this
model3=lm(unpaid ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+home_ownership+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc, data=trainData_b)



###########Vicknesh#########################################################################################################3


test <- read.csv("testData.csv",stringsAsFactors=TRUE)
train <- read.csv("trainData.csv",stringsAsFactors=TRUE)

#Remove missing values and remove test case 52832(guy with ANY)

testnew <- na.omit(test[-52832,-c(3,20:28)])
trainnew <-na.omit(train[,-c(3,20:28)])
testnew <- droplevels(testnew)

#Model.matrix preps the data for model fitting 
#it automatically transforms categorical to numbers and stuff so can immed fit

X_train_logistic = model.matrix(y ~ .,trainnew[,-c(22)])
X_test_logistic = model.matrix(y ~ .,testnew[,-c(22)])
Y_train_logistic = as.factor( trainnew[,"y"])
Y_test_logistic = as.factor( testnew[,"y"])

#checking dimension to see if test and train match
dim(X_train_logistic )
dim(X_test_logistic)


library(glmnet)

#generate saple lambda values
lambda.grid = 10^seq(1,-3,length=100)


#cross validate the lambda cv.glmnet automatically does this for us so no worries :)
#fit a lasso logistic regression
cvfit.lasso = cv.glmnet(X_train_logistic, Y_train_logistic,lambda = lambda.grid, alpha=1, family = "binomial")
plot(cvfit.lasso)

#fit a ridge logistic regression
cvfit.ridge = cv.glmnet(X_train_logistic, Y_train_logistic,lambda = lambda.grid, alpha=0, family = "binomial")
plot(cvfit.ridge)

#Attempt to predict 
prediction.lasso = predict(cvfit.lasso, newx = X_test_logistic,s = "lambda.min", type="response")
prediction.ridge = predict(cvfit.ridge, newx = X_test_logistic,s = "lambda.min", type="response")

#library for ROC
library(pROC)

#plot ROC with AUC values

plot.roc(testnew[,"y"], as.vector(prediction.lasso),col="red", lwd=3, print.auc=TRUE, print.auc.y = 0.2)
plot.roc(testnew[,"y"], as.vector(prediction.ridge),col="green", lwd=3, print.auc=TRUE, print.auc.y = 0.1, add=TRUE)



##########################################################################################################################







