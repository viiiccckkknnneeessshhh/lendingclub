#Classification tree: accuracy=0.816357842558716
library(C50)
model1 <- C5.0(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+home_ownership+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc, data = trainData)
loan_pred <- predict(model1, testData)
loan_actual <- testData$y
accuracy <- sum(loan_actual==loan_pred)/length(loan_actual)

library(caret)
#Naive Bayes: accuracy=0.802260776059903  #AUC & MCC
library(e1071)
folds <- createFolds(trainData$y, k = 10)
cv_results <- lapply(folds, function(x) {
  train <- trainData[-x, ]
  test <- trainData[x, ]
  actual=test$y
  #model1 <- naiveBayes(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+home_ownership+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+acc_now_delinq, data=train)
  model1 <- naiveBayes(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+acc_now_delinq, data=train)
  test_pred <- predict(model1, test)
  res=ModelMetrics::auc(actual,test_pred)

  return(res)
})
nb_cv_res=str(cv_results)

library(party)
folds <- createFolds(trainData$y, k = 10)
cv_results <- lapply(folds, function(x) {
  train <- trainData[-x, ]
  test <- trainData[x, ]
  actual=test$y
  #model2 <- ctree(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+home_ownership+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+acc_now_delinq, data=train)
  model2 <- ctree(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+acc_now_delinq, data=train)
  test_pred <- predict(model2, test)
  res=ModelMetrics::auc(actual,test_pred)
  
  return(res)
})
ctree_cv_res=str(cv_results)

library(e1071)
model1 <- naiveBayes(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+home_ownership+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+acc_now_delinq, data=trainData)
test_pred <- predict(model1, testData)
accuracy <- sum(loan_actual==test_pred)/length(loan_actual)
auc(loan_actual,test_pred)

model2=ctree(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+home_ownership+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+acc_now_delinq, data=trainData)
test_pred2=predict(model2,testData)
accuracy <- sum(loan_actual==test_pred2)/length(loan_actual)
auc(loan_actual,test_pred2)
#Random Forest
testData$term=as.factor(testData$term)
testData$sub_grade=as.factor(testData$sub_grade)
testData$emp_length=as.factor(testData$emp_length)
testData$home_ownership=as.factor(testData$home_ownership)
testData$verification_status=as.factor(testData$verification_status)
testData$purpose=as.factor(testData$purpose)
testData$addr_state=as.factor(testData$addr_state)

model4=randomForest(y ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+home_ownership+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc, data = trainData,na.action=na.exclude,importance=TRUE,ntree=10,do.trace=T,type="classification")
rf_pred=predict(model4,testData)
accuracy <- sum(loan_actual==rf_pred)/length(loan_actual)
#Possible extension: look for commonalities for false positives/true positives

#Regression of amount of unpaid
#Adjusted R2=0.8455
model3=lm(unpaid ~ funded_amnt+term+int_rate+installment+sub_grade+emp_length+home_ownership+annual_inc+verification_status+purpose+addr_state+dti+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc, data=completed_loan_b)


