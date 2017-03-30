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

#Hellooooooo!
