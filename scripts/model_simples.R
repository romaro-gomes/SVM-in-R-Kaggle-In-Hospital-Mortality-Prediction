library(here)
library(tidyverse)
library(e1071)

# Load objects
data=readRDS(here('object/data_to_model.R'))
data=data[,-2]

set.seed(123)
sample_to_model <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train  <- data[sample_to_model, ]
test   <- data[!sample_to_model, ]

svm_model = svm(Class ~ gendera + hypertensive + atrialfibrillation + Hyperlipemia + age + BMI + heart_rate + PH + Respiratory_rate,
                data = train, kernel = "radial", cost = 10,scale = TRUE,gamma=50,probability = T)

summary (svm_model)

# Training Validation ----------------------------------------------------------


table (svm_model$fitted , train$Class )

caret::confusionMatrix(svm_model$fitted,train$Class)

# Test Validation --------------------------------------------


results= predict(svm_model,test[,!names(test) %in% c("Class")], probability = TRUE)
results

table(results,test$Class)
caret::confusionMatrix(results,test$Class)

attr(results, 'probability')
# ROC CURVE -------------------------------------------------------------------------------------------

library(pROC)

roc_svm_train <- roc(response = train$Class, predictor =as.numeric(svm_model$fitted))
roc_svm_test <- roc(response = test$Class, predictor =as.numeric(results))

plot(roc_svm_train)
plot(roc_svm_test)

# Code by https://rviews.rstudio.com/
roc_svm_test <- roc(response = test$Class, predictor =as.numeric(results),
                    smoothed = TRUE,
                    # arguments for ci
                    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                    # arguments for plot
                    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(roc_svm_test)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")



# tune ---------------------------------------------------------
set.seed (123)
tune.out = tune(svm,
                Class ~ gendera + hypertensive + atrialfibrillation + Hyperlipemia + age + BMI + heart_rate + PH + Respiratory_rate,
                data=train,
                kernel ="radial",
                ranges =list (cost=c(0.1 ,1 ,10 ,100 ,1000),
                              gamma =c(0.5 ,1 ,2 ,3 ,4) ))

tune.out

svm_model_tune = svm(Class ~ gendera + hypertensive + atrialfibrillation + Hyperlipemia + age + BMI + heart_rate + PH + Respiratory_rate, data = train, kernel = "radial", cost = 1,scale = TRUE,gamma=0.5,probability = T)

table (svm_model_tune$fitted , train$Class )

caret::confusionMatrix(svm_model_tune$fitted,train$Class)



# Test Validation --------------------------------------------


results_tune= predict(svm_model_tune,test[,!names(test) %in% c("Class")],probability = T)
attr(results_tune,"probabilities")

table(results_tune,test$Class)
caret::confusionMatrix(results_tune,test$Class)

roc_svm_tune_test <- roc(response = test$Class, predictor =as.numeric(results),
                         smoothed = TRUE,
                         # arguments for ci
                         ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                         # arguments for plot
                         plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                         print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(roc_svm_tune_test)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")



#saveRDS(svm_model_tune,here('object/svm_model_simples.R'))
