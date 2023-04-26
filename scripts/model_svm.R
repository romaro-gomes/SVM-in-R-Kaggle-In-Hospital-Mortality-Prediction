library(here)
library(tidyverse)
library(e1071)
library(caret)
library(pROC)
# Split --------------------------------------------------------
# data_to_model= readRDS(here("object/data_to_model.R"))
# 
# factor_columns = c('outcome','hypertensive','gendera', 'atrialfibrillation',"CHD_with_no_MI",'diabetes', 'deficiencyanemias', 'depression','Hyperlipemia', 'Renal_failure', 'COPD')
# 
# 
# data_to_model= data_to_model |> filter(outcome !='0.135204081632653') |>  mutate_at(factor_columns,as.factor) 
# 
# saveRDS(data_to_model,here("object/data_to_model.R"))

data_to_model= readRDS(here("object/data_to_model.R"))
data_to_model= data_to_model[,-2]
set.seed(123)
sample_to_model <- sample(c(TRUE, FALSE), nrow(data_to_model), replace=TRUE, prob=c(0.7,0.3))
train  <- data_to_model[sample_to_model, ]
test   <- data_to_model[!sample_to_model, ]


# Model---------------------------------------------------------------

svm_model = svm(Class ~ ., data = train, kernel = "radial", cost = 10,scale = TRUE,gamma=50)

summary (svm_model)

# Training Validation ----------------------------------------------------------


table (svm_model$fitted , train$Class )

caret::confusionMatrix(svm_model$fitted,train$Class)



# Test Validation --------------------------------------------


results= predict(svm_model,test[,!names(test) %in% c("Class")], decision.values = TRUE)

table(results,test$Class)
caret::confusionMatrix(results,test$Class)


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
                Class ~ .,
                data=train,
                kernel ="radial",
                ranges =list (cost=c(0.1 ,1 ,10 ,100 ,1000),
                                gamma =c(0.5 ,1 ,2 ,3 ,4) ))

tune.out

svm_model_tune = svm(Class ~ ., data = train, kernel = "radial", cost = 1,scale = TRUE,gamma=0.5)

table (svm_model_tune$fitted , train$Class )

caret::confusionMatrix(svm_model_tune$fitted,train$Class)



# Test Validation --------------------------------------------


results_tune= predict(svm_model_tune,test[,!names(test) %in% c("Class")])

table(results_tune,test$Class)
caret::confusionMatrix(results_tune,test$Class)

roc_svm_tune_test <- roc(response = test$Class, predictor =as.numeric(results_tune),
                    smoothed = TRUE,
                    # arguments for ci
                    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                    # arguments for plot
                    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(roc_svm_tune_test)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")


#saveRDS(svm_model_tune,here('object/svm_model_final.R'))
