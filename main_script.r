rm(list=ls())
gc()

Sys.setlocale("LC_ALL", "C")

library(readr) 
library(xgboost)
library(caret)
library(lubridate)
library(moments)
library(robustbase)
library(reshape2)
library(ggplot2)

setwd("~/Documents/Avi/MOOC/Data Science/Kaggle/Springleaf Marketing Response")

set.seed(1) 


###parameters:
nulls_rate_to_remove        <- 0.96  #the lower, the more vars will be removed
same_var_rate_to_remove     <- 0.97   
corrs_rate                  <- 0.998
handle_NAs                  <- 1   # 0 = NO, 1 = YES
remove_negative_vals        <- 1   # 0 = NO, 1 = YES  --> if "implement log == 1" then also remove_negs == 1
implement_log               <- 1   # 0 = NO, 1 = YES
skewness_level_for_log      <- 15
implement_standartization   <- 0   # 0 = NO, 1 = YES

#
replace_outliers            <- 1   # 0 = NO, 1 = YES  
hampel_mult_value           <- 5    #hampel function
c_level_value               <- 6    #distance for standard_boxplot / adjusted_boxplot functions
normal_sk_level             <- 10    # normal skewness - no action is taken
sk_level_value              <- 15    #skewness threshold for type of procedure
replace_nulls_method_value  <- 2   # 1 = most_freq_value, 2 = Gastwirth_value, 3 = max_value
run_final_stats_gathering   <- 1   # 0 = NO, 1 = YES
#
spl_ratio                   <- 0.999
#
xparam_nrounds              <- 1700
xparam_eta                  <- 0.03
xparam_max_depth            <- 250
xparam_subsample            <- 0.80
xparam_colsample_bytree     <- 0.50

source("preprocess.r")
source("split_train.R")


xparam_colsample_bytree     <- 0.10
source("xgboost_model.r")
source("predict_xgboost.r")
write.csv(submission, "xgboost_submission4_subsamp080.colsamp010.csv",row.names=FALSE)
dim(submission);sum(is.na(submission));dim(train)

xparam_colsample_bytree     <- 0.25
source("xgboost_model.r")
source("predict_xgboost.r")
write.csv(submission, "xgboost_submission4_subsamp080.colsamp025.csv",row.names=FALSE)
dim(submission);sum(is.na(submission));dim(train)

xparam_colsample_bytree     <- 0.50
source("xgboost_model.r")
source("predict_xgboost.r")
write.csv(submission, "xgboost_submission4_subsamp080.colsamp050.csv",row.names=FALSE)
dim(submission);sum(is.na(submission));dim(train)

xparam_colsample_bytree     <- 0.75
source("xgboost_model.r")
source("predict_xgboost.r")
write.csv(submission, "xgboost_submission4_subsamp080.colsamp075.csv",row.names=FALSE)
dim(submission);sum(is.na(submission));dim(train)

xparam_nrounds              <- 1600
xparam_colsample_bytree     <- 1
source("xgboost_model.r")
source("predict_xgboost.r")
write.csv(submission, "xgboost_submission4_subsamp100.colsamp100.csv",row.names=FALSE)
dim(submission);sum(is.na(submission));dim(train)


xparam_colsample_bytree     <- 0.40
xparam_nrounds              <- 1700
source("xgboost_model.r")
source("predict_xgboost.r")
write.csv(submission, "xgboost_submission4_subsamp080.colsamp040.csv",row.names=FALSE)
dim(submission);sum(is.na(submission));dim(train)
#######
library(randomForest)
train$VAR_0274 <- NULL
test$VAR_0274 <- NULL

#predModel1 <- predict(rfmodel1,newdata=test)
rfmodel2 <- randomForest(target ~ .,data=train,ntree=1000, nodesize=5,mtry=ncol(train)*0.45,do.trace=TRUE,importance=TRUE)
