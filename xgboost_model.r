start_xgboost_time <- Sys.time()

cat("training a XGBoost classifier\n") 

dval <- xgb.DMatrix(data.matrix(train_validate[,feature.names]), label=train_validate$target)
dtrain <- xgb.DMatrix(data.matrix(train_work[,feature.names]), label=train_work$target)

watchlist <- list(eval = dval, train = dtrain)

param <- list(  objective           = "binary:logistic", 
                eta                 = xparam_eta,
                max_depth           = xparam_max_depth, 
                subsample           = xparam_subsample,
                colsample_bytree    = xparam_colsample_bytree,
                eval_metric         = "auc"
)

start_time <- Sys.time()
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = xparam_nrounds, 
                    verbose             = 2, 
                    early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)
end_xgboost_time <- Sys.time()
