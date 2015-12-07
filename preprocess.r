start_preprocess_time <- Sys.time()

cat("reading the train and test data\n") 

train <- readRDS("train_id_and_NAs")
test  <- readRDS("test_id_and_NAs") 

###################################################################################
source("functions.r")
source("feature engineering.r")
source("dynamic creation of variables.r")
cat("current number of variables : ", ncol(train), "\n")

target_position <- which(names(train)=="target")

cat("creating comb dataset \n")
comb <- rbind(train[,-target_position],test)

########################################################################################################
most_imp_vars <-  c("VAR_0004" ,"VAR_0063" ,"VAR_0070" ,"VAR_0071", "VAR_0072" ,"VAR_0080" ,
                    "VAR_0081" ,"VAR_0082", "VAR_0087" ,"VAR_0088" ,"VAR_0089" ,"VAR_0200",
                    "VAR_0227" ,"VAR_0233" ,"VAR_0238", "VAR_0241", "VAR_0540" ,"VAR_0707",
                    "VAR_0708" ,"VAR_0712", "VAR_0716" ,"VAR_0720" ,"VAR_0721" ,"VAR_0722",
                    "VAR_0795" ,"VAR_0881" ,"VAR_0884" ,"VAR_0885", "VAR_0886" ,"VAR_1127")
#########################################################################################################
##summarize nulls by variable:
cat("handling NAs and removing variables with more than ", nulls_rate_to_remove , " NAs \n")
nas <- sapply(comb,function(x) sum(is.na(x)))
nas_df <- data.frame(nas)
nas_df$var_name <- row.names(nas_df)
row.names(nas_df) <- NULL
##find variables having more than 70% of NA values
nulls_variables <- subset(nas_df,nas > nrow(train)*nulls_rate_to_remove)$var_name
#nulls_variables <- subset(nas_df,nas > 1000)$var_name
cat("number of variables with high rate of NAs found : ", length(nulls_variables) , "\n")

for(n in nulls_variables) {
    train[,n] <- NULL
    test[,n] <- NULL
    comb[,n] <- NULL
}

discarded_nulls_variables <- nulls_variables
######################################################################################################

cat("removing variables having more than ", same_var_rate_to_remove, " the same value \n")
start_time <- Sys.time()
props_list <- find_variances_by_prop(comb,same_var_rate_to_remove)

end_time <- Sys.time()
elapsed <- end_time - start_time
elapsed

high_props_list <- props_list$var_name
high_props_orig <- grep("VAR_[0-9]{4}$",high_props_list,value = T)
high_props_featured <- high_props_list[! high_props_list %in% high_props_orig]

cat("total variables having same value : ", length(high_props_list), " for more than " , same_var_rate_to_remove ," \n")

for(n in high_props_list) {    
    train[,n] <- NULL
    test[,n] <- NULL
    comb[,n] <- NULL
}
dim(train);dim(test)

discarded_high_props <- high_props_list
#####
##Check if "most_important_features" are still there:
sum(most_imp_vars %in% names(train))

################################################################################################
cat("removing city's var = VAR0200 \n")

train$VAR_0200 <- NULL
test$VAR_0200 <- NULL
comb$VAR_0200 <- NULL
################################################################################################
# cat("checking that all factors have same levels \n")
# for(n in names(comb)){
#     if(class(comb[,n])=="factor"){
#         unique_levels <- levels(comb[,n])
#         if(levels(train[,n]) != unique_levels) cat("diff levels in train var :" , n,"\n")
#         if(levels(test[,n]) != unique_levels) cat("diff levels in test var:" , n,"\n")
#         
#     }
# }

################################################################################################
cat("removing negative values \n")
if (remove_negative_vals == 1){
    feature_names <- names(train)
    remove <- c("ID","target")
    feature_names <- feature_names[! feature_names %in% remove]    
    
    for(n in feature_names) {
        if( ! class(train[,n]) %in% c("factor","character")){
            
            indx <- which(train[,n] < 0)
            if (length(indx) > 0){
                cat("negative values in train : ", n, "\n")
                train[indx,n] <- 0
            }
            
            indx <- which(test[,n] < 0)
            if (length(indx) > 0){
                cat("negative values in test : ", n, "\n")
                test[indx,n] <- 0
            }
        }
    }
}
#################################################################################################
feature_names <- names(train)
remove <- c("ID","target")
feature_names <- feature_names[! feature_names %in% remove]
##check if 30 most imp. vars. are still present in datasets
sum(most_imp_vars %in% feature_names)

comb$ID <- NULL
comb$target <- NULL

cat("start handling NA values: find most frequent value and use it instead of NA \n")
####handle NA values:

vars_statistics <- feature_stats(comb)

write.csv(vars_statistics,"vars_statistics.csv",row.names=F)

cat("running replace_nulls on comb \n")
comb <- replace_nulls(comb,replace_nulls_method_value)

if(handle_NAs == 1){
    cat("running replace_nulls on train \n")
    train <- replace_nulls(train,replace_nulls_method_value)
    cat("sum NAs on train = ", sum(is.na(train)),"\n")
    Sys.sleep(2)

    cat("running replace_nulls on test \n")
    test <- replace_nulls(test,replace_nulls_method_value)
    cat("sum NAs on test = ", sum(is.na(test)),"\n")
    Sys.sleep(2)
}



#####################################################################################
cat("copying train rows from comb dataset to comb_corrs \n")

comb_corrs <- comb[1:nrow(train),]

feature_names <- names(comb_corrs)
remove <- c("ID","target")
feature_names <- feature_names[! feature_names %in% remove]

cat ("converting character and feature variables into numeric in comb_corrs \n")
char_vars <- NULL
for (f in feature_names) { 
    if (class(comb_corrs[[f]]) %in% c("character","Date")) { 
        cat("character class : ", f, "\n")
        levels <- unique(c(comb_corrs[[f]])) 
        char_vars <- rbind(char_vars,f)
        comb_corrs[[f]] <- as.integer(factor(comb_corrs[[f]], levels=levels)) 
    }
    if (class(comb_corrs[[f]])=="factor") { 
#        cat("factor class : ", f, "\n")
        levels <- unique(c(comb_corrs[[f]])) 
        char_vars <- rbind(char_vars,f)
        comb_corrs[[f]] <- as.integer(factor(comb_corrs[[f]], levels=levels)) 
    }
} 

final_factors <- as.vector(char_vars[,1])
############################################################################
cat("creating correlations matrix - handle_NAs = ",handle_NAs, "\n")

correlationMatrix <- cor(comb_corrs[,feature_names])

sum_nas <- sum(is.na(correlationMatrix))
if(sum_nas > 0){
    cat(sum_nas , "NAs found in correlationMatrix !! \n")
    correlationMatrix[is.na(correlationMatrix)] <- 0
}

cat("finding and removing variables with correlation higher than " , corrs_rate , "\n")
h0995 <- findCorrelation(correlationMatrix, cutoff = corrs_rate) 
cat("total number of highly correlated variables is :" , length(h0995) , "\n")

rm(comb_corrs)

high_corr <- names(train)[h0995]
dim(train);dim(test);length(high_corr)
for(n in high_corr) {
    comb[,n] <- NULL
    train[,n] <- NULL
    test[,n] <- NULL
}
dim(train);dim(test)
discarded_high_corr <- high_corr

if(run_final_stats_gathering == 1){

    target_position <- which(names(train)=="target")
    comb_final <- rbind(train[,-target_position],test)
    vars_statistics_final <- feature_stats(comb_final)
    write.csv(vars_statistics_final,"vars_statistics_final.csv",row.names=F)
    rm(comb_final)

}
################################################################################################

train_bkp <- train 
test_bkp <- test

################################################################################################
cat("repalce outliers = " , replace_outliers , "\n")
cat("parameters - sk_level =",sk_level_value, " c_value = ", c_level_value ,"\n")

if (replace_outliers == 1){
    train <- handle_skewed_outliers(train,sk_level = sk_level_value, 
                                    t_mult= hampel_mult_value,c_level_value = c_level_value,
                                    normal_sk_level = normal_sk_level)
    test <- handle_skewed_outliers(test,  sk_level = sk_level_value, 
                                   t_mult= hampel_mult_value,c_level_value = c_level_value,
                                    normal_sk_level = normal_sk_level)
}

variance_in_vars <- find_variances(train)
zero_variances <- subset(variance_in_vars,variance==0)$var_name
discarded_zero_variance <- zero_variances

cat("number of variables with zero variance = ", length(zero_variances),"\n")
for(var_name in zero_variances){
    comb[,var_name] <- NULL
    train[,var_name] <- NULL
    test[,var_name] <- NULL
}
################################################################################################
cat("skewness_level_for_log = ", skewness_level_for_log , "\n")

if (implement_log == 1){
    feature_names <- names(train)
    remove <- c("ID","target")
    feature_names <- feature_names[! feature_names %in% remove]    
    
    for(n in feature_names) {
        if( ! class(train[,n]) %in% c("factor","character")){
            if(abs(skewness(train[,n],na.rm=TRUE)) > skewness_level_for_log)  {
                train[,n] <- log(train[,n] + 1)
                test[,n] <- log(test[,n] + 1)
            }
        }
    }
}

################################################################################################

if (implement_standartization == 1){
    train <- standardize_fun(train)
    test  <- standardize_fun(test)
}
############################################################################################
####final fixes
unique_levels <- unique(c(levels(train$VAR_0283),levels(test$VAR_0283)))
unique_levels
levels(train$VAR_0283) <- unique_levels
levels(test$VAR_0283) <- unique_levels

unique_levels <- unique(c(levels(train$VAR_0075_year),levels(test$VAR_0075_year)))
levels(train$VAR_0075_year) <- unique_levels
levels(test$VAR_0075_year) <- unique_levels

unique_levels <- unique(c(levels(train$VAR_1377_factor),levels(test$VAR_1377_factor)))
levels(train$VAR_1377_factor) <- unique_levels
levels(test$VAR_1377_factor) <- unique_levels

################################################################################################

cat("handle_NAs = " , handle_NAs , " sum of NAs in train = ", sum(is.na(train)), "\n")
cat("handle_NAs = " , handle_NAs , " sum of NAs in test  = ", sum(is.na(test)), "\n")


rm(comb)
rm(correlationMatrix)
gc()

############################################################################################
cat("end of preprocess part \n")
cat("number of columns in train dataset = ", ncol(train),"\n")
cat("number of columns in test  dataset = ", ncol(test),"\n")

orig_vars <- grep("^VAR_[0-9]{4}$",names(train),value = T)
orig_vars <- c(orig_vars,c("ID","target"))
cat("number of original variables kept = ", length(orig_vars), "\n")
new_vars <- names(train)[!names(train) %in% orig_vars]
cat("number of new variables kept = " , length(new_vars), "\n")

target_var_num <- which(names(train)=="target")
cat("checking identity of var names \n" )
cat(setdiff(names(train[-target_var_num]),names(test)) , "\n" )
cat(setdiff(names(test),names(train[-target_var_num])) , "\n" )


end_preprocess_time <- Sys.time()

