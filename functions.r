cat("loading functions \n")

Gastwirth_estimator <- function(var){    
    ordstats = quantile(var, probs=c(1/3,1/2,2/3),na.rm=TRUE)
                        wts = c(0.3,0.4,0.3)
                        Gastwirth_value <- sum(wts*ordstats)
                        
                        return(Gastwirth_value)
}


##function for replacing the Month's abbreviation by its value
convert_month <- function(var_date){
    
    var_date <- gsub("JAN","-01-",var_date)
    var_date <- gsub("FEB","-02-",var_date)
    var_date <- gsub("MAR","-03-",var_date)
    var_date <- gsub("APR","-04-",var_date)
    var_date <- gsub("MAY","-05-",var_date)
    var_date <- gsub("JUN","-06-",var_date)
    var_date <- gsub("JUL","-07-",var_date)
    var_date <- gsub("AUG","-08-",var_date)
    var_date <- gsub("SEP","-09-",var_date)
    var_date <- gsub("OCT","-10-",var_date)
    var_date <- gsub("NOV","-11-",var_date)
    var_date <- gsub("DEC","-12-",var_date)
    
    var_date_final <- dmy_hms(var_date)
    return(var_date_final)    
}

#!!!
find_variances_by_prop <- function(dataset,prop_rate){
    
    feature_names <- names(dataset)
    remove <- c("ID","target")
    feature_names <- feature_names[! feature_names %in% remove]
    
    props <- NULL
    for (feature_name in feature_names) {  
        col_number  <- grep(feature_name,colnames(dataset))
        
        prop_df <- data.frame(round(prop.table(table(dataset[,feature_name])),2))
        names(prop_df) <- c("var_name","prop")        
        high_prop <- subset(prop_df,prop > prop_rate)$prop
        if(length(high_prop) > 0){
            props <- rbind(props,c(high_prop,feature_name))
        }
    }
    props_df <- data.frame(props)
    names(props_df) <- c("prop","var_name")
    return(props_df)
}

find_variances <- function(dataset){
    
    feature_names <- names(dataset)
    remove <- c("ID","target")
    feature_names <- feature_names[! feature_names %in% remove]

    vars <- NULL
    for (feature_name in feature_names) {  
        col_number  <- grep(feature_name,colnames(dataset))
        
        variance <- as.numeric(var(dataset[,feature_name],na.rm = TRUE))
        vars <- rbind(vars,c(feature_name ,variance))
    }
    vars <- data.frame(vars)
    names(vars) <- c("var_name","variance")
    vars$variance <- as.numeric(as.character(vars$variance))
    
    return(vars)
}

# find_most_freq_value <- function(dataset){
#     feature_names <- names(dataset)
#     
#     props <- NULL
#     prop_df <- NULL
#     for (feature_name in feature_names) {  
#         prop_df <- data.frame(sort(round(prop.table(table(dataset[,feature_name])),2),decreasing = T)[1])
#         prop_df$max_val <- row.names(prop_df)
#         row.names(prop_df) <- NULL
#         prop_df$var_name <- feature_name
#         prop_df$class  <- class(dataset[,feature_name])
#         names(prop_df) <- c("var_proportion","max_value","var_name","class")        
#         print(prop_df)
#         props <- rbind(props,prop_df)
#     }
#     props_df <- data.frame(props)
#     return(props_df)
# }

replace_nulls <- function(dataset,replace_nulls_method = 1){
    feature_names <- names(dataset)
    count <- 0
    
    for(feature_name in feature_names){
        count <- count + 1
        if(count %% 10 == 0) cat(".")
        if(count %% 50 == 0) cat("/")
        if(count %% 500 == 0) cat("!!!")
        
        nas <- which(is.na(dataset[feature_name]))
        class_name <- vars_statistics[vars_statistics$var_name == feature_name,]$class
        if(length(class_name > 0)){
            if (replace_nulls_method == 1)  {
                replace_value <-  vars_statistics[vars_statistics$var_name == feature_name,]$most_freq_value
            }
            if (replace_nulls_method == 2)  {
                replace_value <-  vars_statistics[vars_statistics$var_name == feature_name,]$Gastwirth_value
            }
            if (replace_nulls_method == 3)  {
                replace_value <-  vars_statistics[vars_statistics$var_name == feature_name,]$max_value
            }
        }
#        cat("var name : ", feature_name, "class_name : ",class_name," number of nas : ", 
#            length(nas)," rep value :  ",replace_value,"\n")
        
        if(length(nas) > 0 & length(class_name)  > 0) {
            if(class_name == "integer"){
                dataset[nas,feature_name] <- as.integer(replace_value)
            }  else if(class_name == "numeric"){
                dataset[nas,feature_name] <- as.numeric(replace_value)
            } else if(class_name == "factor"){
                dataset[,feature_name]  <- as.character(dataset[,feature_name])
                dataset[nas,feature_name] <- as.factor(replace_value)
                dataset[,feature_name]  <- as.factor(dataset[,feature_name])
                
            } else if(class_name == "character"){
                dataset[nas,feature_name] <- as.character(replace_value)
            } else {
                cat("not handled class : " , class_name,"\n")
            }
            
        }  # close if(length(nas)>0)   
    }    # close for loop
    cat("\n")
    return(dataset)
}


feature_stats <- function(dataset){
    count <- 0
    feature_names <- names(dataset)
    
    props <- NULL
    features_df <- NULL
    for (feature_name in feature_names) {  
        count <- count + 1
        if(count %% 10 == 0) cat(".")
        if(count %% 50 == 0) cat("/")
        if(count %% 500 == 0) cat("!!!")
        
        feature_class <- class(dataset[,feature_name])[1]
        
        if(feature_class %in% c("integer", "numeric")){
            features_df <- data.frame(sort(round(prop.table(table(dataset[,feature_name])),2),decreasing = T)[1])
            features_df$max_val <- row.names(features_df)
            row.names(features_df) <- NULL
            features_df$var_name <- feature_name        
            features_df$class  <- feature_class
            features_df$unique_vals <- length(unique(dataset[,feature_name],na.rm=TRUE))
            
            features_df$skewness <- skewness(dataset[,feature_name],na.rm = TRUE)
            features_df$kurtosis <- kurtosis(dataset[,feature_name],na.rm = TRUE)
            features_df$mean <- mean(dataset[,feature_name],na.rm = TRUE)
            features_df$sd <- sd(dataset[,feature_name],na.rm = TRUE)
            features_df$variance <- var(dataset[,feature_name],na.rm = TRUE)
            features_df$min_value <- min(dataset[,feature_name],na.rm = TRUE)
            features_df$max_value <- max(dataset[,feature_name],na.rm = TRUE)
            features_df$median_value <- median(dataset[,feature_name],na.rm = TRUE)     
            features_df$IQR_value <- IQR(dataset[,feature_name],na.rm = TRUE)     
            features_df$Gastwirth_value  <- Gastwirth_estimator(dataset[,feature_name])
            
        }else if (feature_class %in% c("factor", "character")) {
            features_df <- data.frame(sort(round(prop.table(table(dataset[,feature_name])),2),decreasing = T)[1])
            features_df$max_val <- row.names(features_df)
            row.names(features_df) <- NULL
            features_df$var_name <- feature_name        
            features_df$class  <- feature_class
            features_df$unique_vals <- length(unique(dataset[,feature_name],na.rm=TRUE))
            
            features_df$skewness  <- skewness(as.integer(dataset[,feature_name]),na.rm = TRUE)
            features_df$kurtosis  <- kurtosis(as.integer(dataset[,feature_name]),na.rm = TRUE)
            features_df$mean      <- mean(as.integer(dataset[,feature_name]),na.rm = TRUE)
            features_df$sd        <- sd(as.integer(dataset[,feature_name]),na.rm = TRUE)
            features_df$variance  <-    var(as.integer(dataset[,feature_name]),na.rm = TRUE)
            features_df$min_value <-    min(as.integer(dataset[,feature_name]),na.rm = TRUE)
            features_df$max_value <-    max(as.integer(dataset[,feature_name]),na.rm = TRUE)
            features_df$median_value <- median(as.integer(dataset[,feature_name]),na.rm = TRUE)
            features_df$IQR_value <-    IQR(as.integer(dataset[,feature_name]),na.rm = TRUE)
            features_df$Gastwirth_value  <- Gastwirth_estimator(as.integer(dataset[,feature_name]))
        }  else {
            cat("not gathering info for variable : " ,feature_name , " class is " , feature_class,"\n" )
        }
        names(features_df) <- c("var_proportion","most_freq_value","var_name","class",
                                "count_unique_vals","skewness","kurtosis","mean","sd","variance",
                                "min_value","max_value","median_value","IQR_value","Gastwirth_value")
        #        print(features_df)
        props <- rbind(props,features_df)
    }
    props_df <- data.frame(props)
    cat("\n")
    return(props_df)
}


##standartization function: (x - mean)/sd
standardize_fun <- function(dataset){
    feature_names <- names(dataset)
    remove <- c("ID","target")
    feature_names <- feature_names[!feature_names %in% remove]
    
    for (var in feature_names){
        if(class(dataset[,var]) %in% c("integer","numeric")){
            mean_val <- mean(dataset[,var],na.rm=TRUE)
            sd_val   <- sd(dataset[,var],na.rm=TRUE)
            dataset[,var] <- ( dataset[,var] - mean_val) / sd_val
        }        
    }
    return (dataset)
}

##outliers functions
hampel_identify_and_replace <- function(var, t_val = 5, RemoveNAs = TRUE){
    #  Identity outliers according to the Hampel detection rule, if any
    med <- median(var, na.rm = TRUE)
    sig <- mad(var, na.rm = TRUE)
    indx <- which( abs(var - med) > t_val * sig)
    inter_quartile <- IQR(var,na.rm=TRUE)
    #replace outliers by IQR    
    var[indx] <- inter_quartile
    return(var)
}

standard_boxplot <- function(var,c_val = c_value, RemoveNAs = TRUE){
    # [Q1 - c * IQD, Q3 + c * IQD]
    # [Q1 ??? c * IQD, Q3 + c * IQD]
    q1 <- quantile(var,na.rm=TRUE)[2]
    q3 <- quantile(var,na.rm=TRUE)[4]
    inter_quartile <- IQR(var,na.rm=TRUE)
    lower_value <- q1 - c_val * inter_quartile
    if (lower_value < 0) lower_value <- 0
    upper_value <- q3 + c_val * inter_quartile
    replace_lower_values <- lower_value * 0.99
    replace_upper_values <- upper_value * 1.1
    #replace outliers by "standard boxplot" values 
    indx_l <- which(var < lower_value)
    indx_u <- which(var > upper_value)
    var[indx_l] <- replace_lower_values
    var[indx_u] <- replace_upper_values
    return(var)
}

adjusted_boxplot <- function(var,c_val = 1.5,a_val = -4, b_val = 3, RemoveNAs = TRUE){
    mc_value <- mc(var,na.rm=TRUE)
    # for positive MC values:
    #    [Q1 ??? c * exp(a * MC) * IQD, Q3 + c * exp(b * MC) * IQD ]
    # for negative MC values:
    #   [Q1 ??? c * exp(-b * MC) * IQD, Q3 + c * exp(-a * MC) * IQD ]
    q1 <- quantile(var,na.rm=TRUE)[2]
    q3 <- quantile(var,na.rm=TRUE)[4]
    inter_quartile <- IQR(var,na.rm=TRUE)
    if (mc_value > 0){
        lower_value <- (q1 - c_val * exp(a_val * mc_value) * inter_quartile)
        upper_value <- (q3 + c_val * exp(b_val * mc_value) * inter_quartile)
         
    } else {
        lower_value <- (q1 - c_val * exp(-b_val * mc_value) * inter_quartile)
        upper_value <- (q3 + c_val * exp(-a_val * mc_value) * inter_quartile)
        
    }
    
    if (lower_value < 0) lower_value <- 0
    replace_lower_values <- lower_value * 0.99
    replace_upper_values <- upper_value * 1.1
    #replace outliers by "standard boxplot" values 
    indx_l <- which(var < lower_value)
    indx_u <- which(var > upper_value)
    var[indx_l] <- replace_lower_values
    var[indx_u] <- replace_upper_values
    return(var)
}

handle_skewed_outliers <- function(dataset,sk_level=15,t_mult=5,c_level_value = 3,normal_sk_level = 2){
    basic_sk_level <- normal_sk_level
    t_value <- t_mult
    c_value <- c_level_value
    cat("handle_skewed_outliers values : sk_level = " , sk_level, " t_value = ", 
        t_value , " c_value = ", c_value , " normal_sk_level = " , normal_sk_level , "\n" )    
    feature_names <- names(dataset)
    feature.names_work <-   feature_names[! feature_names %in% c("ID","target")]
    for (var_name in feature.names_work){
        if(class(dataset[,var_name]) %in% c("integer","numeric")){
            var <- dataset[,var_name]
            skewness_level <- skewness(var,na.rm = TRUE)      
#            cat("var =", var_name, "skewness =",skewness_level,"\n")
            if(length(skewness_level) >0 & !is.na(skewness_level)) {
                if(abs(skewness_level) < basic_sk_level ){
                    dataset[,var_name] <- dataset[,var_name]
                } else if (abs(skewness_level) > basic_sk_level & abs(skewness_level) < sk_level){
                    dataset[,var_name] <- standard_boxplot(var,c_value)                    
                } else {
                    dataset[,var_name] <- adjusted_boxplot(var,c_value,a_val = -4, b_val = 3)
                }
            } else {
                cat("no skewness found for  variable =" ,var_name  , "\n")
                
            }
        } 
    }
    return(dataset)
} 


draw_vars <- function(var){
    
    p1 <- prop.table(table(train$target,train[,var]),margin=2)
    p1t <- t(p1)
    final_df <- as.data.frame.matrix(p1t)
    names(final_df) <- c("no","yes")
    if(class(train[,var]) %in% c("integer","numeric")){
        plot(final_df$yes,col="blue")
        print(table(train[,var]))
    } else if (class(train[,var]) == "factor") {
        final_df$factor_names <- rownames(final_df)
        long_df <- melt(final_df,id="factor_names")
        ggplot(data=long_df,aes(x=factor_names,y=value,group=variable)) + geom_line(aes(color=variable))
        
        
    }
}

