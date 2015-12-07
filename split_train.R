start_split_time <- Sys.time()

#split train into work and validate
cat("spliting train into work and validation by " , spl_ratio , "ratio \n") 
library(caTools)
set.seed(144)
spl = sample.split(train$target, SplitRatio = spl_ratio)
cat(table(spl),"\n")
train_work = subset(train, spl == TRUE)
train_validate = subset(train, spl == FALSE)

feature.names <- names(train)
remove <- c("ID","target")
feature.names <- feature.names[! feature.names %in% remove]
feature.names

end_split_time <- Sys.time()


