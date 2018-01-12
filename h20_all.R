rm(list = ls())

setwd("C:/Users/Moose/Desktop/Brainwaves/bw2")
options(scipen = 999)
library(data.table)
df = fread("train.csv", header = T , stringsAsFactors = FALSE, check.names = FALSE,na.strings=c("","NA"))
#df = df[1:1000,]
sapply(df,function(x) sum(is.na(x)))

df$cat_var_3 = NULL
df$transaction_id = NULL

mode =as.data.frame(sort( table(df$cat_var_1), decreasing = T))
df$cat_var_1[is.na(df$cat_var_1)] = "gf"

mode =as.data.frame(sort( table(df$cat_var_8), decreasing = T))
df$cat_var_8[is.na(df$cat_var_8)] = "dn"

df = as.data.frame(df)
df[,8:49] = lapply(df[,8:49], function(x) as.factor(x))
df = as.data.frame(df)


#character_vars <- lapply(df, class) == "character"
#df[, character_vars] <- lapply(df[, character_vars], as.factor)



library(h2o)
h2o.init()
df  = as.h2o(df)
drf = h2o.randomForest (y = "target",
                        training_frame    = df,
                        ntrees            = 100,
                        max_depth         = 20)

summary(drf)



#summary of the model
summary(model)

library(data.table)
df = fread("test.csv", header = T , stringsAsFactors = FALSE, check.names = FALSE,na.strings=c("","NA"))

sapply(df,function(x) sum(is.na(x)))

df$cat_var_3 = NULL
df$transaction_id = NULL

mode =as.data.frame(sort( table(df$cat_var_1), decreasing = T))
df$cat_var_1[is.na(df$cat_var_1)] = "gf"

mode =as.data.frame(sort( table(df$cat_var_8), decreasing = T))
df$cat_var_8[is.na(df$cat_var_8)] = "dn"


mode =as.data.frame(sort( table(df$cat_var_6), decreasing = T))
df$cat_var_6[is.na(df$cat_var_6)] = "zs"

df = as.data.frame(df)
df[,8:48] = lapply(df[,8:48], function(x) as.factor(x))
df = as.data.frame(df)

# character_vars <- lapply(df, class) == "character"
# df[, character_vars] <- lapply(df[, character_vars], as.factor)

df= as.h2o(df)
pred = h2o.predict(drf, df)

pred = as.data.frame(pred)
df = as.data.frame(df)

df = fread("test.csv", header = T , stringsAsFactors = FALSE, check.names = FALSE,na.strings=c("","NA"))

output = as.data.frame(cbind(df$transaction_id,pred$p1))
names(output)[1] ="transaction_id" 
names(output)[2] ="target" 

write.csv(output,"output_xgb_all.csv",row.names = F) 


