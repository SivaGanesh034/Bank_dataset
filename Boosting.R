rm(list=ls())
# data1 <- read.csv("G:\\Srinath\\Datasets\\UniversalBank.csv")
data1 <- read.csv("D:\\DataScience_Drive\\universalBank.csv")
View(data1)
str(data1)
# length(unique(data1$ZIP.Code))
data1$ZIP.Code <- NULL

str(data1)
data1$ID <- NULL


data1$Personal.Loan <- as.factor(data1$Personal.Loan)
data1$Securities.Account <- as.factor(data1$Securities.Account)
data1$Family <- as.factor(data1$Family)
data1$CreditCard <- as.factor(data1$CreditCard)
data1$Education <- as.factor(data1$Education)
data1$Online = as.factor(data1$Online)
data1$CD.Account = as.factor(data1$CD.Account)
#data1$ZIP.Code = as.factor(data1$ZIP.Code)

summary(data1)



#table(data1$Mortgage1,data1$Personal.Loan, dnn = c('Mortgage','Loan'))

quantile(data1$Mortgage[data1$Mortgage > 0])

#data1 = data1[,-c(1,2)]
#data1$Mortgage_Cat = ifelse(data1$Mortgage > 0 ,1,0)
#data1$Mortgage <- NULL
#data1$Mortgage_Cat = as.factor(data1$Mortgage_Cat)
#data1$Mortgage_Cat = NULL

#data1$Mortgage = NULL
#data1$ZIP.Code = NULL

head(data1)
names(data1)
# creating dummies
library(dummies)
data_for_dummies = dummy.data.frame(data1[,c(4,6)])
data_for_dummies = data_for_dummies[,-c(4,6)]


data1_dummy = cbind(data1[,-c(4,6)],data_for_dummies)

data1_dummy$Personal.Loan = as.numeric(as.character(data1_dummy$Personal.Loan))
data1_dummy$Mortgage1 = as.numeric(as.character(data1_dummy$Mortgage1))
data1_dummy$CD.Account = as.numeric(as.character(data1_dummy$CD.Account))
data1_dummy$Online = as.numeric(as.character(data1_dummy$Online))
data1_dummy$CreditCard = as.numeric(as.character(data1_dummy$CreditCard))
data1_dummy$Securities.Account = as.numeric(as.character(data1_dummy$Securities.Account))

str(data1_dummy)
###
data1_dummy = data1_dummy[data1_dummy$Experience>=0,]
hist(data1_dummy$Income)

summary(log(data1_dummy$Income))

# quantile(data1_dummy$Income,probs = seq(0,1,0.05))
# 
# summary(data1_dummy$Income[data1_dummy$Income < 145])
# data1_dummy = data1_dummy[data1_dummy$Income < 145,]
# 
# summary(data1_dummy)
# # scaling
# 
# for(i in 1:ncol(data1_dummy)){
#   if(class(data1_dummy[,i]) %in% c('numeric','integer')){
#     data1_dummy[,i] = (data1_dummy[,i] - min(data1_dummy[,i]))/(max(data1_dummy[,i])-min(data1_dummy[,i]))
#   }
# }
# 
# fnNormalize = function(df){
#   for(i in 1:ncol(df)){
#     if(class(df[,i]) %in% c('numeric','integer')){
#       df[,i] = (df[,i] - min(df[,i]))/(max(df[,i])-min(df[,i]))
#     }
#   } 
#   return(df)
# }
# 
# data1_dummy = fnNormalize(data1_dummy)
# fnNormalize = function(df){
#   for(i in 1:ncol(df)){
#     if(class(df[,i]) %in% c('numeric','integer')){
#       df[,i] = (df[,i] - min(df[,i]))/(max(df[,i])-min(df[,i]))
#     }
#   }
#   return(df)
# }
# 
# data1_dummy_norm = fnNormalize(data1_dummy)
# 
# 
###
rows <- 1:nrow(data1_dummy)
set.seed(344)

train_rows<- sample(rows,3500)
test_rows <- rows[-train_rows]

train <- data1_dummy[train_rows,]
test <- data1_dummy[test_rows,]

train$Personal.Loan = as.factor(train$Personal.Loan)
##
library(adabag)
adaboost = boosting(Personal.Loan ~ .,data = train,
                    mfinal = 75)

preds = predict(adaboost,test)
table(test$Personal.Loan,preds$class,dnn = c('acts','preds'))
head(preds)

### gbm
train$Personal.Loan = as.numeric(as.character(train$Personal.Loan))

library(gbm)
gbmmodel = gbm(Personal.Loan ~ .,data = train,
               distribution = 'huberized',
               n.trees = 4000,
              interaction.depth = 6,
              bag.fraction = 0.8,
              n.minobsinnode = 5,
              verbose = T)

preds = predict(gbmmodel,test,n.trees = 4000,type = 'response')
preds_class = ifelse(preds >0.4,1,0)

table(test$Personal.Loan, preds_class,dnn = c('acts','preds'))
# gbm(formula = formula(data),
#     distribution = "bernoulli",
#     data = list(),
#     weights,
#     var.monotone = NULL,
#     n.trees = 100,
#     interaction.depth = 1,
#     n.minobsinnode = 10,
#     shrinkage = 0.001,
#     bag.fraction = 0.5,
#     train.fraction = 1.0,
#     cv.folds=0,
#     keep.data = TRUE,
#     verbose = "CV",
#     class.stratify.cv=NULL,
#     n.cores = NULL)
gbmCrossVal()

