rm(list=ls())
# data1 <- read.csv("G:\\Srinath\\Datasets\\UniversalBank.csv")
data1 <- read.csv("D:\\DataScience_Drive\\universalBank.csv")

# length(unique(data1$ZIP.Code))
#data1$ZIP.Code <- NULL

str(data1)
data1$ID <- NULL
data1$ZIP.Code=NULL

data1$Personal.Loan <- as.factor(data1$Personal.Loan)
data1$Securities.Account <- as.factor(data1$Securities.Account)
data1$Family <- as.factor(data1$Family)
data1$CreditCard <- as.factor(data1$CreditCard)
data1$Education <- as.factor(data1$Education)
data1$Online = as.factor(data1$Online)
data1$CD.Account = as.factor(data1$CD.Account)
data1$ZIP.Code = as.factor(data1$ZIP.Code)

summary(data1)

data1$Mortgage1 = ifelse(data1$Mortgage==0,0,1)
data1$Mortgage1 = as.factor(data1$Mortgage1)


table(data1$Mortgage1,data1$Personal.Loan, dnn = c('Mortgage','Loan'))

quantile(data1$Mortgage[data1$Mortgage > 0])

#data1 = data1[,-c(1,2)]
#data1$Mortgage_Cat = ifelse(data1$Mortgage > 0 ,1,0)
#data1$Mortgage <- NULL
#data1$Mortgage_Cat = as.factor(data1$Mortgage_Cat)
#data1$Mortgage_Cat = NULL

data1$Mortgage = NULL
data1$ZIP.Code = NULL

head(data1)
names(data1)
# creating dummies
library(dummies)
data_for_dummies = dummy.data.frame(data1[,c(4,6)])
data_for_dummies = data_for_dummies[,-c(4,7)]


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

quantile(data1_dummy$Income,probs = seq(0,1,0.05))

summary(data1_dummy$Income[data1_dummy$Income < 145])
data1_dummy = data1_dummy[data1_dummy$Income < 145,]

summary(data1_dummy)
# scaling

for(i in 1:ncol(data1_dummy)){
  if(class(data1_dummy[,i]) %in% c('numeric','integer')){
    data1_dummy[,i] = (data1_dummy[,i] - min(data1_dummy[,i]))/(max(data1_dummy[,i])-min(data1_dummy[,i]))
  }
}

fnNormalize = function(df){
  for(i in 1:ncol(df)){
    if(class(df[,i]) %in% c('numeric','integer')){
      df[,i] = (df[,i] - min(df[,i]))/(max(df[,i])-min(df[,i]))
    }
  } 
  return(df)
}

data1_dummy = fnNormalize(data1_dummy)
fnNormalize = function(df){
  for(i in 1:ncol(df)){
    if(class(df[,i]) %in% c('numeric','integer')){
      df[,i] = (df[,i] - min(df[,i]))/(max(df[,i])-min(df[,i]))
    }
  }
  return(df)
}

data1_dummy_norm = fnNormalize(data1_dummy)


###
rows <- 1:nrow(data1_dummy)
set.seed(344)

train_rows<- sample(rows,3500)
test_rows <- rows[-train_rows]

train <- data1_dummy_norm[train_rows,]
test <- data1_dummy_norm[test_rows,]

prop.table(table(data1_dummy$Personal.Loan))
prop.table(table(train$Personal.Loan))
prop.table(table(test$Personal.Loan))



## library(class)
library(class)
output = knn(train, test,k=15, 
             cl= as.factor(train$Personal.Loan))

table(test$Personal.Loan,output,dnn=c('actuals','preds'))


