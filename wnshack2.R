#==========================================================================================================

#Reading the comet data.
wns = read.csv('C:\\Users\\Kalyan Rohith T G\\Downloads\\train_LZdllcl.csv')
wnst = read.csv('C:\\Users\\Kalyan Rohith T G\\Downloads\\test_2umaH9m.csv')
#Checking the dimension of the dataset
dim(wns)
#So the given data set has 1484 rows and 10 columns.
names(wnst)
str(wns)

#Observing the dataset
head(wns)
summary(wns)

#Checking the class imbalance in the response variable
table(wns$is_promoted)
#So in the response variable 
#negatives = 1055 and 
#positives = 429


#Checking for missing values
wns1 = wns[complete.cases(wns),]
#checking the dimensions of the data without missing values
dim(wns)
#There are 1392 rows and 10 columns

#Identifying no. of missing values in each variable
summary(wns)
#When we look at the summary of our dataset we can see that,
#Variable 'previous_year_rating' has "4124" missing values

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Missing values treatment
#As the variable "A" is continuous, imputing with mean
wns$previous_year_rating[is.na(wns$previous_year_rating)] <- getmode(wns$previous_year_rating)

wnst$previous_year_rating[is.na(wnst$previous_year_rating)] <- getmode(wnst$previous_year_rating)
summary(wnst)


#t-test for numerical variables
#========================================
importance = c()

for(i in 2:ncol(wns))
{
  if(!is.factor(wns[,i]))
  {
    varname = names(wns)[i]
    
    p_value = t.test(wns[,varname] ~ wns$is_promoted)$p.value
    
    importance[varname] = round(p_value,5)
  }
}
(importance)

#Based on t-test,"no_of_trainings", "age","length_of_service","avg_training_score", 
#"awards_won.", "previous_year_rating" all variables turned out as important
cata = wns[c(7, 8, 9, 10, 11, 12, 13)]
#Using IV to understand the imporance of the categorical variables
#==========================================================
library(InformationValue)
for(i in 2:ncol(wns))
{
  if(is.factor(wns[,i]))
  {
    varname = names(wns)[i]
    
    print(varname)
    print(IV(X=factor(wns[,varname]), Y=wns$is_promoted))
    
  }
}
#department, education, gender, recruitment_channel are important
#region is unimportant
#important in categorical variables
ChisqTest <- function(data,var,responseIndex)
{
  o = data.frame()
  x=1
  for (i in var) {
    if (length(unique(data[,i]))>1) {
      o[x,1] = variable.names(data[i])
      o[x,2] = chisq.test(data[,i],data[,responseIndex])$p.value
      x=x+1
    }
  }
  return(o)
}
ChisqTest(wns,cata,1)

#Variable selection
names(wns)
wns_new = wns[c(2, 9, 10, 11, 12, 13, 14)]
x = model.matrix(wns_new$is_promoted ~ ., wns_new)[,-1] 
#model.matrix automatically creates dummy variables for factor variables
dim(x)

#target variable
y = wns_new$is_promoted
length(y)


#Fitting the logistic regression model

#importing the packages
library(caTools)

#splitting the data
#===============================================
set.seed(88)
split <- sample.split(wns_new$is_promoted, SplitRatio = 0.70)
train <- subset(x, split == TRUE)
test <- subset(x, split == FALSE)

#model fitting
#=============================================
model <- glm (is_promoted ~ ., family = binomial, data = data.frame(is_promoted = y, x))

colnames(x)

#logistic model summary
summary(model)

#predicting Comets
#==================================
names(wns)
wnst_new = wnst[c(2, 9, 10, 11, 12, 13)]
#wnst_new$is_promoted = ifelse(wns_new$is_promoted == 'positive',1,0)
#x1 = model.matrix(wnst_new)[,-1]
x1 = model.matrix(~ ., wnst_new)[,-1] 
x1 = data.frame(x1)
predictc <- predict(model, newdata = x1, type = 'response')
head(predictc)

#Importing library caret for calculating the evaluation metrics
library(caret)

#specifying the probability criteria for classification 1 and 0
pred1 = ifelse((predictc > 0.4),'1','0')
table(wns_new$is_promoted, pred1)
#table(wns_new$is_promoted, predictc)
#creating new target variable as we need 1 and o in our response variable
#wns_new$Target = ifelse(wns_new$is_promoted == 'positive',1,0)

#converting into factors
pred1 = factor(pred1)
wns_new$is_promoted = factor(wns_new$is_promoted)

#Confusion matrix
confusionMatrix(wns_new$is_promoted,pred1)
#sensitivity
caret::sensitivity(wns_new$is_promoted, pred1)
#Specificity
caret::specificity(wns_new$is_promoted, pred1)
#Precision
b = caret::precision(wns_new$is_promoted, pred1)
b
#recall
a = caret::recall(wns_new$is_promoted, pred1)
a
#F1 score
F1 = ((2*a*b)/sum(a,b))
F1

#plotting roc curve
#Importing ROCR package
library(ROCR)
ROCRpred <- prediction(predictc, y)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#calculating auroc
#Importing pROC package
library(pROC)
auc(y, predictc)
#Area under the curve: 0.9624

prediction2 = data.frame(wnst$employee_id, pred1)

write.csv(prediction2, "C://Users//Kalyan Rohith T G//Documents//prediction8.csv")

#Random forest classifier
library(randomForest)
rd <- randomForest(is_promoted ~ department + education + gender + recruitment_channel + no_of_trainings + age + previous_year_rating + length_of_service + KPIs_met..80. + awards_won. + avg_training_score, data = wns_new)
summary(rd)
print(rd) 


#predicting churn
predict2 <- predict(rd, type = 'response')
head(predict2)

#sensitivity
table(ch_new$Churn, predict2 > 0.4)


#Sensitivity
sensitivity(ch_new$Churn, predict2)

#Specificity
specificity(ch_new$Churn, predict2)


#Precision
precision(ch_new$Churn, predict2)


#Mis-classification Error
misClassError(ch_new$Churn, predict2)

#Accuracy
accuracy = 1- misClassError(ch_new$Churn, predict2)
accuracy

#plotting roc curve
library(ROCR)
ROCRpred <- prediction(predict2, y)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#calculating auroc
library(pROC)
auc(y, predict2)

