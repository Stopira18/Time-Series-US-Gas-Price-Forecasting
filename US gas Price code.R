

library(ggplot2)
library(pastecs)


library(sqldf)
#Load data

library(ROSE)


library(stats)
library(randomForest)

library(readr)
#train <- read_csv("C:/Users/abdou/OneDrive/Desktop/Data/archive (3)/train.csv")

train <- read_csv("C:/Users/Moussa/Downloads/train.csv")
train <-train[-c(1)]


#Inspect the shape of our data
dim(train)

#Inspect data types
str(train)
head(train)

summary(train)

sapply(train, typeof)
head(train,n=5)

#Check for null values
lapply(train,function(x) { 
  length(which(is.na(x)))})

# descriptive statistics
summary(train)
num_var<-cbind(train$Age, train$Region_Code,train$Annual_Premium,train$Vintage, train$Policy_Sales_Channel, train$Driving_License, train$Previously_Insured, train$Response)
options(scipen=100)
options(digits=2)
stat.desc(num_var)

summary(num_var)

prop.table(table(train$Response))
table(train$Vehicle_Age)
table(train$Vehicle_Damage)


# data visualization

ggplot(train, aes(Age,fill=Vehicle_Damage))+ geom_bar(stat="count",position='dodge', width = 0.5)+ 
  ggtitle("Age vs Vehicle Damage")

ggplot(train, aes(Gender,fill=Vehicle_Damage))+ geom_bar(stat="count",position='dodge', width = 0.5) + 
  labs(title="Gender vs Vehicle Damage") + theme_bw() + scale_fill_brewer()

ggplot(train, aes(x = `Response`)) +
  geom_bar()

ggplot(train, aes(Response, fill = Vehicle_Damage))+ geom_bar(stat="count",position='dodge', width = 0.5)+ 
  ggtitle("Response vs Vehicle Damage")

ggplot(train, aes(Response,fill=Vehicle_Age))+ geom_bar(stat="count",position='dodge', width = 0.5) +
  labs(title="Vehicle Age vs Response count") 

ggplot(train, aes(Response,fill=Vehicle_Damage))+ geom_bar(stat="count",position='dodge', width = 0.5) +
  labs(title="Vehicle_Damage vs Response count") 

ggplot(train, aes(Response,fill=Gender))+ geom_bar(stat="count",position='dodge', width = 0.5) +
  labs(title="Gender vs Response count") 

ggplot(train, aes(Response,fill=Previously_Insured))+ geom_bar(stat="count",position='dodge', width = 0.5) +
  labs(title="Previously_Insured vs Response count") 
# data visualization


ggplot(train, aes(x = `Response`)) +
  geom_bar() 

ggplot(train, aes(x = `Gender`)) +
  geom_bar() 

ggplot(train, aes(x = `Driving_License`)) +
  geom_bar() 

ggplot(train, aes(x = `Previously_Insured`)) +
  geom_bar() 

ggplot(train, aes(x = `Vehicle_Damage`)) +
  geom_bar() 

ggplot(train, aes(x = `Vehicle_Age`)) +
  geom_bar() 

ggplot(train, aes(Gender,fill=Vehicle_Damage))+ geom_bar(stat="count",position='dodge', width = 0.5) + labs(title="Gender vs Vehicle Damage") + theme_bw() + scale_fill_brewer()

cor(train)

new_data <- sqldf("select * from train")

new_data$Gender <- ifelse(new_data$Gender == "Male", 1, 0)
new_data$Vehicle_Damage <- ifelse(new_data$Vehicle_Damage == "Yes", 1, 0)
new_data$Vehicle_Age <- ifelse(new_data$Vehicle_Age == "> 2 Years", 2, ifelse(new_data$Vehicle_Age == "1-2 Year", 1, 0))
str(new_data)

new_data <-new_data[-c(1)]
cor(new_data)

set.seed(123)
t_split <- sample(nrow(new_data), .70*nrow(new_data))
train1 <- new_data[t_split,]
test1 <- new_data[-t_split,]

dim(train1)

model1 <- glm(Response ~.,family=binomial,data = train1)
summary(model1)

model2 <- glm(Response ~ Gender+Age+Driving_License+Previously_Insured
                        +Vehicle_Age+Vehicle_Damage+Annual_Premium+Policy_Sales_Channel,family=binomial,data =train1)
summary(model2)

testing_ins <- predict(model2, test1, type = "response")
hist(testing_ins)

Y_hat_mod2 <- as.numeric(testing_ins > 0.25)
table(test1$Response, Y_hat_mod2, dnn = c("Actual", "Predicted"))

accuracy <- mean(test1$Response == Y_hat_mod2)
accuracy

train1$Response <- as.factor(train1$Response)
test1$Response <- as.factor(test1$Response)
set.seed(500)
RF_Model = randomForest(Response~.,data=train1,ntree=10)
predicted_response = predict(RF_Model,test1)

test1$predicted_response = predicted_response

cm <-table(test1$Response,test1$predicted_response)
err_metric(cm)

Accuracy_Random_Forest = sum(diag(cm)/sum(cm))
Accuracy_Random_Forest
'
library(smotefamily)
table(train$Response)
prop.table(table(train$Response))

balancedata = SMOTE(new_data[c(-12)],  as.numeric(new_data[,12]),  K = 1, dup_size = 0)  # function parameters  
table(balancedata)
prop.table(table(balancedata$Response)) 

head(balancedata)
set.seed(123)
t_split <- sample(nrow(balancedata), .70*nrow(balancedata))

train1 <- balancedata[t_split,]
test1 <- balancedata[-t_split,]
'

#error metrics -- Confusion Matrix
err_metric=function(CM)
{
  TN =CM[1,1]
  TP =CM[2,2]
  FP =CM[1,2]
  FN =CM[2,1]
  precision =(TP)/(TP+FP)
  recall_score =(FP)/(FP+TN)
  
  f1_score=2*((precision*recall_score)/(precision+recall_score))
  accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
  False_positive_rate =(FP)/(FP+TN)
  False_negative_rate =(FN)/(FN+TP)
  
  print(paste("Precision value of the model: ",round(precision,2)))
  print(paste("Accuracy of the model: ",round(accuracy_model,2)))
  print(paste("Recall value of the model: ",round(recall_score,2)))
  print(paste("False Positive rate of the model: ",round(False_positive_rate,2)))
  
  print(paste("False Negative rate of the model: ",round(False_negative_rate,2)))
  
  print(paste("f1 score of the model: ",round(f1_score,2)))
}

Logm<- glm(Response ~ Gender+Age+Driving_License+Previously_Insured
              +Vehicle_Age+Vehicle_Damage+Annual_Premium+Policy_Sales_Channel,family=binomial,data =train1)
summary(Logm)

logit_P = predict(Logm , newdata = test1[-11] ,type = 'response' )
logit_P <- ifelse(logit_P > 0.25,1,0) # Probability check
CM= table(test1[,11] , logit_P)
print(CM)
err_metric(CM)
library(pROC)
roc_score=roc(test1[,11], logit_P) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")

set.seed(500)
RF_Model = randomForest(Response~.,data=train1,ntree=10)
summary(RF_Model)

predicted_response = predict(RF_Model,test1)

test1$predicted_response = predicted_response

cm <-table(test1$Response,test1$predicted_response)

Accuracy_Random_Forest = sum(diag(cm)/sum(cm))
Accuracy_Random_Forest


logit_P = predict(RF_Model , newdata = test1[-11] ,type = 'response' )
logit_P <- ifelse(logit_P > 0.5) # Probability check
CM= table(test1[,11] , logit_P)
print(CM)
err_metric(CM)
library(pROC)
roc_score=roc(test1[,11], logit_P) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")

over <- ovun.sample(Response ~., data = train, method = "over")$data
table(over$Response)


new_data <- sqldf("select * from over")

new_data$Gender <- ifelse(new_data$Gender == "Male", 1, 0)
new_data$Vehicle_Damage <- ifelse(new_data$Vehicle_Damage == "Yes", 1, 0)
new_data$Vehicle_Age <- ifelse(new_data$Vehicle_Age == "> 2 Years", 2, ifelse(new_data$Vehicle_Age == "1-2 Year", 1, 0))
str(new_data)

new_data <-new_data[-c(1)]
cor(new_data)

set.seed(123)
t_split <- sample(nrow(new_data), .70*nrow(new_data))
train1 <- new_data[t_split,]
test1 <- new_data[-t_split,]

dim(train1)

model1 <- glm(Response ~.,family=binomial,data = train1)
summary(model1)

Logm <- glm(Response ~ Gender+Age+Driving_License+Previously_Insured
              +Vehicle_Age+Vehicle_Damage+Annual_Premium+Policy_Sales_Channel,family=binomial,data =train1)
summary(model2)
Logm<- glm(Response ~ Gender+Age+Driving_License+Previously_Insured
           +Vehicle_Age+Vehicle_Damage+Annual_Premium+Policy_Sales_Channel,family=binomial,data =train1)
summary(Logm)

logit_P = predict(Logm , newdata = test1[-11] ,type = 'response' )
logit_P <- ifelse(logit_P > 0.25,1,0) # Probability check
CM= table(test1[,11] , logit_P)
print(CM)
err_metric(CM)
set.seed(500)
RF_Model = randomForest(Response~.,data=train1,ntree=10)
predicted_response = predict(RF_Model,test1)

test1$predicted_response = predicted_response

cm <-table(test1$Response,test1$predicted_response)
err_metric(cm)