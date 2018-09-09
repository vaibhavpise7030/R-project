# ** OBJECTIVE ** 
#1. Import the csv file with past 3 months history for infy share trend
train.data <- read.csv("D:vaibhav/data/Project/R_Credit_Risk_Train_data.csv")

test.data <- read.csv("C:vaibhav/data/Project/R_Credit_Risk_Test_data.csv")

train.data = na.exclude(train.data)
test.data = na.exclude(test.data)

train_subset<- train.data[c(2:13)]
test_subset<- train.data[c(2:13)]

#*********Model 1*****************

model1 <- glm(Loan_Status ~ ., family = binomial(link = 'logit'), data = train_subset) 

summary(model1)

anova(model1, test = 'Chisq')

log_predict1 <- predict(model1, newdata = test_subset, type = "response")
log_predict1 <- ifelse(log_predict1 > 0.5,1,0)

train.data$AllVarPred1_.5 <- log_predict1

conf <-table(train.data$Loan_Status, log_predict1, dnn = c("Actual", "Predicted"))
conf

#exp(coef(model1)) 

#*********Model 2*****************

model2 <- glm(Loan_Status ~ Credit_History+Property_Area+Married, family = binomial(link = 'logit'), data = train_subset)

summary(model2)

anova(model2, test = 'Chisq')

log_predict2 <- predict(model2,newdata = train_subset, type = "response")
log_predict2 <- ifelse(log_predict2 > 0.5,1,0)
log_predict2

train.data$Pred2_.5 <- log_predict2

View(train.data)


#******************************************************
conf <-table(train.data$Loan_Status, log_predict2, dnn = c("Actual", "Predicted"))
conf


# TP, FN, FP and TN using conf
TP <- conf[1, 1] # 
TN <- conf[2,2] #
FP <- conf[2,1] # 
FN <- conf[1, 2] #  

# Calculate and print the accuracy: acc
acc = (TP+TN)/(TP+FN+FP+TN)
print(acc)

# Calculate and print out the precision: prec 
prec = (TP)/(TP+FP)
print(prec)

# Calculate and print out the recall: rec (sensitivity)
rec = (TP)/(TP+FN)
print(rec)

#install.packages("ROCR")
library(ROCR)
p <- predict(model2, newdata = train_subset, type = "response")
pr <- prediction(p, train_subset$Loan_Status)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")

auc <- auc@y.values[[1]]
auc

