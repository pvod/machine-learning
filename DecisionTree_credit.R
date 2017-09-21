# Chapter 5 from MLwithR-Lantz

#data represents loan from credit agency

# Exploring data
credit=read.csv("credit.csv")
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default) #loan applicant who does not meet payment terms and goes under default

# Data Preparation
#First, credit data will be randomly ordered because we dont want training set or test set to be on small or big loans
set.seed(12345)
credit_random=credit[order(runif(1000)),]
#comparing the random order did not change summary statistics
summary(credit$amount)
summary(credit_random$amount)
#comparing first few values
head(credit$amount)
head(credit_random$amount)
#splitting data into training set and test set
credit_train=credit_random[1:900, ]
credit_test=credit_random[901:1000, ]

#install C5.0 package
install.packages("C50")
install.packages("partykit")
library(C50)

#removing 17th column called defaults and applying C5.0 model
## had to convert credit_train$default to factor as it reads as numeric
credit_model <- C5.0(credit_train[-17],as.factor(credit_train$default)) # diff from Mlantz book
credit_pred <- predict(credit_model, credit_test)
summary(credit_model)

# cross table to check prediction
install.packages("gmodels")
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#trying to improve model by adding trials parameter in boosting
credit_boost10 <- C5.0(credit_train[-17], as.factor(credit_train$default),
                       trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#still trying to improve model
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost

credit_cost <- C5.0(credit_train[-17], as.factor(credit_train$default),
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
