#Computing ID: nb7ug
#Name: Niharika Reddy Bollumpally
#Competition 1: Titanic

library(tidyverse)

#Reading in the training and testing dataset
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

# Adding new column to differentiate between training and testing datset
train$Train <- TRUE
test$Train <- FALSE

#Adding an null column to test dataset to facilitate row-bind
test$Survived = NA
titanic <- rbind(train, test)
titanic$Pclass = factor(titanic$Pclass)
titanic$Embarked = factor(titanic$Embarked)
titanic$Sex = factor(titanic$Sex)
#Finding title in each name and making it a factor
titanic$Title <- gsub('(.*, )|(\\..*)', '', titanic$Name)
titanic$Title = factor(titanic$Title)
# assigning a new column with family size
titanic$Familysize = titanic$SibSp+titanic$Parch+1

#Finding the number of missing values for each column header
lapply(titanic, function(x) sum(is.na(x)))

"
PassengerId    Survived      Pclass        Name 
0              418           0           0 
Sex         Age       SibSp       Parch 
0           263           0           0 
Ticket        Fare       Cabin    Embarked 
0              1           0           0 
Train 
0
"

#Median of fares is assigned to missing fare value
titanic[is.na(titanic$Fare), "Fare"]<-median(titanic$Fare, na.rm = T)

# Predicting missing values of age using linear regression
train.age = titanic[!is.na(titanic$Age),]
test.age = titanic[is.na(titanic$Age),]

lm.age = lm(Age ~ poly(Fare,2), data = train.age)#using a regression model of the second order of Fare
summary(lm.age)

"
Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)      29.881      0.437  68.373  < 2e-16 ***
poly(Fare, 2)1   82.717     14.134   5.852 6.49e-09 ***
poly(Fare, 2)2  -43.386     14.134  -3.069   0.0022 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 14.13 on 1043 degrees of freedom
Multiple R-squared:  0.04019,	Adjusted R-squared:  0.03835 
F-statistic: 21.83 on 2 and 1043 DF,  p-value: 5.134e-10
"

#predicting missing values of age using the regression model developed above
test.age$Age=predict(lm.age, newdata = test.age)

#creating the full test dataset with no missing ages
titanic = rbind(train.age,test.age)


#Method 1: Logistic Regression
#splitting data into training and testing dataset
test <- titanic[titanic$Train =="FALSE",]
train <- titanic[titanic$Train =="TRUE",]
train$Survived= factor(train$Survived)


train.recs <- sample(1:891, size=623)#splitting by 70:30
titanic.train <- train[train.recs,]#creates the training dataset for cross validation
titanic.test <- train[-train.recs,]#creates the testing dataset for cross validation

#creates the logistic regression model
train.lm <- glm(Survived ~Pclass+Age+Sex, data=titanic.train, family = "binomial")

summary(train.lm)

probs<-as.vector(predict(train.lm,newdata=titanic.test, type="response"))
preds <- rep(0,268)  # Initialize prediction vector
preds[probs>0.5] <- 1
table(preds,titanic.test$Survived)
"
preds   0   1
    0 132  35
    1  20  81
"

#training the model on the full dataset
train.lm2 <- glm(Survived ~Pclass+Age+Sex, data=train, family = "binomial")
summary(train.lm2)


#making predictions using the above model
survival<-as.vector(predict(train.lm2,newdata=test, type="response"))
survived <- rep(0,418)  # Initialize prediction vector
survived[survival>0.5] <- 1

survival_prediction <- cbind(test,survived)


survival_prediction %>% 
  select(PassengerId,survived)->survive

write.csv(survive,"Titanic.csv", row.names = F)

#Method 2: Random Forest
install.packages("randomForest")

library(randomForest)
library(tidyverse)
rf_model <- randomForest(factor(Survived) ~ Pclass+Sex+Age+Familysize+Fare,data = train, ntree = 500, mtry = 3, nodesize = 0.01 *nrow(train))

test$Survived = predict( rf_model, newdata = test)#makes predictions using the random forst model


titanic1 = test %>% select(PassengerId, Survived)
write.csv(titanic1,"Titanic1.csv", row.names = F)#writes to csv file

