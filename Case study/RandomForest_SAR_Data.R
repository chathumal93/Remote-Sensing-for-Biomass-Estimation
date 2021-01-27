#Import and install necessary libraries
#install.packages("randomForest")
library(randomForest)
library(caret)
#install.packages("CAST")
library(CAST)

#Read Data
setwd("F:\\1_A_biomass\\final_processing\\snap_batch\\csv for RF")
data <- read.csv("rad.csv", header = TRUE)

#install.packages("randomForest")

#Data Partition
set.seed(12)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.75, 0.25))
train <- data[ind==1,]
test <- data[ind==2,]

# Random Forest
library(randomForest)
set.seed(2122)
rf <- randomForest(AGB~., data=train,
                   ntree = 300,
                   mtry = 3,
                   importance = TRUE,
                   proximity = TRUE)
print(rf)
attributes(rf)

##Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
#Getting correlation coef
cor.test(p1, train$AGB)

#Dataframe geration for result analysis
p222<-data.frame(p1)

p233<-data.frame(cbind(p222$p1,train$AGB ))
colnames(p233)=c("a","b")

#Calculation of RMSE
RMSE <- sqrt(sum(( p233$b-p233$a)^2)/155)
RMSE
#Calculation of MAE
MAE<-sum(abs(p233$b-p233$a))/155
MAE  

##Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
#Getting correlation coef
cor.test(p2, test$AGB)
plot(p2, test$AGB)

#Dataframe geration for result analysis
p22<-data.frame(p2)
p23<-data.frame(cbind(p22$p2,test$AGB ))

#write.csv(p23,"F:\\1_A_biomass\\final_processing\\snap_batch\\csv for RF\\test.csv")
colnames(p23)=c("a","b")

#Calculation of RMSE
RMSE <- sqrt(sum( (p23$b-p23$a)^2)/53)
RMSE

#Calculation of MAE
MAE<-sum(abs(p23$b-p23$a))/53
MAE  

#Plotting the model
plot(rf)

# Tune mtry
t <- tuneRF(train[,-24], train[,24],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 250,
            trace = TRUE,
            improve = 0.05)

# No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

#Geting the variable importance
importance(rf)

#Geting the variables used
varUsed(rf)


