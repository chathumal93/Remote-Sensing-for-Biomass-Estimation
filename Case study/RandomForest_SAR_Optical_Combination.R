#Import and install necessary libraries
library(raster)
library(rgdal)
#install.packages("randomForest")
library(randomForest)
library(caret)
#install.packages("CAST")
library(CAST)

# Read Data
setwd("F:\\1_A_biomass\\final_processing\\snap_batch\\csv for RF")

data <- read.csv("combine.csv", header = TRUE)

# Data Partition

set.seed(12)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.75, 0.25))
train <- data[ind==1,]
test <- data[ind==2,]

# Random Forest
library(randomForest)
set.seed(2122)
rf <- randomForest(AGB~., data=train,
                   ntree = 200,
                   mtry = 5,
                   importance = TRUE,
                   proximity = TRUE)
print(rf)

## Prediction & Confusion Matrix - train data
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



## Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)

#Getting correlation coef
cor.test(p2, test$AGB)
plot(p2, test$AGB)

plotPoints(a~b,data = p23,ylab = "Predicted AGB Value",xlab = "Measured AGB Value")

f2=fitModel(a~A+B*b,data = p23)
plotFun(f2(b)~b,xlim=range(0,500),ylim=range(0,500),add = TRUE, col='gray50')


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

# Error rate of Random Forest
plot(rf)


# Tune mtry
t <- tuneRF(train[,-35], train[,35],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 200,
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
------------------------------------------------------------------------------------------------------------------------
## Application of the selected model for the raster image set

#Getting all the rasters  according to the variables
bands<- list.files(path="F:\\1_A_biomass\\final_processing\\snap_batch\\final_2_folders\\518new",
                   pattern =".tif$",all.files=TRUE,full.names=TRUE)
bands<-lapply(bands, raster)


#Creating a band composite including all variable rasters
logo <- brick(bands)
names(logo)

## note the additional argument "type='response'" that is 
## passed to predict.randomForest

#ALOS scene 518
com_bio18 <- predict(logo, rf, type='response', progress='window')
plot(com_bio18)

writeRaster(com_bio18,filename="F:\\1_A_biomass\\final_processing\\snap_batch\\final_rf_biomass\\biomass_com_518.tif", 
            format="GTiff", overwrite=TRUE)

  
#ALOS scene 519  
com_bio19 <- predict(logo19, rf, type='response', progress='window')
plot(com_bio19)
writeRaster(com_bio19,filename="F:\\1_A_biomass\\final_processing\\snap_batch\\final_rf_biomass\\biomass_com_519.tif", 
            format="GTiff", overwrite=TRUE)  
  
#Getting the mosaiced raster  
biomass_com=raster("F:\\1_A_biomass\\final_processing\\snap_batch\\final_rf_biomass\\biomass_combine.tif")  

#Classification of the biomass raster
reclass_df <- c(0, 50 , 1,
                50, 100 , 2,
                100,150 , 3,
                150,200 , 4,
                200,250 , 5,
                250,300 , 6,
                300,350 , 7,
                350,400 , 8,
                400,1000 , 9)

reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)

biomass_classified <- reclassify(biomass_com,reclass_m)

#Ourput the results
writeRaster(biomass_classified,filename="F:\\1_A_biomass\\final_processing\\snap_batch\\final_rf_biomass\\biomass_combinel_classi.tif" ,
            format="GTiff", overwrite=TRUE)






  
  