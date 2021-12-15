library(readxl)
Data<-read_excel('C:\\Users\\richa\\OneDrive\\Documents\\Final_Project_4C.xlsx')
Data
Data2 <- read_excel('C:\\Users\\richa\\OneDrive\\Documents\\Final_Project_Section_Split.xlsx')
Data2
typeof(Data2)

Data$NA_Section <- as.factor(Data$NA_Section)
Data$Pacific_Section <- as.factor(Data$Pacific_Section)
Data$Season <- as.factor(Data$Season)

Data2$NA_1 <- as.factor(Data2$NA_1)
Data2$NA_2 <- as.factor(Data2$NA_2)
Data2$NA_3 <- as.factor(Data2$NA_3)
Data2$NA_4 <- as.factor(Data2$NA_4)
Data2$NA_5 <- as.factor(Data2$NA_5)
Data2$NA_6 <- as.factor(Data2$NA_6)
Data2$NA_7 <- as.factor(Data2$NA_7)
Data2$NA_8 <- as.factor(Data2$NA_8)
Data2$NA_9 <- as.factor(Data2$NA_9)
Data2$NA_10 <- as.factor(Data2$NA_10)
Data2$P_1 <- as.factor(Data2$P_1)
Data2$P_2 <- as.factor(Data2$P_2)
Data2$P_3 <- as.factor(Data2$P_3)
Data2$P_4 <- as.factor(Data2$P_4)
Data2$P_5 <- as.factor(Data2$P_5)
Data2$P_6 <- as.factor(Data2$P_6)
Data2$P_7 <- as.factor(Data2$P_7)
Data2$P_8 <- as.factor(Data2$P_8)
Data2$P_9 <- as.factor(Data2$P_9)
Data2$P_10 <- as.factor(Data2$P_10)
Data2$Pacific_Section <- as.factor(Data$Pacific_Section)
Data2$Season <- as.factor(Data$Season)
quantile(Data2$Average_Prec)


Data2$Average_Prec<-ifelse(Data2$Average_Prec>=2.4737630 & Data2$Average_Prec<=4.9702105,4,Data2$Average_Prec)
Data2$Average_Prec<-ifelse(Data2$Average_Prec>=2.0618437 & Data2$Average_Prec<2.4737630,3,Data2$Average_Prec)
Data2$Average_Prec<-ifelse(Data2$Average_Prec>=1.6778981 & Data2$Average_Prec<2.0618437,2,Data2$Average_Prec)
Data2$Average_Prec<-ifelse(Data2$Average_Prec>=0.4 & Data2$Average_Prec<1.6778981,1,Data2$Average_Prec)
Data2$Average_Prec <- as.factor(Data2$Average_Prec)
Data2$Average_Prec
plot(Data2$Average_Prec)


#define matrix of predictor variables
x <- data.matrix(Data[, c('NA_Section', 'Pacific_Section', 'Month', 'Average_SST', 'Year', 'Season')])
y <- Data$Average_Prec

x1 <- data.matrix(Data2[, c('NA_1','NA_2','NA_3','NA_4','NA_5','NA_6','NA_7','NA_8','NA_9','NA_10',
                            'P_1', 'P_2', 'P_3', 'P_4', 'P_5', 'P_6', 'P_7', 'P_8', 'P_9', 'P_10',
                            'Month', 'Year', 'Average_SST', 'Season')])
x2 <- data.matrix(Data2[, c('NA_1','NA_2','NA_3','NA_4','NA_5','NA_6','NA_7','NA_8','NA_9','NA_10',
                             'P_2', 'P_3', 'P_4', 'P_7', 'P_8', 'P_9', 'P_10',
                            'Month', 'Year', 'Average_SST', 'Season')])

y1 <- Data2$Average_Prec

library(glmnet)
cv_model <- cv.glmnet(x1, y1, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model)
model_cv <- glmnet(x1, y, alpha = 1, lambda = best_lambda, standardize = TRUE)
coef(model_cv)

cv_model <- cv.glmnet(x2, y1, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model)
model_cv <- glmnet(x2, y1, alpha = 1, lambda = best_lambda, standardize = TRUE)
coef(model_cv)

Reduced_Data <- Data2[, -which(names(Data2) %in% c('P_1', 'P_5', 'P_6'))]
Reduced_Data

set.seed(1)
dt = sort(sample(nrow(Reduced_Data), nrow(Reduced_Data)*.7))
train<-Reduced_Data[dt,]
test<-Reduced_Data[-dt,]
train
test
Prec.test <- test$Average_Prec

library(tree)
tree.1 <- tree(Average_Prec~., train)
summary(tree.1)
cv.tree1 <- cv.tree(tree.1)
yhat <- predict(tree.1, newdata = test)
mean((yhat - Prec.test)^2)

library(randomForest)
set.seed(1)
mod_bag <- randomForest(Average_Prec~., train, mtry = 21)
yhat.bag <- predict (mod_bag , newdata = test)
mean((yhat.bag - Prec.test)^2)


mod_RF <- randomForest(Average_Prec~., train, importance = TRUE)
yhat.RF <- predict (mod_RF , newdata = test)
mean((yhat.RF - Prec.test)^2)
importance(mod_RF)
varImpPlot(mod_RF)


library(gbm)
set.seed(1)
mod_boost <- gbm(Average_Prec~., data = train,distribution = "gaussian" , n.trees = 5000,interaction.depth = 4)
summary(mod_boost)
yhat.boost <- predict (mod_boost , newdata = test)
mean((yhat.boost - Prec.test)^2)

typeof(Reduced_Data)
Reduced_Data <- data.frame(Reduced_Data)


library(BART)
x <- Reduced_Data[,2:22]
y <- Reduced_Data[,1]
xtrain <- train[, 2:22]
ytrain <- train[,1]
xtest <- test[,2:22]
ytest <- test[,1]
xtrain = as.data.frame(xtrain)
ytrain = as.matrix(ytrain)
xtest = as.matrix(xtest)
ytest = as.matrix(ytest)
typeof(ytrain)
nrow(xtrain)
nrow(ytrain)

set.seed(1)
mod.bart <- gbart(train[,2:22], train[,1], x.test = test[,2:22])
