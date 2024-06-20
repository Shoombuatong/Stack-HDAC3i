Class = read.csv("Dataset.csv" , header = TRUE)

cctrl <-trainControl(method = "cv", number = 10, classProbs =  TRUE)
Reall <- data.frame()
k = 10

#########################################################

GridSVMP <- expand.grid(degree = c(1, 2, 3, 4), scale = 0.01, C = 0.25 )

GridRF <- expand.grid(.mtry=c(10), .ntree=c(20, 50, 100, 200, 300))

GridXGB <- expand.grid(nrounds = c(20, 50, 100, 200, 300),
                       max_depth = c(3, 5, 7, 10, 15),
                       eta = 0.1, gamma = 0, colsample_bytree = .7, min_child_weight = 3, subsample = 0.8)

GridGBM <- expand.grid(interaction.depth = c(3, 5, 7) ,n.trees = c(50, 100, 200, 300),
                       shrinkage = .1, n.minobsinnode = 10)


#########################################################
Cdes = read.csv("AP2D.csv", header = TRUE)
D = data.frame(scale(Cdes[, -1]) , Activity = Class[,4])
D = remove_empty(D, "cols")
active   <- subset(D, Activity == 'active')
inactive <- subset(D, Activity == 'inactive')
ntr1 <- floor(0.8 * nrow(active))
ntr2 <- floor(0.8 * nrow(inactive))
set.seed(123)
##train_ind1 <- sample(seq_len(nrow(active)), size = ntr1)
##train_ind2 <- sample(seq_len(nrow(inactive)), size = ntr2)
trpos <- active[train_ind1, ]
tspos <- active[-train_ind1, ]
trneg <- inactive[train_ind2, ]
tsneg <- inactive[-train_ind2, ]
Dtr = rbind(trpos, trneg)
Dts = rbind(tspos, tsneg)
##id <- sample(1:k,nrow(Dtr),replace=TRUE)

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method = "pls", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)
F1CV = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

#########################################################
M <- train(Activity ~ ., data = Dtr,  method = "pls", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))
F1IND = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

Re = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, F1CV, ACCIND, SNIND, SPIND, MCCIND, AUCIND, F1IND)
Reall <- rbind(Reall, data.frame(Re))

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train, method = "glmboost", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)
F1CV = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

#########################################################
M <- train(Activity ~ ., data = Dtr, method = "glmboost", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))
F1IND = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

Re = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, F1CV, ACCIND, SNIND, SPIND, MCCIND, AUCIND, F1IND)
Reall <- rbind(Reall, data.frame(Re))


#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method = "knn", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)
F1CV = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

#########################################################
M <- train(Activity ~ ., data = Dtr,  method = "knn", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))
F1IND = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

Re = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, F1CV, ACCIND, SNIND, SPIND, MCCIND, AUCIND, F1IND)
Reall <- rbind(Reall, data.frame(Re))

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train, method = "mlp", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)
F1CV = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

#########################################################
M <- train(Activity ~ ., data = Dtr, method = "mlp", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))
F1IND = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

Re = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, F1CV, ACCIND, SNIND, SPIND, MCCIND, AUCIND, F1IND)
Reall <- rbind(Reall, data.frame(Re))

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train, method = "svmRadial", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)
F1CV = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

#########################################################
M <- train(Activity ~ ., data = Dtr, method = "svmRadial", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))
F1IND = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

Re = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, F1CV, ACCIND, SNIND, SPIND, MCCIND, AUCIND, F1IND)
Reall <- rbind(Reall, data.frame(Re))

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train, method = "svmPoly", tuneGrid = GridSVMP, trControl = cctrl, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)
F1CV = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

#########################################################
M <- train(Activity ~ ., data = Dtr, method = "svmPoly", tuneGrid = GridSVMP, trControl = cctrl, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))
F1IND = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

Re = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, F1CV, ACCIND, SNIND, SPIND, MCCIND, AUCIND, F1IND)

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train, method = "xgbTree", tuneGrid = GridXGB, trControl = cctrl, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)
F1CV = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

#########################################################
M <- train(Activity ~ ., data = Dtr, method = "xgbTree", tuneGrid = GridXGB, trControl = cctrl, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))
F1IND = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

Re = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, F1CV, ACCIND, SNIND, SPIND, MCCIND, AUCIND, F1IND)
Reall <- rbind(Reall, data.frame(Re))

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train, method = "gbm", tuneGrid = GridGBM, trControl = cctrl, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)
F1CV = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

#########################################################
M <- train(Activity ~ ., data = Dtr, method = "gbm", tuneGrid = GridGBM, trControl = cctrl, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))
F1IND = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

Re = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, F1CV, ACCIND, SNIND, SPIND, MCCIND, AUCIND, F1IND)
Reall <- rbind(Reall, data.frame(Re))

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train, method=  customRF, tuneGrid = GridRF, trControl = cctrl, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)
F1CV = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

#########################################################
M <- train(Activity ~ ., data = Dtr, method=  customRF, tuneGrid = GridRF, trControl = cctrl, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))
F1IND = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

Re = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, F1CV, ACCIND, SNIND, SPIND, MCCIND, AUCIND, F1IND)
Reall <- rbind(Reall, data.frame(Re))
