
library(caret)

data(BloodBrain)
bbbDescr <-bbbDescr[, -nearZeroVar(bbbDescr)]


ctrl1 <- trainControl(method = "cv", number = 3, verboseIter = TRUE)


set.seed(849)
reg01 <- train(bbbDescr[, 1:3], logBBB, "lm", preProc = "center", trControl = ctrl1)

set.seed(849)
reg02 <- train(bbbDescr[, 1:3], logBBB, "rf", preProc = "center", trControl = ctrl1)

set.seed(849)
reg03 <- train(bbbDescr[, 1:3], logBBB, "lmStepAIC", trControl = ctrl1, trace = 0)

set.seed(849)
reg04 <- train(bbbDescr[, 1:3], logBBB, "rvmRadial", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg05 <- train(bbbDescr[, 1:3], logBBB, "rvmPoly", trControl = ctrl1, preProc = c("center", "scale"), 
               tuneGrid = data.frame(.degree = 1, .scale = 0.01))

set.seed(849)
reg06 <- train(bbbDescr[, 1:3], logBBB, "rvmPoly", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg07 <- train(bbbDescr[, 1:3], logBBB, "treebag", trControl = ctrl1)

set.seed(849)
reg08 <- train(bbbDescr[, 1:3], logBBB, "knn", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg09 <- train(bbbDescr[, 1:3], logBBB, method = "rlm", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg10 <- train(bbbDescr[, 1:3], logBBB, method = "M5", trControl = ctrl1)


data(mdrr)

ctrl1 <- trainControl(method = "cv", number = 3, verboseIter = TRUE)

set.seed(849)
class01 <- train(mdrrDescr[, 1:3], mdrrClass, "glm", preProc = "center", trControl = ctrl1)

set.seed(849)
class02 <- train(mdrrDescr[, 1:3], mdrrClass, "rda", preProc = "center", trControl = ctrl1)

set.seed(849)
class03 <- train(mdrrDescr[, 1:3], mdrrClass, "rf", preProc = "center", trControl = ctrl1)

set.seed(849)
class04 <- train(mdrrDescr[, 1:3], mdrrClass, "glmStepAIC", trControl = ctrl1, trace = 0)

set.seed(849)
class05 <- train(mdrrDescr[, 1:3], mdrrClass, "treebag", trControl = ctrl1)

set.seed(849)
class06 <- train(mdrrDescr[, 1:3], mdrrClass, "knn", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
class07 <- train(mdrrDescr[, 1:3], mdrrClass, "lvq", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
class08 <- train(mdrrDescr[, 1:3], mdrrClass, "fda", trControl = ctrl1)

set.seed(849)
class09 <- train(mdrrDescr[, 1:3], mdrrClass, "lda", trControl = ctrl1)


ctrl2 <- trainControl(method = "cv", number = 3, verboseIter = TRUE, classProbs = TRUE,
                      summaryFunction = twoClassSummary)

set.seed(849)
classprob01 <- train(mdrrDescr[, 1:3], mdrrClass, "glm", preProc = "center", trControl = ctrl2, metric = "ROC")

set.seed(849)
classprob02 <- train(mdrrDescr[, 1:3], mdrrClass, "rda", preProc = "center", trControl = ctrl2, metric = "ROC")

set.seed(849)
classprob03 <- train(mdrrDescr[, 1:3], mdrrClass, "rf", preProc = "center", trControl = ctrl2, metric = "ROC")

set.seed(849)
classprob04 <- train(mdrrDescr[, 1:3], mdrrClass, "treebag", trControl = ctrl2, metric = "ROC")

set.seed(849)
classprob05 <- train(mdrrDescr[, 1:3], mdrrClass, "knn", trControl = ctrl2, preProc = c("center", "scale"), metric = "ROC")

set.seed(849)
classprob08 <- train(mdrrDescr[, 1:3], mdrrClass, "fda", trControl = ctrl2, preProc = c("center", "scale"), metric = "ROC")

set.seed(849)
classprob09 <- train(mdrrDescr[, 1:3], mdrrClass, "lda", trControl = ctrl2, preProc = c("center", "scale"), metric = "ROC")
