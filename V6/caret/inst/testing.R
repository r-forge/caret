
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

# set.seed(849)
# reg04 <- train(bbbDescr[, 1:3], logBBB, "rvmRadial", trControl = ctrl1, preProc = c("center", "scale"))
# 
# set.seed(849)
# reg05 <- train(bbbDescr[, 1:3], logBBB, "rvmPoly", trControl = ctrl1, preProc = c("center", "scale"), 
#                tuneGrid = data.frame(.degree = 1, .scale = 0.01))

set.seed(849)
reg06 <- train(bbbDescr[, 1:3], logBBB, "rvmLinear", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg07 <- train(bbbDescr[, 1:3], logBBB, "treebag", trControl = ctrl1)

set.seed(849)
reg08 <- train(bbbDescr[, 1:3], logBBB, "knn", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg09 <- train(bbbDescr[, 1:3], logBBB, method = "rlm", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg10 <- train(bbbDescr[, 1:3], logBBB, method = "M5", trControl = ctrl1)

# set.seed(849)
# reg11 <- train(bbbDescr[, 1:3], logBBB, method = "cubist", trControl = ctrl1)

set.seed(849)
reg12 <- train(bbbDescr[, 1:3], logBBB, method = "earth", trControl = ctrl1)

set.seed(849)
reg13 <- train(bbbDescr[, 1:3], logBBB, method = "gbm", trControl = ctrl1, verbose = FALSE)

set.seed(849)
reg14 <- train(bbbDescr[, 1:3], logBBB, method = "svmPoly", trControl = ctrl1)

set.seed(849)
reg15 <- train(bbbDescr[, 1:3], logBBB, method = "svmRadial", trControl = ctrl1)

set.seed(849)
reg16 <- train(bbbDescr[, 1:3], logBBB, method = "svmLinear", trControl = ctrl1)

set.seed(849)
reg17 <- train(bbbDescr[, 1:4], logBBB, method = "pcr", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg18 <- train(bbbDescr[, 1:4], logBBB, method = "pls", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg19 <- train(bbbDescr[, 1:4], logBBB, method = "simpls", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg20 <- train(bbbDescr[, 1:4], logBBB, method = "kernelpls", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg21 <- train(bbbDescr[, 1:4], logBBB, method = "widekernelpls", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg22 <- train(bbbDescr[, 1:50], logBBB, method = "leapForward", trControl = ctrl1, tuneLength = 45)

set.seed(849)
reg23 <- train(bbbDescr[, 1:50], logBBB, method = "leapBackward", trControl = ctrl1, tuneLength = 45)

set.seed(849)
reg24 <- train(bbbDescr[, 1:50], logBBB, method = "leapSeq", trControl = ctrl1, tuneLength = 45)

set.seed(849)
reg25 <- train(bbbDescr[, 4:8], logBBB, method = "bstTree", trControl = ctrl1)

set.seed(849)
reg26 <- train(bbbDescr[, 4:8], logBBB, method = "bstLs", trControl = ctrl1)

set.seed(849)
reg27 <- train(bbbDescr[, 4:8], logBBB, method = "bstSm", trControl = ctrl1)

set.seed(849)
reg28 <- train(bbbDescr[, 4:8], logBBB, method = "glmboost", trControl = ctrl1)

set.seed(849)
reg29 <- train(bbbDescr[, 4:8], logBBB, method = "gamboost", trControl = ctrl1)

set.seed(849)
reg30 <- train(bbbDescr[, 4:8], logBBB, method = "lars", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg31 <- train(bbbDescr[, 4:8], logBBB, method = "foba", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg32 <- train(bbbDescr[, 4:8], logBBB, method = "glmnet", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg33 <- train(bbbDescr[, 4:8], logBBB, method = "enet", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
reg34 <- train(bbbDescr[, 4:8], logBBB, method = "blackboost", trControl = ctrl1)

set.seed(849)
reg35 <- train(bbbDescr, logBBB, method = "lasso", trControl = ctrl1)

set.seed(849)
reg36 <- train(bbbDescr[, 4:8], logBBB, method = "partDSA", trControl = ctrl1)

set.seed(849)
reg37 <- train(bbbDescr[, 4:8], logBBB, method = "nnet", trControl = ctrl1,
               preProc = c("center", "scale"), trace = FALSE)

set.seed(849)
reg38 <- train(bbbDescr[, 4:8], logBBB, method = "avNNet", trControl = ctrl1,
               preProc = c("center", "scale"), trace = FALSE)

set.seed(849)
reg39 <- train(bbbDescr[, 4:8], logBBB, method = "pcaNNet", trControl = ctrl1,
               preProc = c("center", "scale"), trace = FALSE)


library(caret)

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

set.seed(849)
class10 <- train(mdrrDescr[, 1:3], mdrrClass, "LogitBoost", trControl = ctrl1)

set.seed(849)
class11 <- train(mdrrDescr[, 1:3], mdrrClass, "C5.0", trControl = ctrl1)

set.seed(849)
class12 <- train(mdrrDescr[, 1:3], mdrrClass, "earth", trControl = ctrl1)

set.seed(849)
class12 <- train(mdrrDescr[, 1:3], mdrrClass, "gbm", trControl = ctrl1, verbose = FALSE)

set.seed(849)
class13 <- train(mdrrDescr[, 1:3], mdrrClass, "svmPoly", trControl = ctrl1)

set.seed(849)
class14 <- train(mdrrDescr[, 1:3], mdrrClass, "svmRadial", trControl = ctrl1)

set.seed(849)
class15 <- train(mdrrDescr[, 1:3], mdrrClass, "svmLinear", trControl = ctrl1)

set.seed(849)
class16 <- train(mdrrDescr[, 1:3], mdrrClass, "pls", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
class17 <- train(mdrrDescr[, 1:3], mdrrClass, "simpls", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
class18 <- train(mdrrDescr[, 1:3], mdrrClass, "kernelpls", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
class19 <- train(mdrrDescr[, 1:3], mdrrClass, "widekernelpls", trControl = ctrl1, preProc = c("center", "scale"))

set.seed(849)
class20 <- train(mdrrDescr[, 1:3], mdrrClass, "bstTree", trControl = ctrl1)

set.seed(849)
class21 <- train(mdrrDescr[, 1:3], mdrrClass, "bstLs", trControl = ctrl1)

set.seed(849)
class22 <- train(mdrrDescr[, 1:3], mdrrClass, "bstSm", trControl = ctrl1)

set.seed(849)
class22 <- train(mdrrDescr[, 1:3], mdrrClass, "glmboost", trControl = ctrl1)

set.seed(849)
class23 <- train(mdrrDescr[, 1:3], mdrrClass, "gamboost", trControl = ctrl1)

set.seed(849)
class24 <- train(mdrrDescr[, 1:10], mdrrClass, "pam", trControl = ctrl1)

set.seed(849)
class25 <- train(mdrrDescr[, 1:10], mdrrClass, "pam", trControl = ctrl1)

set.seed(849)
class26 <- train(mdrrDescr[, 1:10], mdrrClass, "glmnet", trControl = ctrl1)

set.seed(849)
class27 <- train(mdrrDescr, mdrrClass, "blackboost", trControl = ctrl1, tuneLength = 5)

set.seed(849)
class28 <- train(mdrrDescr[, 1:10], mdrrClass, "partDSA", trControl = ctrl1, tuneLength = 5)

set.seed(849)
class29 <- train(mdrrDescr[, 1:10], mdrrClass, "ada", trControl = ctrl1)

set.seed(849)
class30 <- train(mdrrDescr[, 1:10], mdrrClass, "nnet", trControl = ctrl1,
                 preProc = c("center", "scale"), trace = FALSE)

set.seed(849)
class31 <- train(mdrrDescr[, 1:10], mdrrClass, "avNNet", trControl = ctrl1,
                 preProc = c("center", "scale"), trace = FALSE)

set.seed(849)
class32 <- train(mdrrDescr[, 1:10], mdrrClass, "pcaNNet", trControl = ctrl1,
                 preProc = c("center", "scale"), trace = FALSE)

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

set.seed(849)
classprob10 <- train(mdrrDescr[, 1:3], mdrrClass, "C5.0", trControl = ctrl2, preProc = c("center", "scale"), metric = "ROC")

set.seed(849)
classprob11 <- train(mdrrDescr[, 1:3], mdrrClass, "earth", trControl = ctrl2, metric = "ROC")

set.seed(849)
classprob12 <- train(mdrrDescr[, 1:3], mdrrClass, "gbm", trControl = ctrl2, metric = "ROC", verbose = FALSE)

set.seed(849)
classprob13 <- train(mdrrDescr[, 1:3], mdrrClass, "svmPoly", trControl = ctrl2, metric = "ROC")

set.seed(849)
classprob14 <- train(mdrrDescr[, 1:3], mdrrClass, "svmRadial", trControl = ctrl2, metric = "ROC")

set.seed(849)
classprob15 <- train(mdrrDescr[, 1:3], mdrrClass, "svmLinear", trControl = ctrl2, metric = "ROC")

set.seed(849)
classprob16 <- train(mdrrDescr[, 1:10], mdrrClass, "pls", trControl = ctrl2, metric = "ROC", preProc = c("center", "scale"))

set.seed(849)
classprob17 <- train(mdrrDescr[, 1:10], mdrrClass, "simpls", trControl = ctrl2, metric = "ROC", preProc = c("center", "scale"))

set.seed(849)
classprob18 <- train(mdrrDescr[, 1:10], mdrrClass, "kernelpls", trControl = ctrl2, metric = "ROC", preProc = c("center", "scale"))

set.seed(849)
classprob19 <- train(mdrrDescr[, 1:10], mdrrClass, "widekernelpls", trControl = ctrl2, metric = "ROC", preProc = c("center", "scale"))

set.seed(849)
classprob20 <- train(mdrrDescr[, 1:10], mdrrClass, "glmboost", trControl = ctrl2, metric = "ROC", preProc = c("center", "scale"))

set.seed(849)
classprob21 <- train(mdrrDescr[, 1:10], mdrrClass, "gamboost", trControl = ctrl2, metric = "ROC", preProc = c("center", "scale"))

set.seed(849)
classprob22 <- train(mdrrDescr[, 1:10], mdrrClass, "pam", trControl = ctrl2, metric = "ROC", preProc = c("center", "scale"))

set.seed(849)
classprob23 <- train(mdrrDescr[, 1:10], mdrrClass, "glmnet", trControl = ctrl2, metric = "ROC", preProc = c("center", "scale"))

set.seed(849)
classprob24 <- train(mdrrDescr, mdrrClass, "blackboost", trControl = ctrl2, metric = "ROC", tuneLength = 3)

set.seed(849)
classprob25 <- train(mdrrDescr[, 1:10], mdrrClass, "ada", trControl = ctrl2, metric = "ROC")

set.seed(849)
classprob26 <- train(mdrrDescr[, 1:10], mdrrClass, "nnet", trControl = ctrl2, 
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     trace = FALSE)

set.seed(849)
classprob27 <- train(mdrrDescr[, 1:10], mdrrClass, "avNNet", trControl = ctrl2, 
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     trace = FALSE)

set.seed(849)
classprob28 <- train(mdrrDescr[, 1:10], mdrrClass, "pcaNNet", trControl = ctrl2, 
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     trace = FALSE)