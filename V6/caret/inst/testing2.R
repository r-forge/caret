
suppressMessages(library(caret))

data(BloodBrain)
bbbDescr <-bbbDescr[, -nearZeroVar(bbbDescr)]


ctrl1 <- trainControl(method = "cv", number = 3, verboseIter = TRUE)


set.seed(849)
reg01 <- train(bbbDescr[, 4:8], logBBB, method = "ctree", trControl = ctrl1,
               preProc = c("center", "scale"))

data(mdrr)

ctrl1 <- trainControl(method = "cv", number = 3, verboseIter = TRUE)

set.seed(849)
class01 <- train(mdrrDescr[, 1:3], mdrrClass, "ctree", preProc = "center", trControl = ctrl1)

ctrl2 <- trainControl(method = "cv", number = 3, verboseIter = TRUE, classProbs = TRUE,
                      summaryFunction = twoClassSummary)

set.seed(849)
# classprob01 <- train(mdrrDescr[, 1:3], mdrrClass, "ctree", preProc = "center", trControl = ctrl2, metric = "ROC")
