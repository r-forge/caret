library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "krlsRadial"
tests <- c("test_reg_cv_model", "test_reg_pred", "test_reg_loo_model")

#########################################################################

data(BloodBrain)
bbbDescr <-bbbDescr[, -nearZeroVar(bbbDescr)]
bbbDescr <-bbbDescr[, -findCorrelation(cor(bbbDescr), .5)]

set.seed(2)

inTrain <- createDataPartition(logBBB, p = .50)
trainX <-bbbDescr[inTrain[[1]], ]
trainY <- logBBB[inTrain[[1]]]
testX <- bbbDescr[-inTrain[[1]], ]
testY <- logBBB[-inTrain[[1]]]

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, method = "krlsRadial", trControl = rctrl1,
                           preProc = c("center", "scale"), print.level = 0)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, method = "krlsRadial", trControl = rctrl2,
                            preProc = c("center", "scale"), print.level = 0)

#########################################################################

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


