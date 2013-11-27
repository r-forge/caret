library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "treebag"
tests <- c("test_class_cv_model", "test_class_pred", "test_class_prob",
           "test_class_loo_model", "test_levels", "test_reg_cv_model",
           "test_reg_loo_model", "test_reg_oob_model", "test_class_oob_model")

#########################################################################

set.seed(2)
training <- twoClassSim(100)
testing <- twoClassSim(500)
trainX <- training[, -ncol(training)]
trainY <- training$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl3 <- trainControl(method = "oob")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "treebag", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             nbagg = 7)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "treebag", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              nbagg = 7)
test_levels <- levels(test_class_cv_model)

set.seed(849)
test_class_oob_model <- train(trainX, trainY, 
                              method = "treebag", 
                              trControl = cctrl3,
                              nbagg = 7,
                              keepX = TRUE)

#########################################################################

data(BloodBrain)
bbbDescr <-bbbDescr[, -nearZeroVar(bbbDescr)]
bbbDescr <-bbbDescr[, -findCorrelation(cor(bbbDescr), .5)]

set.seed(2)

inTrain <- createDataPartition(logBBB, p = .5)
trainX <-bbbDescr[inTrain[[1]], ]
trainY <- logBBB[inTrain[[1]]]
testX <- bbbDescr[-inTrain[[1]], ]
testY <- logBBB[-inTrain[[1]]]

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "oob")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "treebag", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"),
                           nbagg = 7)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "treebag",
                            trControl = rctrl2,
                            preProc = c("center", "scale"),
                            nbagg = 7)

set.seed(849)
test_reg_oob_model <- train(trainX, trainY, 
                            method = "treebag",
                            trControl = rctrl3,
                            preProc = c("center", "scale"),
                            nbagg = 7,
                            keepX = TRUE)

#########################################################################


sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


