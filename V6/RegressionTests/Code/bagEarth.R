library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "bagEarth"

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
                             method = "bagEarth", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             B = 10)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "bagEarth", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              B = 10)
test_levels <- levels(test_class_cv_model)

set.seed(849)
test_class_oob_model <- train(trainX, trainY, 
                              method = "bagEarth", 
                              trControl = cctrl3,
                              B = 10)

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
                           method = "bagEarth", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"),
                           B = 10)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "bagEarth",
                            trControl = rctrl2,
                            preProc = c("center", "scale"),
                            B = 10)

set.seed(849)
test_reg_oob_model <- train(trainX, trainY, 
                            method = "bagEarth",
                            trControl = rctrl3,
                            preProc = c("center", "scale"),
                            B = 10)

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_class_predictors2 <- predictors(test_class_cv_model$finalModel)
test_reg_predictors1 <- predictors(test_reg_cv_model)
test_reg_predictors2 <- predictors(test_reg_cv_model$finalModel)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


