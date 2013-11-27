library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "bstSm"

#########################################################################

set.seed(2)
training <- twoClassSim(50)
testing <- twoClassSim(500)
trainX <- training[, -ncol(training)]
trainY <- training$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
cctrl2 <- trainControl(method = "LOOCV",verboseIter = TRUE)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "bstSm", 
                             trControl = cctrl1,
                             preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "bstSm", 
                              trControl = cctrl2,
                              preProc = c("center", "scale"))
test_levels <- levels(test_class_cv_model)

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

## Bst smoothers require at least 4 distinct values in x so 
## we'll filter with some wiggle room

keepers <- apply(trainX, 2, function(x) length(unique(x))) > 10
trainX <- trainX[,keepers]
testX  <- testX[,keepers]

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "bstSm", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"))
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "bstSm",
                            trControl = rctrl2,
                            preProc = c("center", "scale"))

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


