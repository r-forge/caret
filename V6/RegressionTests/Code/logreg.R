library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "logreg"

#########################################################################
gobinary <- function(x){
  out <- apply(x, 2, function(x) ifelse(x > mean(x), 1, 0))
  colnames(out) <- colnames(x)
  out
} 

set.seed(2)
training <- twoClassSim(100)
testing <- twoClassSim(500)
trainX <- gobinary(training[, -ncol(training)])
trainY <- training$Class
testX <- gobinary(testing[, -ncol(testing)])
testY <- testing$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
cctrl2 <- trainControl(method = "LOOCV")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "logreg", 
                             trControl = cctrl1)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "logreg", 
                              trControl = cctrl2)
test_levels <- levels(test_class_cv_model)

#########################################################################

data(BloodBrain)
bbbDescr <-bbbDescr[, -nearZeroVar(bbbDescr)]
bbbDescr <-bbbDescr[, -findCorrelation(cor(bbbDescr), .5)]
numVal <- apply(bbbDescr, 2, function(x) length(unique(x)))
bbbDescr <- bbbDescr[, numVal > 10]

set.seed(2)

inTrain <- createDataPartition(logBBB, p = .5)
trainX <-gobinary(bbbDescr[inTrain[[1]], ])
trainY <- logBBB[inTrain[[1]]]
testX <- gobinary(bbbDescr[-inTrain[[1]], ])
testY <- logBBB[-inTrain[[1]]]

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "logreg", 
                           trControl = rctrl1)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "logreg",
                            trControl = rctrl2)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


