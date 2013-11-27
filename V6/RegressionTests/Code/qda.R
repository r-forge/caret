library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "qda"
tests <- c("test_class_cv_model", "test_class_pred", "test_class_prob",
           "test_class_loo_model", "test_levels")

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

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "qda", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "qda", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"))
test_levels <- levels(test_class_cv_model)

#########################################################################

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


