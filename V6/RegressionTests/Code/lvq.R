library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "lvq"
tests <- c("test_class_cv_model", "test_class_pred", 
           "test_class_loo_model", "test_levels")

#########################################################################

set.seed(2)
training <- twoClassSim(100)
testing <- twoClassSim(500)
trainX <- training[, -ncol(training)]
trainY <- training$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
cctrl2 <- trainControl(method = "LOOCV")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "lvq", 
                             trControl = cctrl1,
                             preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "lvq", 
                              trControl = cctrl2,
                              preProc = c("center", "scale"))
test_levels <- levels(test_class_cv_model)

#########################################################################

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")

