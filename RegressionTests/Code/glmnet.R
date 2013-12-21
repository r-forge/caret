library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "glmnet"

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
                             method = "glmnet", 
                             trControl = cctrl1,
                             preProc = c("center", "scale"),
                             tuneGrid = expand.grid(.alpha = seq(.07, .11, length = 15),
                                                    .lambda = c((1:5)/10)))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "glmnet", 
                              trControl = cctrl2,
                              preProc = c("center", "scale"),
                              tuneGrid = expand.grid(.alpha = c(.07, .11),
                                                     .lambda = c(.1, .5)))
test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

## From ?cv.glmnet 
set.seed(1010)
n=1000;p=100
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
beta=rnorm(nzc)
fx= x[,seq(nzc)] %*% beta
eps=rnorm(n)*5
y=drop(fx+eps)
set.seed(1011)
cvob1=cv.glmnet(x,y)

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")

set.seed(849)
test_reg_cv_model <- train(x, y, method = "glmnet",
                           preProc = c("center", "scale"),
                           trControl = rctrl1,
                           tuneGrid = data.frame(.alpha = c(.5, 1),
                                                 .lambda = cvob1$lambda))
test_reg_pred <- predict(test_reg_cv_model, x)

set.seed(849)
test_reg_loo_model <- train(x, y, method = "glmnet",
                            preProc = c("center", "scale"),
                            trControl = rctrl2,
                            tuneGrid = data.frame(.alpha = c(.5, 1),
                                                  .lambda = cvob1$lambda))

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


