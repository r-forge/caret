# setwd("~/Code/caret/V6//caret/inst/models/")
modelFiles <- list.files(path = "models", pattern = "\\.R$")

models <- vector(mode = "list", length = length(modelFiles))
names(models) <- gsub("\\.R", "", modelFiles)

for(i in seq(along = modelFiles)) {
  cat(sprintf("sourcing %s...\n", modelFiles[i]))
  source(file.path("models", modelFiles[i]))
  models[[i]] <- modelInfo
  rm(modelInfo)
}

save(models, file = "models.RData")
