# setwd("~/Code/caret/V6//caret/inst/models/")
modelFiles <- list.files(path = "models", pattern = "\\.R$")

models <- vector(mode = "list", length = length(modelFiles))
names(models) <- gsub("\\.R", "", modelFiles)

for(i in seq(along = modelFiles)) {
  source(file.path("models", modelFiles[i]))
  models[[i]] <- modelInfo
  rm(modelInfo)
}

save(models, file = "models.RData")

cat("\nYou can update your caret installation using the command:\n\n")
cat(sprintf("  cp models.RData %s/.\n", system.file("models", package="caret")))
