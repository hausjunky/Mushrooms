# Libraries ####
suppressPackageStartupMessages(library(data.table))
library(mltools)
library(xgboost)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
suppressPackageStartupMessages(library(ModelMetrics))

# Miscellaneous ####
options(scipen = 999)
packs <- c('data.table', 'xgboost', 'mltools', 'ModelMetrics')
registerDoParallel(detectCores())

# User Defined Functions ####
custom_folds <- function(label, nfolds = 5L, seed = 42) {
  
  cut <- folds(x = label, nfolds = nfolds, stratified = TRUE, seed = seed)
  
  fold <- list()
  for (i in 1:nfolds) fold[[i]] <- which(cut == i)
  
  return(list(vector = cut, list = fold))
  
}

encode_fit <- function(table, label) {
  
  facs <- names(table[, sapply(table, is.factor), with = FALSE])
  
  if (length(facs) > 0) {
    
    table[, (facs) := lapply(.SD, as.character), .SDcols = facs]
    
  }
  
  cats <- names(table[, sapply(table, is.character), with = FALSE])
  
  encode <- list()
  
  for (i in cats) {
    
    tbl <- data.table(lbl = label, col = table[[i]])
    tbl <- tbl[, .(pct = .N / nrow(tbl), trg = sum(lbl) / sum(tbl$lbl)), .(col)]
    tbl <- tbl[trg > 0]
    tbl[, rto := trg / pct]
    tbl <- tbl[, .(col, rto)]
    setnames(tbl, names(tbl), c(i, paste0('enc_', i)))
    encode[[i]] <- tbl
    
  }
  
  return(encode)
  
}

encode_transform <- function(table, encode) {
  
  facs <- names(table[, sapply(table, is.factor), with = FALSE])
  
  if (length(facs) > 0) {
    
    table[, (facs) := lapply(.SD, as.character), .SDcols = facs]
    
  }
  
  cats <- names(table[, sapply(table, is.character), with = FALSE])
  
  for (i in cats) {
    
    tbl <- table[, .(get(i))]
    setnames(tbl, names(tbl), i)
    tbl[, row := 1:nrow(tbl)]
    tbl <- merge.data.table(tbl, encode[[i]], all.x = TRUE, by = i)
    setorder(tbl, row)
    tbl[[i]] <- NULL
    tbl[, row := NULL]
    setnames(tbl, names(tbl), i)
    table[[i]] <- tbl[[i]]
    
  }
  
  return(table)
  
}

model_fit <- function(matrix, folds, hyper) {
  
  set.seed(42)
  cross <- xgb.cv(params = hyper,
                  data = matrix,
                  nrounds = 1e6,
                  folds = folds,
                  verbose = FALSE,
                  early_stopping_rounds = 10)
  
  loss <- as.numeric(cross$evaluation_log[cross$best_iteration, 2])
  stop <- cross$best_iteration * 2
  list <- list(train = matrix)
  
  set.seed(42)
  best <- xgb.train(hyper, matrix, stop, list, verbose = 0)$evaluation_log
  setnames(best, names(best), c('iter', 'error'))
  best <- min(best[error < loss]$iter)
  
  set.seed(42)
  model <- xgb.train(hyper, matrix, best)
  
  return(model)
  
}

shap_fit <- function(model, matrix, ohe = FALSE, cats = NULL, sign = '+') {
  
  shap <- data.table(predict(model, matrix, predcontrib = TRUE))
  
  if (ohe) {
    
    for (i in cats) {
      
      names <- names(shap)[substring(names(shap), 1, nchar(i)) == i]
      table <- shap[, ..names]
      table <- table[, Reduce(sign, .SD), .SDcols = names]
      shap[[i]] <- table
      shap[, (names) := NULL]
      
    }
    
  }
  
  bias <- shap$BIAS
  shap$BIAS <- NULL
  shap <- cbind(bias, shap)
  
  return(shap)
  
}

# Read ####
table <- fread('mushroom.csv')

# Row ####
table[, rowNumber := .I]

# Label ####
label <- table$poisonous
table$poisonous <- NULL

# Folds ####
folds <- custom_folds(label)

# Hyperparameters ####
hyper <- list(objective = 'binary:logistic',
              tree_method = 'hist',
              grow_policy = 'lossguide',
              max_depth = 0,
              max_leaves = 10)

# Cross-Validation ####
cross <- foreach(i = 1:5, .combine = rbind, .packages = packs) %dopar% {
  
  cv_train <- table[folds$vector != i]
  cv_train[, rowNumber := NULL]
  cv_label <- label[folds$vector != i]
  cv_ncode <- encode_fit(cv_train, cv_label)
  cv_folds <- custom_folds(cv_label)$list
  cv_train <- encode_transform(cv_train, cv_ncode)
  cv_train <- xgb.DMatrix(data = as.matrix(cv_train), label = cv_label)
  cv_model <- model_fit(cv_train, cv_folds, hyper)
  cv_valid <- table[folds$vector == i]
  cv_score <- cv_valid[, .(rowNumber)]
  cv_valid[, rowNumber := NULL]
  cv_valid <- encode_transform(cv_valid, cv_ncode)
  cv_valid <- xgb.DMatrix(data = as.matrix(cv_valid))
  cv_score[, fold := i]
  cv_score[, label := label[folds$vector == i]]
  cv_score[, predicted := predict(cv_model, cv_valid)]
  cv_score
  
}

setorder(cross, rowNumber)

# Drop ####
table$rowNumber <- NULL
cross$rowNumber <- NULL

# Errors ####
error <- foreach(i = 1:1000, .combine = c, .packages = packs) %dopar% {
  
  set.seed(i)
  f <- sample(5, 1)
  subset <- cross[fold == f]
  predicted <- subset$predicted
  actual <- subset$label
  x <- sample(length(actual), length(actual), replace = TRUE)
  brier(actual[x], predicted[x])
  
}

# Encode ####
ncode <- encode_fit(table, label)

for (i in 1:length(ncode)) {
  
  write.csv(ncode[[i]], paste0(names(ncode[[i]])[1], '.csv'), row.names = FALSE)
  
}

# Training ####
train <- encode_transform(table, ncode)
train <- xgb.DMatrix(data = as.matrix(train), label = label)

# Model ####
model <- model_fit(train, folds$list, hyper)
xgb.save(model, 'script.model')

# SHAP ####
score <- shap_fit(model, train)
setnames(score, names(score), paste0('SHAP_', names(score)))
score <- cbind(table, cross, score)
