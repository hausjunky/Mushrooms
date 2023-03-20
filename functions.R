# Libraries ####
suppressPackageStartupMessages(library(data.table))
library(mltools)
library(xgboost)
library(Matrix)
options(scipen = 999)

# User Defined Functions ####
custom_folds <- function(label, nfolds = 5L, seed = 42) {
  
  cut <- folds(x = label, nfolds = nfolds, stratified = TRUE, seed = seed)
  
  fold <- list()
  for (i in 1:nfolds) fold[[i]] <- which(cut == i)
  
  return(list(vector = cut, list = fold))
  
}

encode_fit <- function(data, label, cats) {
  
  encode <- list()
  
  for (i in cats) {
    
    tbl <- data.table(lbl = label, col = data[[i]])
    tbl <- tbl[, .(pct = .N / nrow(tbl), trg = sum(lbl) / sum(tbl$lbl)), .(col)]
    tbl[, rto := fifelse(trg == 0, NA_real_, trg / pct)]
    setorder(tbl, -rto)
    tbl <- tbl[, .(col, rto)]
    setnames(tbl, names(tbl), c(i, paste0('enc_', i)))
    encode[[i]] <- tbl
    
  }
  
  return(encode)
  
}

encode_transform <- function(data, encode, cats) {
  
  keep <- data
  
  for (i in cats) {
    
    tbl <- keep[, .(get(i))]
    setnames(tbl, 'V1', i)
    tbl[, row := 1:nrow(tbl)]
    tbl <- merge.data.table(tbl, encode[[i]], all.x = TRUE, by = i)
    setorder(tbl, row)
    tbl[[i]] <- NULL
    tbl[, row := NULL]
    setnames(tbl, names(tbl), i)
    keep[[i]] <- tbl[[i]]
    
  }
  
  return(keep)
  
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

shap_fit <- function(model, matrix, vars, cats, combine = '+') {
  
  shap <- data.table(predict(model, matrix, predcontrib = TRUE))
  
  if (ncol(shap) != length(vars) + 1) {
    
    for (i in cats) {
      
      names <- names(shap)[substring(names(shap), 1, nchar(i)) == i]
      table <- shap[, ..names]
      table <- table[, Reduce(combine, .SD), .SDcols = names]
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
data <- fread('mushroom.csv')

# Variables ####
target <- 'poisonous'
vars <- setdiff(names(data), target)

# Split ####
label <- data[[target]]
data <- data[, ..vars]

# Hyperparameters ####
hyper <- list(objective = 'binary:logistic',
              tree_method = 'hist',
              grow_policy = 'lossguide',
              max_depth = 0,
              max_leaves = 10)

# Categorical Variables ####
cats <- names(data[, sapply(data, is.character), with = FALSE])
data[, (cats) := lapply(.SD, as.factor), .SDcols = cats]

# Stratified Folds ####
folds <- custom_folds(label)$list

# Matrix ####
encode <- encode_fit(data, label, cats)
matrix <- as.matrix(encode_transform(data, encode, cats))
# matrix <- Matrix(data = as.matrix(one_hot(data)), sparse = TRUE)
# matrix <- as.matrix(data)

# xgb.DMatrix ####
matrix <- xgb.DMatrix(data = matrix, label = label)

# Model ####
model <- model_fit(matrix, folds, hyper)
fitted <- predict(model, matrix)

# SHAP ####
shap <- shap_fit(model, matrix, vars, cats, combine = '+')
setnames(shap, names(shap), paste0('SHAP_', names(shap)))
shap <- cbind(label, fitted, data, shap)
