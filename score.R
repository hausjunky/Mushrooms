# Libraries ####
suppressPackageStartupMessages(library(data.table))
library(xgboost)
library(waterfalls)

# Categorical Load ####
cats <- c('capShape',
          'capSurface',
          'capColor',
          'odor',
          'gillColor',
          'stalkRoot',
          'stalkSurfaceAboveRing',
          'stalkSurfaceBelowRing',
          'stalkColorAboveRing',
          'stalkColorBelowRing',
          'veilColor',
          'ringType',
          'sporePrintColor',
          'population',
          'habitat')

ncode <- list()
for (i in cats) ncode[[i]] <- fread(paste0(i, '.csv'))

# Model Load ####
model <- xgb.load('script.model')

# New Data ####
# data <- data.table(capShape = 'bell',
#                    capSurface = 'smooth',
#                    capColor = 'white',
#                    bruises = 1,
#                    odor = 'anise',
#                    gillAttached = 0,
#                    gillCrowded = 0,
#                    gillNarrow = 0,
#                    gillColor = 'brown',
#                    stalkEnlarging = 1,
#                    stalkRoot = 'club',
#                    stalkSurfaceAboveRing = 'smooth',
#                    stalkSurfaceBelowRing = 'smooth',
#                    stalkColorAboveRing = 'white',
#                    stalkColorBelowRing = 'white',
#                    veilColor = 'white',
#                    ringNumber = 1,
#                    ringType = 'pendant',
#                    sporePrintColor = 'brown',
#                    population = 'numerous',
#                    habitat = 'meadows')

data <- fread('mushroom.csv')
data <- data[sample(nrow(data), 1)]
data[, poisonous := NULL]

# Transform ####
table <- copy(data)

for (i in cats) {
  
  tbl <- table[, .(get(i))]
  setnames(tbl, names(tbl), i)
  tbl[, row := 1:nrow(tbl)]
  tbl <- merge.data.table(tbl, ncode[[i]], all.x = TRUE, by = i)
  setorder(tbl, row)
  tbl[[i]] <- NULL
  tbl[, row := NULL]
  setnames(tbl, names(tbl), i)
  table[[i]] <- tbl[[i]]
  
}

# SHAP ####
shap <- copy(data)
shap[, bias := 'all']
shap <- data.table(t(shap), keep.rownames = TRUE)
setnames(shap, names(shap), c('Variable', 'Value'))
shap[, Score := t(predict(model, as.matrix(table), predcontrib = TRUE))]
shap <- shap[c(nrow(shap), 1:(nrow(shap) - 1))]
plogis(sum(shap$Score))
waterfall(values = round(cumsum(shap$Score), 5), labels = shap$Variable)
