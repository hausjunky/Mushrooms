# Libraries ####
suppressPackageStartupMessages(library(data.table))

# Load Data ####
site <- c('https://archive.ics.uci.edu/ml/machine-learning-databases/')
folder <- c('mushroom/agaricus-lepiota.data')
data <- fread(paste0(site, folder))

# Rename ####
label <- 'poisonous'

cols <- c('capShape',
          'capSurface',
          'capColor',
          'bruises',
          'odor',
          'gillAttached',
          'gillCrowded',
          'gillNarrow',
          'gillColor',
          'stalkEnlarging',
          'stalkRoot',
          'stalkSurfaceAboveRing',
          'stalkSurfaceBelowRing',
          'stalkColorAboveRing',
          'stalkColorBelowRing',
          'veilType',
          'veilColor',
          'ringNumber',
          'ringType',
          'sporePrintColor',
          'population',
          'habitat')

names(data) <- c(label, cols)

# Pre-Process ####
data[, poisonous := fifelse(poisonous == 'p', 1, 0)]

data[, capShape := fcase(capShape == 'b', 'bell',
                         capShape == 'c', 'conical',
                         capShape == 'f', 'flat',
                         capShape == 'k', 'knobbed',
                         capShape == 's', 'sunken',
                         capShape == 'x', 'convex')]

data[, capSurface := fcase(capSurface == 'f', 'fibrous',
                           capSurface == 'g', 'grooves',
                           capSurface == 's', 'smooth',
                           capSurface == 'y', 'scaly')]

data[, capColor := fcase(capColor == 'b', 'buff',
                         capColor == 'c', 'cinnamon',
                         capColor == 'e', 'red',
                         capColor == 'g', 'gray',
                         capColor == 'n', 'brown',
                         capColor == 'p', 'pink',
                         capColor == 'r', 'green',
                         capColor == 'u', 'purple',
                         capColor == 'w', 'white',
                         capColor == 'y', 'yellow')]

data[, bruises := fifelse(bruises == 't', 1, 0)]

data[, odor := fcase(odor == 'a', 'almond',
                     odor == 'c', 'creosote',
                     odor == 'f', 'foul',
                     odor == 'l', 'anise',
                     odor == 'm', 'musty',
                     odor == 'n', 'none',
                     odor == 'p', 'pungent',
                     odor == 's', 'spicy',
                     odor == 'y', 'fishy')]

data[, gillAttached := fifelse(gillAttached == 'a', 1, 0)]

data[, gillCrowded := fifelse(gillCrowded == 'w', 1, 0)]

data[, gillNarrow := fifelse(gillNarrow == 'n', 1, 0)]

data[, gillColor := fcase(gillColor == 'b', 'buff',
                          gillColor == 'e', 'red',
                          gillColor == 'g', 'gray',
                          gillColor == 'h', 'chocolate',
                          gillColor == 'k', 'black',
                          gillColor == 'n', 'brown',
                          gillColor == 'o', 'orange',
                          gillColor == 'p', 'pink',
                          gillColor == 'r', 'green',
                          gillColor == 'u', 'purple',
                          gillColor == 'w', 'white',
                          gillColor == 'y', 'yellow')]

data[, stalkEnlarging := fifelse(stalkEnlarging == 'e', 1, 0)]

data[, stalkRoot := fcase(stalkRoot == '?', 'none',
                          stalkRoot == 'b', 'bulbous',
                          stalkRoot == 'c', 'club',
                          stalkRoot == 'e', 'equal',
                          stalkRoot == 'r', 'rooted')]

data[, stalkSurfaceAboveRing := fcase(stalkSurfaceAboveRing == 'f', 'fibrous',
                                      stalkSurfaceAboveRing == 'k', 'silky',
                                      stalkSurfaceAboveRing == 's', 'smooth',
                                      stalkSurfaceAboveRing == 'y', 'scaly')]

data[, stalkSurfaceBelowRing := fcase(stalkSurfaceBelowRing == 'f', 'fibrous',
                                      stalkSurfaceBelowRing == 'k', 'silky',
                                      stalkSurfaceBelowRing == 's', 'smooth',
                                      stalkSurfaceBelowRing == 'y', 'scaly')]

data[, stalkColorAboveRing := fcase(stalkColorAboveRing == 'b', 'buff',
                                    stalkColorAboveRing == 'c', 'cinnamon',
                                    stalkColorAboveRing == 'e', 'red',
                                    stalkColorAboveRing == 'g', 'gray',
                                    stalkColorAboveRing == 'n', 'brown',
                                    stalkColorAboveRing == 'o', 'orange',
                                    stalkColorAboveRing == 'p', 'pink',
                                    stalkColorAboveRing == 'w', 'white',
                                    stalkColorAboveRing == 'y', 'yellow')]

data[, stalkColorBelowRing := fcase(stalkColorBelowRing == 'b', 'buff',
                                    stalkColorBelowRing == 'c', 'cinnamon',
                                    stalkColorBelowRing == 'e', 'red',
                                    stalkColorBelowRing == 'g', 'gray',
                                    stalkColorBelowRing == 'n', 'brown',
                                    stalkColorBelowRing == 'o', 'orange',
                                    stalkColorBelowRing == 'p', 'pink',
                                    stalkColorBelowRing == 'w', 'white',
                                    stalkColorBelowRing == 'y', 'yellow')]

data[, veilType := NULL]

data[, veilColor := fcase(veilColor == 'n', 'brown',
                          veilColor == 'o', 'orange',
                          veilColor == 'w', 'white',
                          veilColor == 'y', 'yellow')]

data[, ringNumber := fcase(ringNumber == 'n', 0,
                           ringNumber == 'o', 1,
                           ringNumber == 't', 2)]

data[, ringType := fcase(ringType == 'e', 'evanescent',
                         ringType == 'f', 'flaring',
                         ringType == 'l', 'large',
                         ringType == 'n', 'none',
                         ringType == 'p', 'pendant')]

data[, sporePrintColor := fcase(sporePrintColor == 'b', 'buff',
                                sporePrintColor == 'h', 'chocolate',
                                sporePrintColor == 'k', 'black',
                                sporePrintColor == 'n', 'brown',
                                sporePrintColor == 'o', 'orange',
                                sporePrintColor == 'r', 'green',
                                sporePrintColor == 'u', 'purple',
                                sporePrintColor == 'w', 'white',
                                sporePrintColor == 'y', 'yellow')]

data[, population := fcase(population == 'a', 'abundant',
                           population == 'c', 'clustered',
                           population == 'n', 'numerous',
                           population == 's', 'scattered',
                           population == 'v', 'several',
                           population == 'y', 'solitary')]

data[, habitat := fcase(habitat == 'd', 'woods',
                        habitat == 'g', 'grasses',
                        habitat == 'l', 'leaves',
                        habitat == 'm', 'meadows',
                        habitat == 'p', 'paths',
                        habitat == 'u', 'urban',
                        habitat == 'w', 'waste')]

# Save ####
write.csv(data, 'mushroom.csv', row.names = FALSE)
