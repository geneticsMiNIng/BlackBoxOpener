## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

## ------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(randomForest)
library(randomForestExplainer)

## ------------------------------------------------------------------------
load("GlioblastomaWide.rda")

## ------------------------------------------------------------------------
GlioblastomaWide$death1y <- as.factor(GlioblastomaWide$death1y)
remove <- is.na(GlioblastomaWide) %>% colSums()
GlioblastomaWide <- GlioblastomaWide[, remove == 0]
rm(remove)
GlioblastomaWide <- GlioblastomaWide[, -1]

## ------------------------------------------------------------------------
# set.seed(2017)
# forest <- randomForest(death1y ~ ., data = GlioblastomaWide, ntree = 10000, importance = TRUE)
# save(forest, file = "GlioblastomaWide_forest.rda")
load("GlioblastomaWide_forest.rda")

## ------------------------------------------------------------------------
min_depth_frame <- min_depth_distribution(forest)
head(min_depth_frame, n = 10) # this is how the output looks like

## ------------------------------------------------------------------------
# library(tidyr)
# min_depth_frame_complete <- tidyr::complete(min_depth_frame, tree = 1:forest$ntree, variable = labels(forest$terms))

## ---- fig.width = 7, fig.height = 5--------------------------------------
plot_min_depth_distribution(min_depth_frame)

## ---- fig.width = 7, fig.height = 7--------------------------------------
plot_min_depth_distribution(min_depth_frame, k = 15, min_no_of_trees = 50)

## ------------------------------------------------------------------------
# importance_frame <- measure_importance(forest)
# save(importance_frame, file = "GlioblastomaWide_importance_frame.rda")
load("GlioblastomaWide_importance_frame.rda")
head(importance_frame, n = 10)

## ---- fig.width = 7, fig.height = 7--------------------------------------
# Possible measures for classification: "mean_minimal_depth", "no_of_nodes", "accuracy_decrease", "gini_decrease", "no_of_trees", "times_a_root"
plot_multi_way_importance(importance_frame, x_measure = "mean_minimal_depth", 
                          y_measure = "no_of_trees", size_measure = "gini_decrease", 
                          min_no_of_trees = 0.2*max(importance_frame$no_of_trees),
                          x_label_prob = 0.01, y_label_prob = 0.01, size_label_prob = 0.01, 
                          main = "Multi-way importance plot")
plot_multi_way_importance(importance_frame, x_measure = "mean_minimal_depth", 
                          y_measure = "no_of_nodes", size_measure = "accuracy_decrease", 
                          min_no_of_trees = 0.2*max(importance_frame$no_of_trees),
                          x_label_prob = 0.01, y_label_prob = 0.01, size_label_prob = 0.01, 
                          main = "Multi-way importance plot")

## ---- fig.width = 7, fig.height = 7--------------------------------------
plot_multi_way_importance(importance_frame, x_measure = "mean_minimal_depth", 
                          y_measure = "times_a_root", size_measure = "gini_decrease", 
                          min_no_of_trees = 0.2*max(importance_frame$no_of_trees),
                          x_label_prob = 0.01, y_label_prob = 0.01, size_label_prob = 0.01, 
                          main = "Multi-way importance plot")
plot_multi_way_importance(importance_frame, x_measure = "mean_minimal_depth", 
                          y_measure = "gini_decrease", size_measure = "times_a_root", 
                          min_no_of_trees = 0.4*max(importance_frame$no_of_trees),
                          x_label_prob = 0.01, y_label_prob = 0.01, size_label_prob = 0.01, 
                          main = "Multi-way importance plot")

## ---- fig.width = 7, fig.height = 6--------------------------------------
plot_importance_ggpairs(importance_frame)

## ---- fig.width = 7, fig.height = 6--------------------------------------
plot_importance_rankings(importance_frame)

## ------------------------------------------------------------------------
vars <- as.character(importance_frame[importance_frame[["mean_minimal_depth"]] < 
                                        quantile(importance_frame[["mean_minimal_depth"]], 0.01, na.rm = TRUE) &
                                        importance_frame[["no_of_trees"]] > 
                                        quantile(importance_frame[["no_of_trees"]], 0.95, na.rm = TRUE), "variable"])

## ------------------------------------------------------------------------
# interactions_frame <- min_depth_interactions(forest, vars)
# save(interactions_frame, file = "BlackBoxOpener/randomForestExplainer/vignettes/GlioblastomaWide_interactions_frame.rda")
load("GlioblastomaWide_interactions_frame.rda")

## ------------------------------------------------------------------------
head(interactions_frame)

## ------------------------------------------------------------------------
head(interactions_frame[order(interactions_frame$occurances, decreasing = TRUE), ])

## ------------------------------------------------------------------------
# set.seed(2017)
# forest_v2 <- randomForest(death1y ~ ., data = GlioblastomaWide, ntree = 10000, mtry = floor(ncol(GlioblastomaWide)/3), importance = TRUE, localImp = TRUE)
# save(forest_v2, file = "GlioblastomaWide_forest_v2.rda")
load("GlioblastomaWide_forest_v2.rda")
importance_frame <- measure_importance(forest_v2)
vars <- as.character(importance_frame[importance_frame[["mean_minimal_depth"]] < 
                                        quantile(importance_frame[["mean_minimal_depth"]], 0.01, na.rm = TRUE) &
                                        importance_frame[["no_of_trees"]] > 
                                        quantile(importance_frame[["no_of_trees"]], 0.95, na.rm = TRUE), "variable"])
interactions_frame <- min_depth_interactions(forest_v2, vars)
# save(interactions_frame, file = "GlioblastomaWide_interactions_frame_v2.rda")
load("GlioblastomaWide_interactions_frame_v2.rda")
head(interactions_frame[order(interactions_frame$occurances, decreasing = TRUE), ])

## ---- fig.width = 7, fig.height = 5--------------------------------------
plot_min_depth_interactions(interactions_frame)

## ------------------------------------------------------------------------
# source("https://bioconductor.org/biocLite.R")
# biocLite("DESeq")
# biocLite("limma")
# biocLite("TxDb.Hsapiens.UCSC.hg18.knownGene")
# biocLite("org.Hs.eg.db")
# biocLite("DESeq2")
# biocLite("edgeR")
# devtools::install_github("geneticsMiNIng/MLGenSig", subdir = "MetExpR")

# brca <- MetExpR::BRCA_mRNAseq_chr17
# colnames(brca) <- make.names(colnames(brca))
# brca$SUBTYPE <- factor(brca$SUBTYPE)

# save(brca, file = "BreastCancer.rda")
load("BreastCancer.rda")

## ------------------------------------------------------------------------
# set.seed(2017)
# forest_brca <- randomForest(SUBTYPE ~ ., data = brca, ntree = 10000, importance = TRUE, localImp = TRUE)
# save(forest_brca, file = "BreastCancer_forest.rda")
load("BreastCancer_forest.rda")

## ------------------------------------------------------------------------
# devtools::install_github("pbiecek/PISA2012lite")
# library("PISA2012lite")

# pisa <- na.omit(student2012[,c(1, 4, 12, 13, 18:20, 39, 61:62, 114, 488, 457, 501)])
# pisa <- pisa[pisa$CNT %in% c("Austria", "Belgium", "Czech Republic", "Germany", "Denmark", "Spain", "Estonia", "Finland", "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Sweden", "Romania", "Croatia", "Bulgaria"),] # consider only EU countries to reduce the size
# pisa <- pisa[pisa$CNT %in% c(), ] # only the Visegrad group to begin with
# pisa$CNT <- factor(pisa$CNT)

# save(pisa, file = "PISA.rda")
load("PISA.rda")

## ------------------------------------------------------------------------
# set.seed(2017)
# forest_pisa <- randomForest(PV1MATH ~ ., data = pisa, importance = TRUE, localImp = TRUE)
# save(forest_pisa, file = "PISA_forest.rda")
load("PISA_forest.rda")

