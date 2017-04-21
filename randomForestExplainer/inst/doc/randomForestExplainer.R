## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

## ------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(randomForest)

## ------------------------------------------------------------------------
load("GlioblastomaWide.rda")

## ------------------------------------------------------------------------
GlioblastomaWide$death1y <- as.factor(GlioblastomaWide$death1y)
remove <- is.na(GlioblastomaWide) %>% colSums()
GlioblastomaWide <- GlioblastomaWide[, remove == 0]
rm(remove)
GlioblastomaWide <- GlioblastomaWide[, -1]

## ------------------------------------------------------------------------
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
# Possible measures: "mean_minimal_depth", "no_of_nodes", "MeanDecreaseGini", "no_of_trees", "times_a_root"
multi_way_importance_plot(importance_frame, x_measure = "mean_minimal_depth", y_measure = "no_of_trees", 
                          size_measure = "MeanDecreaseGini", min_no_of_trees = 0.2*max(importance_frame$no_of_trees),
                          x_label_prob = 0.01, y_label_prob = 0.01, size_label_prob = 0.01, main = "Multi-way importance plot")
multi_way_importance_plot(importance_frame, x_measure = "mean_minimal_depth", y_measure = "no_of_nodes", 
                          size_measure = "MeanDecreaseGini", min_no_of_trees = 0.2*max(importance_frame$no_of_trees),
                          x_label_prob = 0.01, y_label_prob = 0.01, size_label_prob = 0.01, main = "Multi-way importance plot")

## ---- fig.width = 7, fig.height = 7--------------------------------------
multi_way_importance_plot(importance_frame, x_measure = "mean_minimal_depth", y_measure = "times_a_root", 
                          size_measure = "MeanDecreaseGini", min_no_of_trees = 0.2*max(importance_frame$no_of_trees),
                          x_label_prob = 0.01, y_label_prob = 0.01, size_label_prob = 0.01, main = "Multi-way importance plot")
multi_way_importance_plot(importance_frame, x_measure = "mean_minimal_depth", y_measure = "MeanDecreaseGini", 
                          size_measure = "times_a_root", min_no_of_trees = 0.4*max(importance_frame$no_of_trees),
                          x_label_prob = 0.01, y_label_prob = 0.01, size_label_prob = 0.01, main = "Multi-way importance plot")

