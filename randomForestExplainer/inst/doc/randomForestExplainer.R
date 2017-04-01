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
# forest <- randomForest(death1y ~ ., data = GlioblastomaWide, ntree = 10000)
# save(forest, file = "GlioblastomaWide_forest.rda")
load("GlioblastomaWide_forest.rda")

## ------------------------------------------------------------------------
frame <- getTree(forest, k = 1, labelVar = TRUE) # the argument labelVar has to be set to TRUE!
head(frame) # this is how such frame looks like
head(calculate_tree_depth(frame)) # this is what our function produces

## ------------------------------------------------------------------------
min_depth_frame <- min_depth_distribution(forest)
head(min_depth_frame, n = 10) # this is how the output looks like

## ------------------------------------------------------------------------
# library(tidyr)
# min_depth_frame <- tidyr::complete(min_depth_frame, tree = 1:forest$ntree, variable = labels(forest$terms))

## ------------------------------------------------------------------------
min_depth_count_list <- min_depth_count(min_depth_frame)
head(min_depth_count_list[[1]], n = 10)

## ------------------------------------------------------------------------
head(min_depth_count_list[[2]], n = 10)

## ---- fig.width = 6, fig.height = 6--------------------------------------
plot_min_depth_distribution(min_depth_frame)

## ---- fig.width = 6, fig.height = 9--------------------------------------
plot_min_depth_distribution(min_depth_frame, k = 15, min_no_of_trees = 50)

