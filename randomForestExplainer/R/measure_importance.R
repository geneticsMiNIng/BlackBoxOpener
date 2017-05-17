#' Importance of variables in a random forest
#'
#' Get a data frame with various measures of importance of variables in a random forest
#'
#' @param forest A random forest produced by the function randomForest with option localImp = TRUE
#'
#' @return A data frame with rows corresponding to variables and columns to various measures of importance of variables
#'
#' @import dplyr
#'
#' @examples
#' measure_importance(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE))
#'
#' @export
measure_importance <- function(forest){
  forest_table <-
    lapply(1:forest$ntree, function(i) randomForest::getTree(forest, k = i, labelVar = T) %>%
             calculate_tree_depth() %>% cbind(tree = i)) %>%
    data.table::rbindlist()
  min_depth_frame <- dplyr::group_by(forest_table, tree, `split var`) %>%
    dplyr::summarize(min(depth))
  colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
  min_depth_frame <- as.data.frame(min_depth_frame[!is.na(min_depth_frame$variable),])
  importance_frame <- aggregate(minimal_depth ~ variable, data = min_depth_frame, mean)
  colnames(importance_frame)[2] <- "mean_minimal_depth"
  importance_frame$variable <- as.character(importance_frame$variable)
  nodes_occurances <- dplyr::group_by(forest_table, `split var`) %>%
    dplyr::summarize(n())
  colnames(nodes_occurances) <- c("variable", "no_of_nodes")
  nodes_occurances <- as.data.frame(nodes_occurances[!is.na(nodes_occurances$variable),])
  nodes_occurances$variable <- as.character(nodes_occurances$variable)
  if(forest$type == "classification"){
    vimp_frame <- as.data.frame(forest$importance[, c("MeanDecreaseAccuracy", "MeanDecreaseGini")])
    colnames(vimp_frame) <- c("accuracy_decrease", "gini_decrease")
  } else if(forest$type =="regression"){
    vimp_frame <- as.data.frame(forest$importance)
    colnames(vimp_frame) <- c("mse_increase", "node_purity_increase")
  }
  vimp_frame$variable <- rownames(vimp_frame)
  min_depth_count <- dplyr::group_by(min_depth_frame, variable, minimal_depth) %>%
    dplyr::summarize(count = n()) %>% as.data.frame()
  trees_occurances <- aggregate(count ~ variable, data = min_depth_count, sum)
  colnames(trees_occurances)[2] <- "no_of_trees"
  trees_occurances$variable <- as.character(trees_occurances$variable)
  root_count <-
    merge(min_depth_count[min_depth_count$minimal_depth == 0, c("variable", "count")],
          data.frame(variable = unique(min_depth_count$variable)), all = TRUE)
  colnames(root_count)[2] <- "times_a_root"
  root_count$variable <- as.character(root_count$variable)
  root_count[is.na(root_count$times_a_root), "times_a_root"] <- 0
  importance_frame <- merge(importance_frame, nodes_occurances, all = TRUE)
  importance_frame <- merge(importance_frame, vimp_frame, all = TRUE)
  importance_frame <- merge(importance_frame, trees_occurances, all = TRUE)
  importance_frame <- merge(importance_frame, root_count, all = TRUE)
  importance_frame[is.na(importance_frame$no_of_nodes), c("no_of_nodes", "no_of_trees", "times_a_root")] <- 0
  importance_frame$variable <- as.factor(importance_frame$variable)
  return(importance_frame)
}

#' Extract k most important variables in a random forest
#'
#' Get the names of k variables with highest sum of rankings based on the specified importance measures
#'
#' @param importance_frame A result of using the function measure_importance() to a random forest
#' @param measures A character vector specifying the measures of importance to be used
#' @param k The number of variables to extract
#' @param ties_action One of three: c("none", "all", "draw"); specifies which variables to pick when ties occur. When set to "none" we may get less than k variables, when "all" whe may get more and "draw" makes us get exactly k.
#'
#' @return A character vector with names of k variables with highest sum of rankings
#'
#' @examples
#' important_variables(measure_importance(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)))
#'
#' @export
important_variables <- function(importance_frame, k = 15, measures = names(importance_frame)[2:5],
                                ties_action = "all"){
  rankings <- data.frame(variable = importance_frame$variable, mean_minimal_depth =
                           data.table::frankv(importance_frame$mean_minimal_depth, ties.method = "dense"),
                         apply(importance_frame[, -c(1, 2)], 2,
                               function(x) frankv(x, order = -1, ties.method = "dense")))
  rankings$index <- rowSums(rankings[, measures])
  vars <- as.character(rankings[order(rankings$index), "variable"])[1:min(k, nrow(rankings))]
  if(length(rankings[rankings$index == rankings[rankings$variable == vars[length(vars)], "index"], "index"]) > 1 &
     length(rankings[rankings$index <= rankings[rankings$variable == vars[length(vars)], "index"], "variable"]) > k){
    draw <- rankings[rankings$index == rankings[rankings$variable == vars[length(vars)], "index"], ]
    if(ties_action == "none"){
      vars <- as.character(rankings[rankings$index < draw$index[1], "variable"])
    } else if(ties_action == "all"){
      vars <- as.character(rankings[rankings$index <= draw$index[1], "variable"])
    } else if(ties_action == "draw"){
      vars <- as.character(rankings[rankings$index < draw$index[1], "variable"])
      vars <- c(vars, sample(as.character(draw$variable), size = k - length(vars)))
    }
  }
  return(vars)
}

#' Multi-way importance plot
#'
#' Plot two or three measures of importance of variables in a random fores. Choose importance measures from the colnames(importance_frame).
#'
#' @param importance_frame A result of using the function measure_importance() to a random forest
#' @param x_measure The measure of importance to be shown on the X axis
#' @param y_measure The measure of importance to be shown on the Y axis
#' @param size_measure The measure of importance to be shown as size of points (optional)
#' @param min_no_of_trees The minimal number of trees in which a variable has to be used for splitting to be used for plotting
#' @param no_of_labels The approximate number of best variables (according to all measures plotted) to be labeled (more will be labeled in case of ties)
#' @param main A string to be used as title of the plot
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import ggrepel
#'
#' @examples
#' plot_multi_way_importance(measure_importance(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)))
#'
#' @export
plot_multi_way_importance <- function(importance_frame, x_measure = "mean_minimal_depth",
                                      y_measure = "times_a_root", size_measure = NULL,
                                      min_no_of_trees = 0.1*max(importance_frame$no_of_trees),
                                      no_of_labels = 10,
                                      main = "Multi-way importance plot"){
  data <- importance_frame[importance_frame$no_of_trees > min_no_of_trees, ]
  data_for_labels <- importance_frame[importance_frame$variable %in%
                                        important_variables(importance_frame, k = no_of_labels,
                                                            measures = c(x_measure, y_measure, size_measure)), ]
  plot <- ggplot(data, aes_string(x = x_measure, y = y_measure, size = size_measure)) +
    geom_point(aes(colour = "black")) + geom_point(data = data_for_labels, aes(colour = "blue")) +
    geom_label_repel(data = data_for_labels, aes(label = variable, size = NULL), show.legend = FALSE) +
    scale_colour_manual(name = "variable", values = c("black", "blue"), labels = c("non-top", "top")) +
    theme_bw()
  if(x_measure %in% c("no_of_nodes", "no_of_trees", "times_a_root")){
    plot <- plot + scale_x_sqrt()
  } else if(y_measure %in% c("no_of_nodes", "no_of_trees", "times_a_root")){
    plot <- plot + scale_y_sqrt()
  }
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}

#' Plot importance measures with ggpairs
#'
#' Plot selected measures of importance of variables in a forest using ggpairs
#'
#' @param importance_frame A result of using the function measure_importance() to a random forest
#' @param measures A character vector specifying the measures of importance to be used
#' @param main A string to be used as title of the plot
#' @return A ggplot object
#'
#' @import ggplot2
#' @import GGally
#'
#' @examples
#' plot_importance_ggpairs(measure_importance(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)))
#'
#' @export
plot_importance_ggpairs <- function(importance_frame, measures =
                                      names(importance_frame)[c(2, 4, 5, 3, 7)],
                                    main = "Relations between measures of importance"){
  plot <- ggpairs(importance_frame[, measures]) + theme_bw()
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}

#' Plot importance measures rankings with ggpairs
#'
#' Plot against each other rankings of variables according to various measures of importance
#'
#' @param importance_frame A result of using the function measure_importance() to a random forest
#' @param measures A character vector specifying the measures of importance to be used
#' @param main A string to be used as title of the plot
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import GGally
#'
#' @examples
#' plot_importance_ggpairs(measure_importance(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)))
#'
#' @export
plot_importance_rankings <- function(importance_frame, measures =
                                       names(importance_frame)[c(2, 4, 5, 3, 7)],
                                     main = "Relations between rankings according to different measures"){
  rankings <- data.frame(variable = importance_frame$variable, mean_minimal_depth =
                           frankv(importance_frame$mean_minimal_depth, ties.method = "dense"),
                         apply(importance_frame[, -c(1, 2)], 2,
                               function(x) frankv(x, order = -1, ties.method = "dense")))
  plot <- ggpairs(rankings[, measures]) + theme_bw()
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}
