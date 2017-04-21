#' Get a data frame with various measures of importance of variables in a forest
#'
#' @param forest A random forest produced by the function randomForest with option importance = TRUE
#' @return A data frame with rows corresponding to variables and columns to various measures of importance of variables
#' @examples
#' measure_importance(randomForest(Species ~ ., data = iris))
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
  vimp_frame <- as.data.frame(forest$importance)
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


#' Plot two or three measures of importance of variables in a forest
#'
#' @param importance_frame A result of using the function measure_importance() to a random forest
#' @param x_measure The measure of importance to be shown on the X axis
#' @param y_measure The measure of importance to be shown on the Y axis
#' @param size_measure The measure of importance to be shown as size of points (optional)
#' @param min_no_of_trees The minimal number of trees in which a variable has to be used for splitting to be used for plotting
#' @param x_label_prob The fraction of best points according to the measure on X axis to be labeled
#' @param y_label_prob The fraction of best points according to the measure on Y axis to be labeled
#' @param size_label_prob The fraction of best points according to the measure displayed by size of points to be labeled
#' @param main A string to be used as title of the plot
#' @return A ggplot object
#' @examples
#' multi_way_importance_plot(measure_importance(randomForest(Species ~ ., data = iris)))
#' @export
multi_way_importance_plot <- function(importance_frame, x_measure, y_measure, size_measure = NULL,
                                      min_no_of_trees = 0.1*max(importance_frame$no_of_trees),
                                      x_label_prob = 0.01, y_label_prob = 0.01, size_label_prob = 0.01,
                                      main = "Multi-way importance plot"){
  data <- importance_frame[importance_frame$no_of_trees > min_no_of_trees, ]
  if(x_measure %in% c("no_of_nodes", "no_of_trees", "times_a_root", "MeanDecreaseGini")){
    x_label_prob_down <- 1 - x_label_prob
    x_label_prob_up <- 1
  } else {
    x_label_prob_down <- 0
    x_label_prob_up <- x_label_prob
  }
  if(y_measure %in% c("no_of_nodes", "no_of_trees", "times_a_root", "MeanDecreaseGini")){
    y_label_prob_down <- 1 - y_label_prob
    y_label_prob_up <- 1
  } else {
    y_label_prob_down <- 0
    y_label_prob_up <- y_label_prob
  }
  if(size_measure %in% c("no_of_nodes", "no_of_trees", "times_a_root", "MeanDecreaseGini")){
    size_label_prob_down <- 1 - size_label_prob
    size_label_prob_up <- 1
  } else {
    size_label_prob_down <- 0
    size_label_prob_up <- size_label_prob
  }
  data_for_labels <-
    importance_frame[importance_frame[[x_measure]] > quantile(importance_frame[[x_measure]], x_label_prob_down, na.rm = TRUE) &
                       importance_frame[[x_measure]] < quantile(importance_frame[[x_measure]], x_label_prob_up, na.rm = TRUE) &
                       importance_frame[[y_measure]] > quantile(importance_frame[[y_measure]], y_label_prob_down, na.rm = TRUE) &
                       importance_frame[[y_measure]] < quantile(importance_frame[[y_measure]], y_label_prob_up, na.rm = TRUE) &
                       importance_frame[[size_measure]] > quantile(importance_frame[[size_measure]], size_label_prob_down, na.rm = TRUE) &
                       importance_frame[[size_measure]] < quantile(importance_frame[[size_measure]], size_label_prob_up, na.rm = TRUE), ]
  plot <- ggplot(data, aes_string(x = x_measure, y = y_measure, size = size_measure)) + geom_point() +
    geom_label_repel(data = data_for_labels, aes(label = variable)) +
    geom_point(data = data_for_labels, color = "blue")
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
