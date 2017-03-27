#' Calculate the depth of each node in a tree
#'
#' @param frame A data frame returned by the function getTree() applied to a random forest
#' @return The original data frame with an additional column \code{depth}
#' @examples
#' calculate_tree_depth(getTree(randomForest(Species ~ ., data = iris), k = 1, labelVar = T))
calculate_tree_depth <- function(frame){
  if(!is.data.frame(frame)) stop("The object is not a data frame!")
  if(!all(c("right daughter", "left daughter") %in% names(frame))){
    stop("The data frame has to contain columns called 'right daughter' and 'left daughter'!
          It should be a product of the function getTree(..., labelVar = T).")
  }
  frame$depth <- NA
  frame$depth[1] <- 0
  for(i in 2:nrow(frame)){
    frame[i, "depth"] <-
      frame[frame[, "left daughter"] == as.numeric(rownames(frame[i,])) |
            frame[, "right daughter"] == as.numeric(rownames(frame[i,])), "depth"] + 1
  }
  return(frame)
}

#' Get minimal depth values for all trees in a random forest
#'
#' @param forest A randomForest object
#' @return A data frame with the value of minimal depth for every variable in every tree
#' @examples
#' min_depth_distribution(randomForest(Species ~ ., data = iris))
min_depth_distribution <- function(forest){
  if(!("randomForest" %in% class(forest))) stop("The object you supplied is not a random forest!")
  forest_table <-
    lapply(1:forest$ntree, function(i) randomForest::getTree(forest, k = i, labelVar = T) %>%
             calculate_tree_depth() %>% cbind(tree = i)) %>%
    data.table::rbindlist()
  min_depth_frame <- dplyr::group_by(forest_table, tree, `split var`) %>%
    dplyr::summarize(min(depth))
  colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
  min_depth_frame <- min_depth_frame[!is.na(min_depth_frame$variable),]
  all_variables <- expand.grid(variable = unique(min_depth_frame$variable), tree = 1:forest$ntree)
  min_depth_frame <- merge(min_depth_frame, all_variables, all = TRUE)
  return(min_depth_frame)
}

#' Plot the distribution of minimal depth in a random forest
#'
#' @param min_depth_frame A min_depth_distribution object
#' @return A ggplot object
#' @examples
#' plot_min_depth_distribution(min_depth_distribution(randomForest(Species ~ ., data = iris)))
plot_min_depth_distribution <- function(min_depth_frame){
  min_depth_means <- aggregate(minimal_depth ~ variable, data = min_depth_frame, mean)
  colnames(min_depth_means)[2] <- "mean_minimal_depth"
  min_depth_count <- dplyr::group_by(min_depth_frame, variable, minimal_depth) %>%
    dplyr::summarize(count = n()) %>% as.data.frame()
  frame_with_means <- merge(min_depth_count, min_depth_means)
  frame_with_means$mean_minimal_depth <- frame_with_means$mean_minimal_depth *
    max(min_depth_frame$tree) / max(min_depth_frame$minimal_depth, na.rm = TRUE)
  frame_with_means$minimal_depth <- as.factor(frame_with_means$minimal_depth)
  frame_with_means <-
    within(frame_with_means, variable <-
             factor(variable, levels = rev(min_depth_means[order(min_depth_means$mean_minimal_depth), "variable"])))
  plot <- ggplot(frame_with_means, aes(x = variable, y = count, fill = minimal_depth)) +
    geom_col(position = position_stack(reverse = TRUE)) + coord_flip() +
    geom_errorbar(aes(ymin = mean_minimal_depth, ymax = mean_minimal_depth), size = 1.5) +
    xlab("Variable") + ylab("Number of trees") + guides(fill = guide_legend(title = "Minimal depth")) +
    ggtitle("The distribution of minimal depth among the trees of the forest")
  return(plot)
}
