#' Calculate the depth of each node in a tree
#'
#' @param frame A data frame returned by the function getTree() applied to a random forest
#' @return The original data frame with an additional column \code{depth}
#' @examples
#' calculate_tree_depth(getTree(randomForest(Species ~ ., data = iris), k = 1, labelVar = T))
#' @export
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
#' @export
min_depth_distribution <- function(forest){
  if(!("randomForest" %in% class(forest))) stop("The object you supplied is not a random forest!")
  forest_table <-
    lapply(1:forest$ntree, function(i) randomForest::getTree(forest, k = i, labelVar = T) %>%
             calculate_tree_depth() %>% cbind(tree = i)) %>%
    data.table::rbindlist()
  min_depth_frame <- dplyr::group_by(forest_table, tree, `split var`) %>%
    dplyr::summarize(min(depth))
  colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
  min_depth_frame <- as.data.frame(min_depth_frame[!is.na(min_depth_frame$variable),])
  return(min_depth_frame)
}

#' Count the trees in which each variable had a given minimal depth
#'
#' @param min_depth_frame A data frame output of min_depth_distribution or min_depth_distribution_memory function
#' @return A data frame with count of occurances of each minimal depth value including NA's
#' @examples
#' min_depth_count(min_depth_distribution(randomForest(Species ~ ., data = iris)))
#' @export
min_depth_count <- function(min_depth_frame){
  min_depth_count <- dplyr::group_by(min_depth_frame, variable, minimal_depth) %>%
    dplyr::summarize(count = n()) %>% as.data.frame()
  occurances <- aggregate(count ~ variable, data = min_depth_count, sum)
  colnames(occurances)[2] <- "no_of_occurances"
  min_depth_count <-
    data.frame(variable = occurances$variable, minimal_depth = NA, count = max(min_depth_frame$tree) - occurances$no_of_occurances) %>%
    rbind(min_depth_count)
  min_depth_count <- min_depth_count[order(min_depth_count$variable, min_depth_count$minimal_depth),]
  rownames(min_depth_count) <- 1:nrow(min_depth_count)
  return(list(min_depth_count, occurances))
}

#' Plot the distribution of minimal depth in a random forest
#'
#' @param min_depth_frame A data frame output of min_depth_distribution or min_depth_distribution_memory function
#' @param k The maximal number of variables with highest mean minimal depth to be used for plotting
#' @param min_no_of_trees The minimal number of trees in which a variable has to be used for splitting to be used for plotting
#' @return A ggplot object
#' @examples
#' plot_min_depth_distribution(min_depth_distribution(randomForest(Species ~ ., data = iris)))
#' @export
plot_min_depth_distribution <- function(min_depth_frame, k = 10, min_no_of_trees =
                                          round(0.5 * max(min_depth_count(min_depth_frame)[[2]]$no_of_occurances))){
  min_depth_count_list <- min_depth_count(min_depth_frame)
  min_depth_count_list[[1]][is.na(min_depth_count_list[[1]]$minimal_depth), "count"] <-
    min_depth_count_list[[1]][is.na(min_depth_count_list[[1]]$minimal_depth), "count"] -
    min(min_depth_count_list[[1]][is.na(min_depth_count_list[[1]]$minimal_depth), "count"])
  variables <- min_depth_count_list[[2]][min_depth_count_list[[2]]$no_of_occurances >= min_no_of_trees, "variable"]
  min_depth_frame <- min_depth_frame[min_depth_frame$variable %in% variables, ]
  min_depth_count_list[[1]] <- min_depth_count_list[[1]][min_depth_count_list[[1]]$variable %in% variables, ]
  min_depth_means <- aggregate(minimal_depth ~ variable, data = min_depth_frame, mean)
  colnames(min_depth_means)[2] <- "mean_minimal_depth"
  frame_with_means <- merge(min_depth_count_list[[1]], min_depth_means)
  frame_with_means$mean_minimal_depth <- frame_with_means$mean_minimal_depth *
    max(min_depth_count_list[[2]]$no_of_occurances) / max(min_depth_frame$minimal_depth, na.rm = TRUE)
  frame_with_means$minimal_depth <- as.factor(frame_with_means$minimal_depth)
  frame_with_means <-
    within(frame_with_means, variable <-
             factor(variable, levels = rev(min_depth_means[order(min_depth_means$mean_minimal_depth), "variable"])))
  plot <- ggplot(frame_with_means[frame_with_means$variable %in% levels(frame_with_means$variable)[1:min(k, nrow(min_depth_count_list[[2]]))], ],
                 aes(x = variable, y = count, fill = minimal_depth)) +
    geom_col(position = position_stack(reverse = TRUE)) + coord_flip() +
    geom_errorbar(aes(ymin = mean_minimal_depth, ymax = mean_minimal_depth), size = 1.5) +
    xlab("Variable") + ylab("Number of trees") + guides(fill = guide_legend(title = "Minimal depth")) +
    ggtitle("The distribution of minimal depth")
  return(plot)
}
