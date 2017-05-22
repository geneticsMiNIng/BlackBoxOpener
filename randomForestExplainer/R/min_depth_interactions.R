#' Calculate conditional depth in a tree
#'
#' For a vactor of conditioning variables insert values of conditional depth to a data frame of proper structure
#'
#' @param frame A data frame returned by the function calculate_tree_depth(getTree()) applied to a random forest
#' @param vars A character vector with variables with respect to which conditional minimal depth will be calculated
#'
#' @return The original data frame with an additional column \code{depth}
#'
#' @import data.table
#'
#' @examples
#' tree_frame <- randomForest::getTree(randomForest::randomForest(Species ~ ., data = iris), k = 1, labelVar = TRUE)
#' conditional_depth(calculate_tree_depth(cbind(tree_frame, number = 1:nrow(tree_frame))), vars = names(iris))
#'
#' @export
conditional_depth <- function(frame, vars){
  index <- as.data.table(frame)[, .SD[which.min(depth), "number"], by = `split var`]
  index <- index[!is.na(index$`split var`), ]
  if(any(index$`split var` %in% vars)){
    for(j in vars){
      begin <- as.numeric(index[index$`split var` == j, "number"])
      if(!is.na(begin)){
        df <- frame[begin:nrow(frame), setdiff(names(frame), setdiff(vars, j))]
        df[[j]][1] <- 0
        for(k in 2:nrow(df)){
          if(length(df[df[, "left daughter"] == as.numeric(df[k, "number"]) |
                       df[, "right daughter"] == as.numeric(df[k, "number"]), j]) != 0){
            df[k, j] <-
              df[df[, "left daughter"] == as.numeric(df[k, "number"]) |
                   df[, "right daughter"] == as.numeric(df[k, "number"]), j] + 1
          }
        }
        frame[begin:nrow(frame), setdiff(names(frame), setdiff(vars, j))] <- df
      }
    }
  }
  frame[frame == 0] <- NA
  return(frame)
}
#' Calculate conditional minimal depth
#'
#' Get a data frame with values of minimal depth conditional on selected variables
#'
#' @param forest A randomForest object
#' @param vars A character vector with variables with respect to which conditional minimal depth will be calculated
#'
#' @return A data frame with the value of minimal depth conditional on variables from the supplied vector
#'
#' @import data.table
#' @import dplyr
#'
#' @examples
#' min_depth_interactions_values(randomForest::randomForest(Species ~ ., data = iris), vars = names(iris))
#'
#' @export
min_depth_interactions_values <- function(forest, vars){
  if(!("randomForest" %in% class(forest))) stop("The object you supplied is not a random forest!")
  interactions_frame <-
    lapply(1:forest$ntree, function(i) randomForest::getTree(forest, k = i, labelVar = T) %>%
             calculate_tree_depth() %>% cbind(., tree = i, number = 1:nrow(.))) %>% data.table::rbindlist() %>% as.data.frame()
  interactions_frame[vars] <- as.numeric(NA)
  interactions_frame <-
    as.data.table(interactions_frame)[, conditional_depth(as.data.frame(.SD), vars), by = tree] %>% as.data.frame()
  min_depth_interactions_frame <-
    interactions_frame %>% dplyr::group_by(tree, `split var`) %>%
    dplyr::summarize_each_(funs(min(., na.rm = TRUE)), vars) %>% as.data.frame()
  min_depth_interactions_frame <- min_depth_interactions_frame[!is.na(min_depth_interactions_frame$`split var`),]
  colnames(min_depth_interactions_frame)[2] <- "variable"
  min_depth_interactions_frame[, -c(1:2)] <- min_depth_interactions_frame[, -c(1:2)] - 1
  return(min_depth_interactions_frame)
}

#' Calculate mean conditional minimal depth
#'
#' Calculate mean conditional minimal depth with respect to a vector of variables
#'
#' @param forest A randomForest object
#' @param vars A character vector with variables with respect to which conditional minimal depth will be calculated
#'
#' @return A data frame with each observarion giving the means of conditional minimal depth and the size of sample for a given interaction
#'
#' @import dplyr
#'
#' @examples
#' min_depth_interactions(randomForest::randomForest(Species ~ ., data = iris), vars = names(iris))
#'
#' @export
min_depth_interactions <- function(forest, vars){
  if(!("randomForest" %in% class(forest))) stop("The object you supplied is not a random forest!")
  min_depth_interactions_frame <- min_depth_interactions_values(forest, vars)
  interactions_frame <-
    min_depth_interactions_frame %>% dplyr::group_by(variable) %>%
    dplyr::summarize_each_(funs(mean(., na.rm = TRUE)), vars) %>% as.data.frame()
  interactions_frame[is.na(as.matrix(interactions_frame))] <- NA
  interactions_frame <- reshape2::melt(interactions_frame, id.vars = "variable")
  colnames(interactions_frame)[2:3] <- c("root_variable", "mean_min_depth")
  occurances <-
    min_depth_interactions_frame %>% dplyr::group_by(variable) %>%
    dplyr::summarize_each_(funs(sum(!is.na(.))), vars) %>% as.data.frame()
  occurances <- reshape2::melt(occurances, id.vars = "variable")
  colnames(occurances)[2:3] <- c("root_variable", "occurances")
  interactions_frame <- merge(interactions_frame, occurances)
  interactions_frame$interaction <- paste(interactions_frame$root_variable, interactions_frame$variable, sep = ":")
  return(interactions_frame)
}

#' Plot the top mean conditional minimal depth
#'
#' @param interactions_frame A data frame produced by the min_depth_interactions_means() function
#' @param k The number of best interactions to plot, if set to NULL then all plotted
#' @param main A string to be used as title of the plot
#' @return A ggplot2 object
#' @import ggplot2
#' @examples
#' plot_min_depth_interactions(min_depth_interactions(randomForest::randomForest(Species ~ ., data = iris), vars = names(iris)))
#' @export
plot_min_depth_interactions <- function(interactions_frame, k = 30,
                                        main = paste0("Mean minimal depth for ",
                                                      paste0(k, " most frequent interactions"))){
  interactions_frame$interaction <- factor(interactions_frame$interaction, levels =
                                             interactions_frame[
                                               order(interactions_frame$occurances, decreasing = TRUE), "interaction"])
  minimum <- min(interactions_frame$mean_min_depth, na.rm = TRUE)
  if(is.null(k)) k <- length(levels(interactions_frame$interaction))
  plot <- ggplot(interactions_frame[interactions_frame$interaction %in% levels(interactions_frame$interaction)[1:k] &
                                      !is.na(interactions_frame$mean_min_depth), ],
                 aes(x = interaction, y = mean_min_depth, fill = occurances)) +
    geom_bar(stat = "identity") +
    geom_hline(aes(yintercept = minimum, linetype = "minimum"), color = "red", size = 1.5) +
    scale_linetype_manual(name = NULL, values = 1) + theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}
