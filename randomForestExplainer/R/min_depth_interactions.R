# For a vactor of conditioning variables insert values of conditional depth to a data frame of proper structure
conditional_depth <- function(frame, vars){
  index <- as.data.table(frame)[, .I[which.min(depth)], by = `split var`]
  if(any(index$`split var` %in% vars)){
    for(j in vars){
      begin <- as.numeric(index[index$`split var` == j, "V1"])
      if(!is.na(begin)){
        df <- frame[begin:nrow(frame), setdiff(names(frame), setdiff(vars, j))]
        df[[j]][1] <- 0
        for(k in 2:nrow(df)){
          if(length(df[df[, "left daughter"] == as.numeric(rownames(df[k,])) |
                       df[, "right daughter"] == as.numeric(rownames(df[k,])), j]) != 0){
            df[k, j] <-
              df[df[, "left daughter"] == as.numeric(rownames(df[k,])) |
                   df[, "right daughter"] == as.numeric(rownames(df[k,])), j] + 1
          }
        }
        frame[begin:nrow(frame), setdiff(names(frame), setdiff(vars, j))] <- df
      }
    }
  }
  return(frame)
}
#' Get a data frame with values of minimal depth conditional on selected variables
#'
#' @param forest A randomForest object
#' @param vars A character vector with variables with respect to which conditional minimal depth will be calculated
#' @return A data frame with the value of minimal depth conditional on variables from the supplied vector
#' @examples
#' min_depth_interactions(randomForest(Species ~ ., data = iris), vars = names(iris))
#' @export
min_depth_interactions <- function(forest, vars){
  if(!("randomForest" %in% class(forest))) stop("The object you supplied is not a random forest!")
  interactions_frame <-
    lapply(1:forest$ntree, function(i) randomForest::getTree(forest, k = i, labelVar = T) %>%
             calculate_tree_depth() %>% cbind(tree = i)) %>% data.table::rbindlist() %>% as.data.frame()
  interactions_frame[vars] <- as.numeric(NA)
  interactions_frame <-
    as.data.table(interactions_frame)[, conditional_depth(as.data.frame(.SD), vars), by = tree] %>% as.data.frame()
  min_depth_interactions_frame <-
    interactions_frame %>% dplyr::group_by(tree, `split var`) %>%
    dplyr::summarize_each_(funs(min(., na.rm = TRUE)), vars) %>% as.data.frame()
  min_depth_interactions_frame <- min_depth_interactions_frame[!is.na(min_depth_interactions_frame$`split var`),]
  colnames(min_depth_interactions_frame)[2] <- "variable"
  return(min_depth_interactions_frame)
}

#' Count the trees in which each variable had a given conditional minimal depth
#'
#' @param forest A randomForest object
#' @param vars A character vector with variables with respect to which conditional minimal depth will be calculated
#' @return A list with means of conditional minimal depth in the first element and the size of sample for each mean in the second element
#' @examples
#' min_depth_interactions_means(randomForest(Species ~ ., data = iris), vars = names(iris))
#' @export
min_depth_interactions_means <- function(forest, vars){
  min_depth_interactions_frame <- min_depth_interactions(forest, vars)
  min_depth_interactions_means <-
    min_depth_interactions_frame %>% dplyr::group_by(variable) %>%
    dplyr::summarize_each_(funs(mean(., na.rm = TRUE)), vars) %>% as.data.frame()
  min_depth_interactions_means[is.na(as.matrix(min_depth_interactions_means))] <- NA
  occurances <-
    min_depth_interactions_frame %>% dplyr::group_by(variable) %>%
    dplyr::summarize_each_(funs(sum(!is.na(.))), vars) %>% as.data.frame()
  return(list(min_depth_interactions_means, occurances))
}


