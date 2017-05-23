#' Summarize a random forest
#'
#' Summarizes a random forest in a html document using plots created by randomForestExplainer
#'
#' @param forest A randomForest object created with the option localImp = TRUE
#' @param vars A character vector with variables with respect to which interactions will be considered if NULL then they will be selected using the important_variables() function
#' @param measures A character vector specifying the importance measures to be used for plotting ggpairs
#' @param interactions Logical value: should variable interactions be considered (this may be time-consuming)
#'
#' @return A html document in your working directory
#'
#' @import DT
#'
#' @examples
#' summarize_forest(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE), vars = names(iris), interactions = TRUE)
#'
#' @export
summarize_forest <- function(forest, interactions = FALSE, vars = NULL,
                             measures = if(forest$type == "classification")
                               c("mean_min_depth", "accuracy_decrease", "gini_decrease", "no_of_nodes", "times_a_root") else
                                 c("mean_min_depth", "mse_increase", "node_purity_increase", "no_of_nodes", "times_a_root")){
  environment <- new.env()
  environment$forest <- forest
  environment$interactions <- interactions
  environment$vars <- vars
  environment$measures <- measures
  directory <- getwd()
  rmarkdown::render(paste0(path.package("randomForestExplainer"), "/templates/Summarize_forest_template.rmd"),
                    "html_document", output_file = paste0(directory, "/Summary_of_your_forest.html"),
                    envir = environment)
}

# To powinno być 1 argumentem rmarkdown::render na githubie:
# paste0(path.package("randomForestExplainer"), "/templates/Summarize_forest_template.rmd")
# A do pracy to: "BlackBoxOpener/randomForestExplainer/inst/templates/Summarize_forest_template.Rmd"
