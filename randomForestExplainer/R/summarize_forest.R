summarize_forest <- function(){
  rmarkdown::render(paste0(path.package("randomForestExplainer"), "/templates/Summarize_forest_template.rmd"),
                    "html_document", output_file = "Summary_of_your_forest.html", envir = new.env(parent = globalenv()))
}
