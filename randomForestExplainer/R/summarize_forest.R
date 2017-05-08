summarize_forest <- function(){
  rmarkdown::render("Summarize_forest_template.rmd", "html_document", envir = new.env(parent = globalenv()))
}
