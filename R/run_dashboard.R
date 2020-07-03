explore_data2 <- function(dataset, path="dashboard.Rmd", data_dictionary=NULL){

  dataset_name <- deparse(substitute(dataset))
  rmarkdown::run(path)

}
