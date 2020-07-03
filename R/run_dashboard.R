#' Explore data with a shiny app
#'
#' @param dataset - dataset to explore
#' @param data_dictionary - optional. Data dictionary with
#' information about variables
#'
#' @return
#' @export
#'
#' @examples
explore_data2 <- function(dataset, data_dictionary=NULL){

  path=system.file("app/dashboard.Rmd", package="burro")

  dataset_name <- deparse(substitute(dataset))
  rmarkdown::run(path)

}
