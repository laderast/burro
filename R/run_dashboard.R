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

#' Run the modularized flexdashboard
#'
#' @param dataset - a data frame
#' @param outcome_var - a categorical variable that describes an outcome to examine
#' @param data_dictionary - Optional. A data.frame describing covariates. This is passed
#' on to DT:data.table() so it can be easily searched. There is no required format,
#' but the example uses two columns: `VariableName` and `Description`.
#'
#' `explore_data` gives you a simple shiny app to explore data, The app is tailored to the
#'  covariate types of the data.frame/data.table that you pass to it.
#'
#'  Each menu item covers a different part of exploratory data analysis with
#'  simple visualizations for discussion.
#'
#'  The tabs are automatically updated with variables depending on the variable types.
#'
#'
#' @return A Shiny App object that can either be run in the console or on an external
#' shiny server such as shinyapps.io. For building the actual app.R, see
#' build_shiny_app
#' @export
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
#' @import dplyr
#' @import skimr
#' @examples
#' library(ggplot2)
#' data(diamonds)
#' if(interactive()){
#'    burro::explore_data(diamonds, outcome_var="cut")
#' }
#'
#' # example with data dictionary
#' # look at NHANES data (you need to have NHANES package installed)
#' library(NHANES)
#' data(NHANES)
#' if(interactive()){
#'   #this data dictionary is provided since NHANES doesn't have one
#'   data_dict <- read.csv(system.file("nhanes/data_dictionary.csv", package="burro"))
#'   burro::explore_data(NHANES[1:10,], data_dictionary=data_dict)
#' }
#'
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$am <- factor(mtcars$am)
#' mtcars$gear <- factor(mtcars$gear)
#'
#' if(interactive()){
#'    burro::explore_data(mtcars)
#' }
#'
explore_data <- function(dataset, data_dictionary=NULL, outcome_var=NULL){

  path=system.file("app/dashboard_module.Rmd", package="burro")

  dataset_name <- deparse(substitute(dataset))
  rmarkdown::run(path)

}
