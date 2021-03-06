#' Checks the Dataset and sets attributes on it for use in explore_data
#'
#' @param dataset - dataset
#' @param covariates - optional. Character vector containing columns to subset dataset to.
#' If NULL, then all columns will be used.
#' @param outcome_var - optional. Character vector containing columns of outcome.
#' These columns must be factors or characters. If NULL, all categorical variables
#' are used as possible outcomes
#'
#' @return data.table with the following attributes: outcome_var (validated outcomes),
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(diamonds)
#' diamond_frame <- check_data(diamonds, outcome_var=c("cut"))
#' attr(diamond_frame, "categoricalVars")
#' attr(diamond_frame, "numericVars")
#' attr(diamond_frame, "outcome_var")
check_data <- function(dataset, covariates=NULL, outcome_var=NULL){

    covariates_in_data <- covariates %in% colnames(dataset)

    num_in_data <- which(covariates_in_data)
    not_in_data <- which(!covariates_in_data)

    if(length(num_in_data) == 0 & length(covariates) > 0) {
        paste0("The following covariates weren't in the dataset:", paste(covariates[not_in_data]))
    }

  myDataFrame <- data.table::data.table(dataset)

  covariates <- covariates[covariates_in_data]

  if(length(covariates) > 0){
    myDataFrame <- myDataFrame[,covariates,with=FALSE]
  }

  categoricalVars <- sort(names(burro:::get_category_variables(myDataFrame)))
  outcome_var <- outcome_var[outcome_var %in% categoricalVars]

  if(is.null(outcome_var)){
    outcome_var <- categoricalVars
  }

  in_dataset <- length(which(outcome_var %in% colnames(dataset)))

  # need to check column names and outcome var names in data
  if(in_dataset == 0 & length(outcome_var) > 0){
    warning("Your outcome variable is not the dataset - using all categorical variables as outcome")
  }

  if(in_dataset < length(outcome_var)){
    warning("Some of your outcomes weren't in the dataset - using the ones we found")
  }

  remove_categories <- outcome_var
  cat_no_outcome <- categoricalVars

  if(length(outcome_var) != length(categoricalVars)){
    cat_no_outcome <-
      setdiff(categoricalVars, remove_categories)
  }

  numericVars <- sort(burro:::get_numeric_variables(myDataFrame))

  attr(myDataFrame, "outcome_var") <-  outcome_var
  attr(myDataFrame, "cat_no_outcome") <-  cat_no_outcome
  attr(myDataFrame, "categoricalVars") <-  categoricalVars
  attr(myDataFrame, "numericVars") <- numericVars

  return(myDataFrame)

}
