check_data <- function(dataset){
  if(!is.null(covariates)) {

    covariates_in_data <- covariates %in% colnames(dataset)
    num_in_data <- which(covariates_in_data)
    not_in_data <- which(!covariates_in_data)

    if(length(num_in_data) == 0) {
      stop("Your covariates aren't in the dataset - make sure they correpond to column names in the data")
    }

    if(length(not_in_data) > 0) {
      warning(
        paste0("The following covariates weren't in the dataset:", paste(covariates[not_in_data]))
      )
    }

  }


  myDataFrame <- data.table::data.table(dataset)

  if(!is.null(covariates)){
    myDataFrame <- myDataFrame[,covariates,with=FALSE]
  }

  categoricalVars <- sort(names(burro:::get_category_variables(myDataFrame)))
  outcome_var <- outcome_var[outcome_var %in% categoricalVars]

  if(is.null(outcome_var)){
    outcome_var <- categoricalVars
  }

  in_dataset <- length(which(outcome_var %in% colnames(dataset)))

  # need to check column names and outcome var names in data
  if(in_dataset == 0){
    stop("Your outcome variable is not the dataset - try using colnames(data) to select it")
  }
  if(in_dataset < length(outcome_var)){
    warning("Some of your outcomes weren't in the dataset")
  }

  #myDataFrame <- burro:::sanitize_data_frame(myDataFrame, outcome_var)
  remove_categories <- outcome_var

  cat_no_outcome <- categoricalVars

  if(length(outcome_var) != length(categoricalVars)){
    cat_no_outcome <-
      setdiff(categoricalVars, remove_categories)
  }

  numericVars <- sort(burro:::get_numeric_variables(myDataFrame))


}
