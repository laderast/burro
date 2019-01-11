## Some helper functions to simplify our app


get_numeric_variables <- function(df){
  varClass <- sapply(df, class)

  numericVars <- names(varClass[varClass %in% c("numeric", "integer")])
  return(numericVars)
}


get_category_variables <- function(df) {

  ## get variable classes
  varClass <- sapply(df, class)

  varClass <- sapply(varClass, function(x){x[1]})

  ## grab characters - they may be categorical, or unique ids
  characterVars <- names(varClass[varClass %in% c("character", "factor", "ordered")])

  #remove those character variables that aren't
  char_list <- sapply(characterVars, function(x){
    size_vec <- length(df[[x]])
    cate_vec <- length(unique(df[[x]]))
    ##This is a bit o a kludge - don't want to return categories which
    ##have levels bigger than nrow(df)/2
    if(cate_vec > (size_vec/2)){
      out <- NULL
    } else{
      out <- as.character(unique(df[[x]]))
    }
    return(out)
  })

  ##remove null values from list
  char_list <- char_list[lapply(char_list,length)!=0]

  return(char_list)

}



data_dictionary_input <- function(id, choices){

  ns <- NS(id)

  tagList(
    selectInput("variable_id", "Select Variable to look up",
      choices=choices, selected=choices[1]),
    textOutput(ns("dict_entry"))
  )

}

data_dictionary <- function(input, output, session, data_dict){
  uiEntry <- reactive({
    entry <- sym(input$variable_id)
    data_dict %>% filter(id == !!entry)
  })

  output$dict_entry <- renderDT({

  })
}
