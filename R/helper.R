## Some helper functions to simplify our app


get_numeric_variables <- function(df){
  varClass <- sapply(df, class)

  numericVars <- names(varClass[varClass %in% c("numeric", "integer", "Date")])

  if(length(numericVars) == 0){
    numericVars <- NULL
  }

  return(numericVars)
}


get_category_variables <- function(df) {

  ## get variable classes
  varClass <- sapply(df, class)

  varClass <- sapply(varClass, function(x){x[1]})

  ## grab characters - they may be categorical, or unique ids
  characterVars <- names(varClass[varClass %in% c("character", "factor", "ordered", "logical")])

  #remove those character variables that aren't
  char_list <- lapply(characterVars, function(x){
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
  char_list <- as.list(char_list)

  names(char_list) <- characterVars

  ##remove null values from list
  char_list <- char_list[lapply(char_list,length)!=0]

  if(length(char_list) == 0){
    char_list <- NULL
  }

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


percent_table <- function(proportion_table, condition_var, outcome_var){

  if(condition_var == outcome_var){
    return(NULL)
  }

  percent_table <- proportion_table %>% data.frame() %>%
    group_by(!!sym(condition_var)) %>%
    count(!!sym(outcome_var)) %>%
    mutate(ratio=scales::percent(n/sum(n)), ratio2 = n/sum(n),
           pos=1-cumsum(ratio2), !!sym(outcome_var) := forcats::fct_rev(!!sym(outcome_var)))

  percent_table

}

percent_plot <- function(proportion_table, outcome_var, condition_var){

  ov <- sym(outcome_var)
  cv <- sym(condition_var)

  proportion_table <- proportion_table %>% mutate( !!ov :=
                                                     forcats::fct_rev(!!ov))

  per_tab <- percent_table(proportion_table = proportion_table,
                           outcome_var = outcome_var,
                           condition_var = condition_var)

  if(is.null(per_tab)){
    return(NULL)
  }

  out_plot <- proportion_table %>%
    ggplot(aes(x=!!cv, fill=!!ov)) +
    geom_bar(position="fill", color="black") +
    theme(text=element_text(size=20), axis.text.x = element_text(angle = 90)) +
    geom_label(data = per_tab, mapping = aes(y=pos, label=ratio), fill="white"
              , vjust= -0.5) +
    viridis::scale_fill_viridis(discrete=TRUE, option="magma")

  out_plot
}
