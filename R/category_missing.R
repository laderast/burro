cat_missing_ui <- function(id, categoricalVars, fillcol=TRUE){
  if(fillcol){
    out_html <- fillCol(flex=c(NA,1),
          selectInput(inputId = NS(id, "missingVar"),
                      "Select Variable to Examine",
                      choices=categoricalVars, selected = categoricalVars[1]),
          plotOutput(NS(id,"missingTab"))
    )}
  else{
    out_html <- tagList(
      selectInput(inputId = NS(id, "missingVar"),
                  "Select Variable to Examine",
                  choices=categoricalVars, selected = categoricalVars[1]),
      plotOutput(NS(id,"missingTab"))
    )

  }

}

cat_missing_server <- function(id, dataOut){
  moduleServer(id, function(input, output, session){
    output$missingTab <- renderPlot({

      var <- sym(input$missingVar)

      dataOut() %>%
        data.frame() %>%
        naniar::gg_miss_fct(fct = !!var) +
        theme(axis.text = element_text(size = 15))

    })
  })
}


cat_missing_app <- function(dataset){

  id <- "new_app"
  my_data_table <- check_data(dataset)
  dataOut <- reactive({my_data_table})

  numericVars <- attr(my_data_table, "numericVars")
  categoricalVars <- attr(my_data_table, "categoricalVars")
  outcome_var <- attr(my_data_table, "outcome_var")
  cat_no_outcome <- attr(my_data_table, "cat_no_outcome")


  ui <- fluidPage(
    cat_missing_ui(id, categoricalVars, fillcol = FALSE)
  )

  server <- function(input, output, session){
    cat_missing_server(id, dataOut)
  }

  shinyApp(ui, server)

}
