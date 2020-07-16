cont_boxplot_ui <- function(id, numericVars, categoricalVars){

  fillCol(flex=c(NA,1),
          inputPanel(
            selectInput(inputId = NS(id,"numericVarBox"), "Select Numeric Variable",
                        choices = numericVars, selected=numericVars[1]),
            selectInput(inputId = NS(id,"catVarBox"), "Select Category Variable",
                        choices = categoricalVars, selected=categoricalVars[1])),
          plotOutput(NS(id,"boxPlot")))

}

cont_boxplot_server <- function(id, dataOut){
  moduleServer(id, function(input, output, session){
    output$boxPlot <- renderPlot({
      outPlot <- ggplot(dataOut(), aes_string(x=input$catVarBox,
                                              y=input$numericVarBox,
                                              fill=input$catVarBox)) +
        geom_boxplot() + theme(text=element_text(size=20), axis.text.x =
                                 element_text(angle=90)) +
        theme(legend.position = "none")
      outPlot
    })

  })
}


#' Explore the relationship between continous and categorical
#' data
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(diamonds)
#' cont_boxplot_app(diamonds)
#'
cont_boxplot_app <- function(dataset, height=NULL){

  id <- "new_app"
  my_data_table <- check_data(dataset)
  dataOut <- reactive({my_data_table})

  numericVars <- attr(my_data_table, "numericVars")
  categoricalVars <- attr(my_data_table, "categoricalVars")
  outcome_var <- attr(my_data_table, "outcome_var")
  cat_no_outcome <- attr(my_data_table, "cat_no_outcome")


  ui <-fillPage(
    cont_boxplot_ui(id, numericVars, categoricalVars),
    height=height
  )

  server <- function(input, output, session){
    cont_boxplot_server(id, dataOut)
  }

  shinyApp(ui, server, options=list(height=height))

}

