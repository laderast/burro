cont_hist_ui <- function(id, numericVars){

  fillCol(flex=c(NA,1), inputPanel(
    selectInput(inputId = NS(id, "numericVarHist"), "Select Numeric Variable",
                choices = numericVars, selected=numericVars[1]),
    sliderInput(NS(id, "bins"), "Number of bins:", min = 1, max = 50,value = 30)
  ),
  plotOutput(NS(id, "histPlot"))
  )

}

cont_hist_server <- function(id, dataOut){
  moduleServer(id, function(input, output, session){
    output$histPlot <- renderPlot({

    outPlot <- ggplot(dataOut(), aes_string(x=input$numericVarHist)) +
      geom_histogram(bins=input$bins) +
      theme(text=element_text(size=20),
            axis.text.x = element_text(angle=90))
  outPlot
})
})
}

#' App for exploring data using histograms
#'
#' This is an embeddable app that lets you explore the distribution
#' of continuous variables
#'
#' This can be embedded into rmarkdown documents that use
#' runtime: shiny.
#'
#' @param dataset - dataset that contains numeric variables
#'
#'
#' @return Shiny App
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' data(diamonds)
#' cont_hist_app(diamonds)
#'
cont_hist_app <- function(dataset){

  id <- "new_app"
  my_data_table <- check_data(dataset)
  dataOut <- reactive({my_data_table})

  numericVars <- attr(my_data_table, "numericVars")
  categoricalVars <- attr(my_data_table, "categoricalVars")
  outcome_var <- attr(my_data_table, "outcome_var")
  cat_no_outcome <- attr(my_data_table, "cat_no_outcome")


  ui <- fluidPage(
    cont_hist_ui(id, numericVars)
  )

  server <- function(input, output, session){
    cont_hist_server(id, dataOut)
  }

  shinyApp(ui, server)

}
