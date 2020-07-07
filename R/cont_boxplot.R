cont_boxplot_ui <- function(id, numericVars, categoricalVars){

  fillCol(flex=c(NA,1),
          inputPanel(
            selectInput(inputId = NS(id,"numericVarBox"), "Select Numeric Variable",
                        choices = numericVars, selected=numericVars[1]),
            selectInput(inputId = NS(id,"catVarBox"), "Select Category to Condition on",
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
