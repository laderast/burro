cat_tabular_ui <- function(id, categoricalVars){

  fillCol(flex = c(NA,2,1),
          inputPanel(
            selectInput(inputId = NS(id, "crossTab1"),
                        "Select Crosstab Variable (x)",
                        choices=categoricalVars,
                        selected=categoricalVars[1]),
            selectInput(inputId = NS(id, "crossTab2"),
                        "Select Crosstab Variable (y)",
                        choices=categoricalVars,
                        selected=categoricalVars[2])),
          plotly::plotlyOutput(NS(id, "cross_size")),
          verbatimTextOutput(NS(id,"crossTab"))
  )

}

cat_tabular_server <- function(id, dataOut){
  moduleServer(id, function(input, output, session){

  output$crossTab <- renderPrint({

    out <- dataOut()[,c(input$crossTab1, input$crossTab2), with=FALSE]
    tab <- table(out, useNA = "ifany")
    tab
  })

  output$cross_size <- plotly::renderPlotly({
    outplot <- dataOut() %>%
      data.frame() %>%
      ggplot(aes_string(y=input$crossTab1, x=input$crossTab2)) +
      geom_count() +
      theme(axis.text.x=element_text(angle=90))

    plotly::ggplotly(outplot, tooltip = "all")
  })
  })

}
