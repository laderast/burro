cat_missing_ui <- function(id, categoricalVars){
  fillCol(flex=c(NA,1),
          selectInput(inputId = NS(id, "missingVar"),
                      "Select Variable to Examine",
                      choices=categoricalVars, selected = categoricalVars[1]),
          plotOutput(NS(id,"missingTab"))
  )

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
