cat_outcome_ui <- function(id, outcome_var, cat_no_outcome){
  fillCol(flex = c(NA,1),
          inputPanel(
            selectInput(inputId = NS(id, "condTab"), "Select Variable",
                        choices=cat_no_outcome, selected=cat_no_outcome[1]),

            selectInput(inputId = NS(id, "outcomeTab"), "Select Outcome Variable",
                        choices=outcome_var, selected=outcome_var[2])),

          plotlyOutput(NS(id, "proportionBarplot"))

  )

}

cat_outcome_server <- function(id, outcome_var, cat_no_outcome, dataOut){
  moduleServer(id, function(input, output, session){
    proportionTable <- reactive({
      out <- dataOut()[,c(input$condTab,
                          input$outcomeTab), with=FALSE]
      out
    })

    output$proportionTab <- renderPrint({
      tab <- table(proportionTable(), useNA="ifany")
      return(tab[,"Yes"]/(tab[,"No"] + tab[,"Yes"]))
    })


    output$proportionBarplot <- renderPlotly({
      #need to figure out how to calculate cumulative sum?
      #https://stackoverflow.com/questions/43520318/how-to-use-percentage-as-label-in-stacked-bar-plot

      out_plot <- burro:::percent_plot(proportion_table = proportionTable(),
                                       outcome_var = input$outcomeTab,
                                       condition_var = input$condTab) +
        theme(legend.position = "None")

      ggplotly(out_plot, tooltip = c("x", "fill", "y"))

    })

  })
}

