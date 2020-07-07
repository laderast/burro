cat_single_ui <- function(id, categoricalVars){

  fillCol(flex = c(NA, 1), inputPanel(selectInput(inputId = NS(id, "singleVar"),
                                       "Select Categorical Variable",
                                       choices = categoricalVars,
                                       selected =categoricalVars[1])),
          plotOutput(NS(id, "singleTab"))
  )
}

cat_single_server <- function(id, categoricalVars, dataOut){
  moduleServer(id, function(input, output, session){
    output$singleTab <- renderPlot({

      if(is.null(categoricalVars)){
        return(NULL)
      }

      dataOut() %>%
        #data.frame() %>%
        mutate(gr = 1) %>%
        ggplot(aes_string(x=input$singleVar, fill=input$singleVar)) +
        geom_bar(aes(y = ..count..), color="black") +
        viridis::scale_fill_viridis(discrete=TRUE, option="magma") +
        geom_text(aes(group=gr, label = scales::percent(..prop..),
                      y= ..count..), stat= "count", vjust=-0.5) +
        theme(axis.text.x=element_text(angle=90), legend.position = "none")
    })

  })
}
