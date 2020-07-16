cat_tabular_ui <- function(id, categoricalVars, fill=TRUE){

  out_html <- fillCol(flex = c(NA,2,1),
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
          tagList(#verbatimTextOutput(NS(id,"crossTab")),
                  verbatimTextOutput(NS(id, "code")))
  )
  return(out_html)

  }

cat_tabular_ui_app <- function(id, categoricalVars){

  out_html <- tagList(
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
                      tagList(#verbatimTextOutput(NS(id,"crossTab")),
                              verbatimTextOutput(NS(id, "code")))
  )

  return(out_html)

}

cat_tabular_server <- function(id, dataOut, dataset_name){
  moduleServer(id, function(input, output, session){

  output$crossTab <- renderPrint({

    out <- dataOut()[,c(input$crossTab1, input$crossTab2), with=FALSE]
    tab <- table(out, useNA = "ifany")
    tab
  })

  output$code <- renderPrint({

    ex <- as.character(gg_exp())
    if(!is.null(dataset_name)){
      ex <- gsub("dataOut()", dataset_name, ex, fixed=TRUE)
    }
    if(!is.null(input$crossTab1)){
      ex <- gsub("input$crossTab1", input$crossTab1, ex, fixed=TRUE)
      ex <- gsub("input$crossTab2", input$crossTab2, ex, fixed=TRUE)
    }

    styler::style_text(ex)

  })

  gg_exp <- reactive(expression(dataOut() %>%
                         data.frame() %>%
                         ggplot(aes_string(y=input$crossTab1, x=input$crossTab2)) +
                         geom_count() +
                         theme(axis.text.x=element_text(angle=90))))

  output$cross_size <- plotly::renderPlotly({
    outplot <- eval(gg_exp())

    plotly::ggplotly(outplot, tooltip = "all")
  })
  })

}


cat_tabular_app <- function(dataset, height = 600){

  id <- "new_app"
  my_data_table <- check_data(dataset)
  dataOut <- reactive({my_data_table})
  dataset_name <- deparse(substitute(dataset))

  numericVars <- attr(my_data_table, "numericVars")
  categoricalVars <- attr(my_data_table, "categoricalVars")
  outcome_var <- attr(my_data_table, "outcome_var")
  cat_no_outcome <- attr(my_data_table, "cat_no_outcome")

  ui <- fillPage(
    cat_tabular_ui_app(id, categoricalVars)
  )

  server <- function(input, output, session){
    cat_tabular_server(id, dataOut, dataset_name)
  }

  shinyApp(ui, server, options=list(height=height))

}
