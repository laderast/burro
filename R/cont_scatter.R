cont_scatter_ui <- function(id, numericVars){
  fillCol( flex=c(NA,1),
           inputPanel(
             selectInput(NS(id,"x_var"),
                         "Select Y Variable",
                         choices=numericVars,
                         selected = numericVars[1]),
             selectInput(NS(id,"y_var"),
                         "Select Y Variable",
                         choices=numericVars,
                         selected = numericVars[2])
           ),
           plotOutput(NS(id,"corr_plot"))
  )


}

cont_scatter_server <- function(id, dataOut){
  moduleServer(id, function(input, output, session){
    output$corr_plot <- renderPlot({

      mini_frame <- dataOut() %>%
        data.frame() %>%
        select(!!sym(input$x_var), !!sym(input$y_var)) %>%
        tidyr::drop_na()
      xcol <- mini_frame %>% pull(!!sym(input$x_var))
      ycol <- mini_frame %>% pull(!!sym(input$y_var))

      corval <- signif(cor(xcol, ycol), digits = 3)

      ggplot(dataOut(), aes_string(x=input$x_var, y=input$y_var)) +
        naniar::geom_miss_point() + stat_smooth(method=lm, se=FALSE) +
        #viridis::scale_color_viridis(discrete = TRUE, option="magma") +
        ggtitle(paste(input$x_var, "vs.", input$y_var, "correlation =", corval))
    })

  })
}


#' App for exploring data using scatter plots
#'
#' This is an embeddable app that lets you explore the relationship
#' between continuous variables.
#'
#' This can be embedded into rmarkdown documents that use
#' runtime: shiny.
#'
#' @param dataset
#'
#' @return shiny app that can be embedded in a doc
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' data(diamonds)
#' cont_scatter_app(diamonds)
cont_scatter_app <- function(dataset){

  id <- "new_app"
  my_data_table <- check_data(dataset)
  dataOut <- reactive({my_data_table})

  numericVars <- attr(my_data_table, "numericVars")
  categoricalVars <- attr(my_data_table, "categoricalVars")
  outcome_var <- attr(my_data_table, "outcome_var")
  cat_no_outcome <- attr(my_data_table, "cat_no_outcome")


  ui <- fluidPage(
    cont_scatter_ui(id, numericVars)
  )

  server <- function(input, output, session){
    cont_scatter_server(id, dataOut)
  }

  shinyApp(ui, server)

}

