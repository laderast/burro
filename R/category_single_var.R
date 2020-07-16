cat_single_ui <- function(id, categoricalVars){
  shinyjs::useShinyjs()

  fillCol(flex=c(NA, 4, 1),
    inputPanel(selectInput(inputId = NS(id, "singleVar"),
               "Select Categorical Variable",
              choices = categoricalVars,
              selected =categoricalVars[1])),
    plotOutput(NS(id, "singleTab")),
    verbatimTextOutput(NS(id, "code"))
  )

}

cat_single_ui_app <- function(id, categoricalVars){
  shinyjs::useShinyjs()

  tagList(
    selectInput(inputId = NS(id, "singleVar"),
                "Select Categorical Variable",
                choices = categoricalVars,
                selected =categoricalVars[1]),
    plotOutput(NS(id, "singleTab")),
    verbatimTextOutput(NS(id, "code"))
  )

}


cat_single_server <- function(id, dataOut, dataset_name=NULL){
  moduleServer(id, function(input, output, session){

    #observeEvent(input$push, {
    #  shinyjs::toggle("text_div", FALSE)

    #})

    output$code <- renderPrint({

      ex <- as.character(plot_expression())
      if(!is.null(dataset_name)){
      ex <- gsub("dataOut()", dataset_name, ex, fixed=TRUE)
      }
      if(!is.null(input$singleVar)){
      ex <- gsub("input$singleVar", input$singleVar, ex, fixed=TRUE)
      }

      styler::style_text(ex)

    })

    plot_expression <- reactive({
      expression(
        dataOut() %>%
          mutate(gr = 1) %>%
          ggplot(aes_string(x=input$singleVar, fill=input$singleVar)) +
          geom_bar(aes(y = ..count..), color="black") +
          viridis::scale_fill_viridis(discrete=TRUE, option="magma") +
          geom_text(aes(group=gr, label = scales::percent(..prop..),
                        y= ..count..), stat= "count", vjust=-0.5) +
          theme(axis.text.x=element_text(angle=90), legend.position = "none"))

    })


    output$singleTab <- renderPlot({

      eval(plot_expression())
    })

  })
}


cat_single_app <- function(dataset, height=600){

  id <- "new_app"
  my_data_table <- check_data(dataset)
  dataOut <- reactive({my_data_table})

  dataset_name <- deparse(substitute(dataset))

  categoricalVars <- attr(my_data_table, "categoricalVars")

  ui <- miniPage(
    cat_single_ui(id, categoricalVars = categoricalVars)
  )

  server <- function(input, output, session){
    cat_single_server(id, dataOut, dataset_name)
  }

  shinyApp(ui, server, options=list(height=height))

}


#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#' @import miniUI
#'
#' @examples
mini_ui_category_app <- function(dataset){

  id <- "new_app"
  my_data_table <- check_data(dataset)

  dataset_name <- deparse(substitute(dataset))

  numericVars <- attr(my_data_table, "numericVars")
  categoricalVars <- attr(my_data_table, "categoricalVars")
  outcome_var <- attr(my_data_table, "outcome_var")
  cat_no_outcome <- attr(my_data_table, "cat_no_outcome")


    ui <- miniPage(
      gadgetTitleBar(dataset_name,
                     left = NULL, right=NULL),
      miniTabstripPanel(
        miniTabPanel("Single Variable",
          miniContentPanel(
            cat_single_ui("mini_single",
                           categoricalVars)
          )
        ),
        miniTabPanel("Outcome",
          miniContentPanel(
            cat_outcome_ui("mini_outcome", outcome_var, cat_no_outcome)
          )
        ),
        miniTabPanel("Tabular",
          miniContentPanel(
            cat_tabular_ui("mini_tab",categoricalVars)
          ))
      )
    )

    server <- function(input, output, session){
      dataOut <- reactive({my_data_table})

      cat_single_server(id="mini_single",dataOut,dataset_name)
      cat_outcome_server(id="mini_outcome", dataOut)
      cat_tabular_server(id="mini_tab", dataOut, dataset_name)

      observeEvent(input$done, {
        stopApp(TRUE)
      })

    }

    shinyApp(ui, server)

}

