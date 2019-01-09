#' Title
#'
#' @param dataset - a data frame
#' @param covariates - a character vector of column names you want to include
#' @param outcome_var - a categorical variable that describes an outcome to examine
#'
#' @return
#' @export
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
#' @import dplyr
#'
#'
#' @examples
#' library(ggplot)
#' data(diamonds)
#' burro::explore_data(diamonds, outcome_var="Cut")
#'
explore_data <- function(dataset, covariates=NULL, outcome_var) {

  # need to check column names and outcome var names in data
  if(!outcome_var %in% colnames(dataset)){
    stop("Your outcome variable is not the dataset - try using colnames(data) to select it")
  }

  if(!is.null(covariates)) {

    covariates_in_data <- covariates %in% colnames(dataset)
    num_in_data <- which(covariates_in_data)
    not_in_data <- which(!covariates_in_data)

    if(length(num_in_data) == 0) {
      stop("Your covariates aren't in the dataset - make sure they correpond to column names in the data")
    }

    if(length(not_in_data) > 0) {
      warning(
        paste0("The following covariates weren't in the dataset:", paste(covariates[not_in_data]))
      )
    }


  }


  myDataFrame <- data.table(dataset)

  if(!is.null(covariates)){
    myDataFrame <- myDataFrame[,covariates,with=FALSE]
  }

  myDataFrame <- burro:::sanitize_data_frame(myDataFrame, outcome_var)

  remove_categories <- outcome_var
  categoricalVars <- sort(names(burro:::get_category_variables(myDataFrame)))
  cat_no_outcome <- setdiff(categoricalVars, remove_categories)

  numericVars <- sort(burro:::get_numeric_variables(myDataFrame))

  ggplot2::theme_set(ggplot2::theme_classic(base_size = 15))
  #data_dictionary <- readr::read_csv("data/data_dictionary.csv") %>%
  #  dplyr::filter(VariableName %in% covariates)

  ui <- dashboardPage(
    header= dashboardHeader(
      title = "Data Explorer"
    ),
    sidebar=dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", selected=TRUE),
        menuItem("Categorical", tabName = "categorical"),
        menuItem("Continuous", tabName = "continuous"))
    ),
    body=  dashboardBody(
      tabItems(
        tabItem("overview",
                tabBox(width = 12,
                       tabPanel("Visual Summary of Data",
                                tags$head(tags$style("#TxtOut {white-space: nowrap;}")),
                                fluidRow(column(12, offset=0, plotOutput("visdat")))
                       ),
                       tabPanel("Tabular Summary of Data",
                                tags$head(tags$style("#TxtOut {white-space: nowrap;}")),
                                fluidRow(column(12, offset=0, verbatimTextOutput("summaryTable")
                                )
                                )),#,

                       # tabPanel("Missing Clusters", tags$head(tags$style("#TxtOut {white-space: nowrap;}")),
                       #                  fluidRow(column(12, offset=0, plotOutput("missing_clusters"))
                       #                                  )
                       #
                       #          )

                       tabPanel("Data Dictionary",
                                tags$head(tags$style("#TxtOut {white-space: nowrap;}")),
                                fluidRow(dataTableOutput("data_dictionary"))
                       )

                )
        ),
        tabItem("categorical",
                tabBox(width=12,
                       tabPanel("Single Category",
                                selectInput(inputId = "singleVar",
                                            "Select Categorical Variable",
                                            choices = categoricalVars,
                                            selected =categoricalVars[1]),
                                plotOutput("singleTab")
                       ),

                       tabPanel("Category/Outcome",
                                selectInput(inputId = "condTab", "Select Variable to Calculate Proportions",
                                            choices=cat_no_outcome, selected=cat_no_outcome[1]),
                                plotOutput("proportionBarplot")

                       ),
                       tabPanel("Crosstab Explorer",
                                selectInput(inputId = "crossTab1", "Select Crosstab Variable (x)",
                                            choices=categoricalVars, selected=categoricalVars[1]),
                                selectInput(inputId = "crossTab2", "Select Crosstab Variable (y)",
                                            choices=categoricalVars, selected=categoricalVars[1]),
                                verbatimTextOutput("crossTab")
                       ),

                       tabPanel("Missing Data Explorer",
                                selectInput(inputId = "missingVar", "Select Variable to Examine",
                                            choices=categoricalVars, selected = categoricalVars[1]),
                                plotOutput("missingTab")
                       )
                )),
        tabItem("continuous",
                tabBox(width=12,
                       tabPanel("Histogram Explorer",
                                fluidRow(column(width = 4,
                                                selectInput(inputId = "numericVarHist",
                                                            "Select Numeric Variable",
                                                            choices = numericVars, selected=numericVars[1])),
                                         column(width=4, sliderInput("bins", "Number of bins:", min = 1, max = 50,value = 30))),
                                plotOutput("distPlot")
                       ),
                       tabPanel("Boxplot Explorer",
                                fluidRow(column(width = 4, selectInput(inputId = "numericVarBox", "Select Numeric Variable",
                                                                       choices = numericVars, selected=numericVars[1])),
                                         column(width=4,selectInput(inputId = "catVarBox", "Select Category to Condition on",
                                                                    choices = categoricalVars, selected=categoricalVars[1]))),
                                plotOutput("boxPlot")
                       ),
                       tabPanel("Correlation Explorer",
                                fluidRow(
                                  column(width=4, selectInput("x_var", "Select Y Variable",
                                                              choices=numericVars, selected = numericVars[1])),
                                  column(width=4, selectInput("y_var", "Select Y Variable",
                                                              choices=numericVars, selected = numericVars[2]))
                                ),
                                fluidRow(plotOutput("corr_plot"))
                       ))
        ))
    )
  )


  server <- function(input, output, session) {

    dataOut <- reactive({
      #req(input$cohort)
      myDataFrame #%>% filter_(cohortList[[input$cohort]])

    })

    output$singleTab <- renderPlot({

      #dataOut()[,c(input$singleVar)] %>%
      dataOut() %>%
        #data.frame() %>%
        mutate(gr = 1) %>%
        ggplot(aes_string(x=input$singleVar, fill=input$singleVar)) +
        geom_bar(aes(y = ..count..), color="black") +
        geom_text(aes(group=gr, label = scales::percent(..prop..),
                      y= ..count.. ), stat= "count", vjust=-0.5) +
        theme(axis.text.x=element_text(angle=90))
    })

    # output$missing_clusters <- renderPlot({
    #   visdat::vis_miss(data.frame(dataOut()), cluster = TRUE) +
    #     theme(axis.text.x = element_text(size = 15, angle = 90))
    # })

    output$visdat <- renderPlot({
      head(dataOut())

      visdat::vis_dat(data.frame(dataOut())) +
        theme(axis.text.x = element_text(size = 15, angle = 45))
    })

    output$summaryTable <- renderPrint({
      skimr::skim(dataOut())
    })

    output$missingTab <- renderPlot({

      var <- sym(input$missingVar)

      dataOut() %>%
        data.frame() %>%
        naniar::gg_miss_fct(fct = !!var) +
        theme(axis.text = element_text(size = 15))

    })

    output$crossTab <- renderPrint({

      out <- dataOut()[,c(input$crossTab1, input$crossTab2), with=FALSE]
      tab <- table(out, useNA = "ifany")
      tab
    })

    proportionTable <- reactive({

      out <- dataOut()[,c(input$condTab, outcome_var), with=FALSE]
      out
    })

    output$proportionTab <- renderPrint({
      tab <- table(proportionTable(), useNA="ifany")
      return(tab[,"Yes"]/(tab[,"No"] + tab[,"Yes"]))

    })

    output$proportionBarplot <- renderPlot({

      print(input$condTab)

      percent_table <- proportionTable() %>% data.frame() %>% group_by(!!sym(input$condTab)) %>%
        count(!!sym(outcome_var)) %>% mutate(ratio=scales::percent(n/sum(n)))

      proportionTable() %>%
        ggplot(aes_string(x=input$condTab, fill=outcome_var)) +
        geom_bar(position="fill", color="black") + theme(text=element_text(size=20), axis.text.x = element_text(angle = 90)) +
        geom_text(data = percent_table, mapping = aes(y=n, label=ratio),
                  position=position_fill(vjust=0.5))

      # group= !!sym(input$condTab)
    })

    output$distPlot <- renderPlot({

      outPlot <- ggplot(dataOut(), aes_string(x=input$numericVarHist)) +
        geom_histogram(bins=input$bins) + theme(text=element_text(size=20),
                                                axis.text.x = element_text(angle=90))
      outPlot
    })

    output$boxPlot <- renderPlot({
      outPlot <- ggplot(dataOut(), aes_string(x=input$catVarBox,
                                              y=input$numericVarBox, fill=input$catVarBox)) +
        geom_boxplot() + theme(text=element_text(size=20), axis.text.x =
                                 element_text(angle=90))
      outPlot
    })

#    output$data_dictionary <- renderDataTable(
#      datatable(data_dictionary, options=list(pageLength=20))
#    )

    output$corr_plot <- renderPlot({

      mini_frame <- dataOut() %>% data.frame() %>% select(!!sym(input$x_var), !!sym(input$y_var)) %>%
        tidyr::drop_na()
      xcol <- mini_frame %>% pull(!!sym(input$x_var))
      ycol <- mini_frame %>% pull(!!sym(input$y_var))

      corval <- signif(cor(xcol, ycol), digits = 3)

      ggplot(dataOut(), aes_string(x=input$x_var, y=input$y_var)) +
        naniar::geom_miss_point() + stat_smooth(method=lm, se=FALSE) +
        ggtitle(paste(input$x_var, "vs.", input$y_var, "correlation =", corval))
    })

  }

  shinyApp(ui = ui, server = server)

}


sanitize_data_frame <- function(dataset, outcome_var){

  myDataFrame <- dataset

  remove_categories <- outcome_var
  categoricalVars <- sort(names(burro:::get_category_variables(myDataFrame)))
  cat_no_outcome <- setdiff(categoricalVars, remove_categories)

  numericVars <- sort(burro:::get_numeric_variables(myDataFrame))
  #numericVars <- setdiff(numericVars, remove_numeric)

  myDataFrame <- myDataFrame[,c(numericVars, categoricalVars), with=FALSE]

  myDataFrame

}
