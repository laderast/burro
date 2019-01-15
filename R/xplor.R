#' Explore a dataaset using a simple app
#'
#' @param dataset - a data frame
#' @param covariates - Optional. a character vector of column names you want
#' to include in the dataset
#' @param outcome_var - a categorical variable that describes an outcome to examine
#' @param data_dictionary - Optional. A data.frame describing covariates. This is passed
#' on to DT:data.table() so it can be easily searched
#'
#'
#' `explore_data` gives you a simple shiny app to explore data, The app is tailored to the
#'  covariate types of the data.frame/data.table that you pass to it.
#'
#'  Each menu item covers a different part of exploratory data analysis with
#'  simple visualizations for discussion.
#'
#'  The tabs are automatically updated with variables depending on the variable types.
#'
#'
#' @return A Shiny App object that can either be run in the console or on an external
#' shiny server such as shinyapps.io. For building the actual app.R, see
#' build_shiny_app
#' @export
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
#' @import dplyr
#' @import skimr
#'
#'
#' @examples
#' library(ggplot2)
#' data(diamonds)
#' if(interactive()){
#'    burro::explore_data(diamonds, outcome_var="cut")
#' }
#'
#'
#'
#' #need another example here
explore_data <- function(dataset, covariates=NULL, outcome_var, data_dictionary = NULL) {
  #needed to show spark histograms
  Sys.setlocale("LC_CTYPE", "Chinese")
  dataset_name <- deparse(substitute(dataset))


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


  myDataFrame <- data.table::data.table(dataset)

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
      title = dataset_name
    ),
    sidebar=dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", selected=TRUE),
        menuItem("Categorical", tabName = "categorical"),
        menuItem("Continuous", tabName = "continuous")),
        shiny::br(),
        tags$div(tags$a(href="http://github.com/laderast/burro", "Built with burro!"))
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
                                #tags$head(tags$style("#TxtOut {white-space: nowrap;}")),
                                fluidRow(dataTableOutput("data_dict"))
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
        #viridis::scale_fill_viridis(discrete=TRUE, option="magma") +
        geom_text(aes(group=gr, label = scales::percent(..prop..),
                      y= ..count.. ), stat= "count", vjust=-0.5) +
        theme(axis.text.x=element_text(angle=90))
    })

    # output$missing_clusters <- renderPlot({
    #   visdat::vis_miss(data.frame(dataOut()), cluster = TRUE) +
    #     theme(axis.text.x = element_text(size = 15, angle = 90))
    # })

    output$visdat <- renderPlot({

      visdat::vis_dat(data.frame(dataOut())) +
        theme(axis.text.x = element_text(size = 15, angle = 45)) #+
        #viridis::scale_color_viridis(discrete=TRUE, option="magma")
    })

    output$summaryTable <- renderPrint({
      skimr::skim(dataOut()) #%>% skimr::kable()
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
                  position=position_fill(vjust=0.5)) #+
        #viridis::scale_fill_viridis(discrete=TRUE, option="magma")

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
                                 element_text(angle=90)) #+
        #viridis::scale_fill_viridis(discrete=TRUE, option="magma")
      outPlot
    })

   output$data_dict <- renderDataTable({
     print(data_dictionary)

     if(is.null(data_dictionary)){
          return(NULL)
     }

      DT::datatable(data_dictionary, options=list(pageLength=20))
    })

    output$corr_plot <- renderPlot({

      mini_frame <- dataOut() %>% data.frame() %>% select(!!sym(input$x_var), !!sym(input$y_var)) %>%
        tidyr::drop_na()
      xcol <- mini_frame %>% pull(!!sym(input$x_var))
      ycol <- mini_frame %>% pull(!!sym(input$y_var))

      corval <- signif(cor(xcol, ycol), digits = 3)

      ggplot(dataOut(), aes_string(x=input$x_var, y=input$y_var)) +
        naniar::geom_miss_point() + stat_smooth(method=lm, se=FALSE) +
        #viridis::scale_color_viridis(discrete = TRUE, option="magma") +
        ggtitle(paste(input$x_var, "vs.", input$y_var, "correlation =", corval))
    })

  }

  if(getOption("app_list")==TRUE){

    return(list(ui=ui, server=server))
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

#' Make a shiny app from a dataset
#'
#' @param dataset - data.frame or data.table of data
#' @param covariates - character vector of covariates, corresponding to the column names of dataset
#' @param outcome_var - character of variable name in dataset correpsonding to the outcome of interest
#'
#' @return shiny app in a folder
#' @export
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @import here
#'
#' @examples
#' #create a new project/folder in RStudio before you run this
#' library(ggplot2)
#' data(diamonds)
#' covars <- colnames(diamonds)
#' if(interactive()){
#' burro::build_shiny_app(dataset=diamonds,
#' covariates=covars, outcome_var="cut")
#' }
build_shiny_app <- function(dataset, covariates, outcome_var) {

  out_covar_string <- c("c(\'{outvar}\')")
  outvar <- glue_collapse(covariates, sep="\',\'")

  covar_string <- glue(out_covar_string)
  folder_name <- deparse(substitute(dataset))

  dir.create(here::here("data"), showWarnings = FALSE)
  saveRDS(dataset, file=here("data", "dataset.rds"))

  out_app_string <- "library(burro)
  library(shiny)
  library(here)
  options(app_list=TRUE)

  {folder_name} <- readRDS(here('data','dataset.rds'))
    outcome_var <- '{outcome_var}'

  #edit your covariates here
  covars <- {covar_string}

  #build the burro app and run it
  app_list <- burro::explore_data({folder_name}, covars, outcome_var)
  ui <- app_list[['ui']]
  server <- app_list[['server']]

  app <- shiny::shinyApp(ui, server)
  app"

  out_string <- glue(out_app_string)

  writeLines(out_string, con=here("app.R"))

  print("Your burro App has been created in your project folder! \n Open the app.R file in RStudio and hit the 'Run App' button to run it")
  }

