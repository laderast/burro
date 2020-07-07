context("test-get_categorical_data")
library(ggplot2)
library(shiny)
library(fivethirtyeight)
library(testthat)
data(diamonds)
my_data_table <- burro::check_data(diamonds[1:100,])

numericVars <- attr(my_data_table, "numericVars")
categoricalVars <- attr(my_data_table, "categoricalVars")
outcome_var <- attr(my_data_table, "outcome_var")
cat_no_outcome <- attr(my_data_table, "cat_no_outcome")


testModule(burro:::cat_single_server, {
  session$setInputs(singleVar = "cut")
  print(str(output$singleTab))

  session$setInputs(singleVar = "blah")
  expect_null(output$singleTab)},

  dataOut=reactive({diamonds}))

testModule(burro:::cat_single_ui, {
  expect_equal(categoricalVars[1], input$singleVar)
}, categoricalVars=categoricalVars)
