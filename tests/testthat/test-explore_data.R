context("test-explore_data")
source("R/helper.R")

library(ggplot2)
library(dplyr)
library(rlang)

diamonds <- data.table::data.table(diamonds)
test_tab <- diamonds[,c('cut', 'clarity'), with=FALSE]

test_plot <- percent_plot(test_tab, "cut", "clarity")

test_that("percent_plot tests", {
  expect_is(test_plot, "ggplot")
  expect_null(percent_plot(test_tab, "cut", "cut"))
})

#TODO: quote() ggplot expressions to print
#How do I evaluate the expression with tidyeval?
test_var <- sym("cut")

test <- quote(ggplot(diamonds, aes(x=!!test_var, y=carat)))
deparse(test)
