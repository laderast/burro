context("test-explore_data")
source("R/helper.R")

library(ggplot2)
library(dplyr)

diamonds <- data.table::data.table(diamonds)
test_tab <- diamonds[,c('cut', 'clarity'), with=FALSE]

percent_plot(test_tab, "cut", "clarity")

test_that("multiplication works", {
})
