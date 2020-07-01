context("test-get_categorical_data")
source("R/helper.R")
library(ggplot2)
library(fivethirtyeight)
data(diamonds)

cats1 <- get_category_variables(iris)
cats2 <- get_category_variables(diamonds)
cats3 <- get_category_variables(candy_rankings)
cats4 <- get_category_variables(mtcars)

test_that("get category variables works", {
  expect_equal(length(cats1), 1)
  expect_equal(length(cats2), 3)
  expect_equal(names(cats1), "Species")
  expect_equal(NULL, cats4)
})

data1 <- check_data(mtcars)
data2 <- check_data(diamonds)

test_that("check_data works",{

})
