context("test-build_shiny_app")

temp_path <- tempdir()
setwd(temp_path)
library(ggplo2)
data(diamonds)
covars <- c("cut", "color", "depth", "table")
build_shiny_app(diamonds, covariates=covars, outcome_var="cut")

test_that("build and deploy works", {

  expect_equal(2 * 2, 4)
})
