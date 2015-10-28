context("Check aggregation function behavior.")

library(magrittr)

test_that("Attribute handling functions behave properly", {
  expect_equal(listAttributes(DAM_DD), c("vial_number", "genotype", "sex"))
  expect_equal(listAttribVals(DAM_DD, "genotype") %>% as.character(), 
               c("control A", "control B", "experimental"))
  expect_equal(byAttribute(DAM_DD, "experimental", "genotype") %>% 
                 listAttribVals("genotype") %>% as.character(), 
               "experimental")
  expect_equal(dropAttribute(DAM_DD, c("control A", "control B"), "genotype") %>% 
                 listAttribVals("genotype") %>% as.character(), 
               "experimental")
})

test_that("catExperiments() works", {
  expect_equal(dim(getVals(catExperiments(c(DAM_DD, DAM_DD))@data)), c(1085, 64))
})

test_that("toInterval() is behaving properly", {
  half_day <- toInterval(DAM_DD, 12, "hours", "average")
  expect_equal(dim(getVals(half_day@data)), c(7, 32))
  # cannot go backwards in terms of data depth
  expect_error(toInterval(half_day, 5, "minutes", "sum"))
})

test_that("subsetTime() is behaving properly", {
  testDat <- toInterval(DAM_DD, 1, "hours", "sum") %>% subsetTime(1.5, 1, "days")
  # get the right number of points
  expect_equal(length(testDat@data$read_time), 24)
  expect_equal(as.numeric(difftime(testDat@data$read_time[length(testDat@data$read_time)], 
                        testDat@data$read_time[1])), 23)
})
