context("Test example workflows using the example dataset.") 

library(magrittr)
test_that("Raw activity calculations work.", {
  DAM_DD %>% dropDead() %>% toInterval(1, "hours", "sum") %>% linePlot("genotype")
  success <- TRUE
  expect_that(success, is_true())
})

test_that("Raw sleep calculations work.", {
  DAM_DD %>% dropDead() %>% calcSleep() %>% 
    toInterval(1, "hours", "average") %>% linePlot("genotype")
  success <- TRUE
  expect_that(success, is_true())
})

test_that("Sleep bout calculations work.", {
  sleep <- DAM_DD %>% dropDead() %>% calcSleep
  calcMeanBout(sleep)
  plotData <- calcNumBouts(sleep)
  barPlot(sleep, "genotype", vector = plotData)
  success <- TRUE
  expect_that(success, is_true())
})

test_that("Activity index calculations work.", {
  DAM_DD %>% dropDead() %>% calcActivityIndex()
  success <- TRUE
  expect_that(success, is_true())
})


