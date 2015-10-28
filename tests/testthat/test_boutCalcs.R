context("Make sure sleep bout calculation is working properly.")

allAwake <- rep(0, 100)
allAsleep <- rep(100, 100)
singleBoutErr <- c(0,0,0,0,0,0,0,0,0,1,1,1,1)
testGood <- c(0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0)
testGood2 <- c(0,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,0)
shortBout <- c(0,1,0)

test_that("calcBouts() properly identifies sleep bouts", {
  # edge cases
  expect_equal(calcBouts(allAwake), integer(0))
  expect_equal(calcBouts(allAsleep), integer(0))
  expect_equal(calcBouts(singleBoutErr), integer(0))
  expect_equal(calcBouts(1 - singleBoutErr), integer(0))
  expect_equal(calcBouts(shortBout), 1)
  
  # actually decent-ish test data
  expect_equal(calcBouts(testGood), 4)
  expect_equal(calcBouts(1 - testGood), integer(0))
  expect_equal(calcBouts(testGood2), c(4, 6))
  expect_equal(calcBouts(1 - testGood2), 5)
})
