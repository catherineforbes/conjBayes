test_that("GammaExponential is working", {
 post <- GammaExponential(y = c(1.215, 3.915, 2.519, 2.593, 6.924), a0 = 2, b0 = 1.0)
 out <- list(a1=7, b1=18.166)
  expect_equal(post, out)
})
