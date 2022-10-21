test_that("BetaBinomial is working", {
 post <- BetaBinomial(y = 4, n = 10, a0 = 0.5, b0 = 0.5)
 out <- list(a1=4.5, b1=6.5)
  expect_equal(post, out)
})
