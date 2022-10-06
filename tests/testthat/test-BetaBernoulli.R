test_that("BetaBernoulli is working", {
 post <- BetaBernoulli(y = c(1,1,1,0,0), a0 = 0.5, b0 = 0.5)
 out <- list(a1=3.5, b1=2.5)
  expect_equal(post, out)
})
