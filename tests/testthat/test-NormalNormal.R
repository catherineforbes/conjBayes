test_that("BetaBernoulli is working", {
 post <- NormalNormal(y = numeric(), mu0 = 2, tau0 = 1, sigma = 2)
 out <- list(mu1=2, tau1=1)
  expect_equal(post, out)
})
