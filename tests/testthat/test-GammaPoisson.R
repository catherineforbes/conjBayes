test_that("GammaPoisson is working", {
  post <- GammaPoisson(y = c(1, 3, 5, 6, 5), a0 = 2.0, b0 = 1.0)
  out <- list(a1=22, b1=6)
  expect_equal(post, out)
})


