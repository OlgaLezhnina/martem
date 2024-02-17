test_that("differ_length works", {
  expect_equal(differ_length(4, sqrt), 2)
  expect_equal(differ_length(list(4, 9), sqrt), list(2, 3))
})
