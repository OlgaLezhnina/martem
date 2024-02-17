test_that("format_string works", {
  expect_equal(format_string("a-b c"), "a_b_c")
})
