test_that("request_orkg works", {
  info = request_orkg("api/templates/R937648")
  obtained_info = info$target_class
  expect_equal(obtained_info, "C75002")
})
