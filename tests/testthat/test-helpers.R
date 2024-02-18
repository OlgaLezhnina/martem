test_that("format_string works", {
  expect_equal(format_string("a-b c"), "a_b_c")
})

test_that("show_fields works", {
  tp <- load_reference_classes("R937648")
expect_equal(show_fields(tp$measurement_scale), "label")
})
