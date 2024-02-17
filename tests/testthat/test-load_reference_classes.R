test_that("extractor_orkg works", {
  result_extractor = extractor_orkg("R937648")
  string_representation = capture.output(print(result_extractor))
  expected = c("$measurement_scale",
  "$measurement_scale[[1]]",
  "$measurement_scale[[1]][[1]]",
  "      template_name template_class",
  "1 measurement_scale         C75002",
  "",
  "$measurement_scale[[1]][[2]]",
  "[1] predicate_id    predicate_label value_class_id  nested_template",
  "<0 rows> (or 0-length row.names)",
  "",
  "",
  "")
  expect_equal(string_representation, expected)
})
