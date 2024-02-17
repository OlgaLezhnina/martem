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

test_that("load_reference_classes works", {
  result_load = load_reference_classes("R937648")
  string_load = capture.output(print(result_load))
  expected_load = c(
    "$measurement_scale",
    "Generator for class \"measurement_scale_orkg\":",
    "",
    "Class fields:",
    "                                                                  ",
    "Name:           label  template_name template_class     components",
    "Class:      character      character      character           list",
    "",
    "Class Methods: ",
    "     \"initialize\", \"field\", \"trace\", \"getRefClass\", \"initFields\", \"copy\", ",
    "     \"callSuper\", \".objectPackage\", \"export\", \"untrace\", \"getClass\", \"show\", ",
    "     \"usingMethods\", \".objectParent\", \"import\"",
    "",
    "Reference Superclasses: ",
    "     \"envRefClass\"",
    "",
    "")
  expect_equal(string_load, expected_load)
})
