test_that("differ_length works", {
  expect_equal(differ_length(4, sqrt), 2)
  expect_equal(differ_length(list(4, 9), sqrt), list(2, 3))
})

test_that("turn_json_orkg works", {
  result_load = load_reference_classes("R937648")
  my_instance <- result_load$measurement_scale(label = "my_scale")
  json_result <- turn_json_orkg(my_instance)
  json_string = capture.output(print(json_result))
  expected_json = c(
    "{",
    "  \"@id\": \"_:n1\",",
    "  \"label\": \"my_scale\",",
    "  \"@type\": [",
    "    \"https://incubating.orkg.org/class/C75002\"",
    "  ],",
    "  \"@context\": {",
    "    \"label\": \"http://www.w3.org/2000/01/rdf-schema#label\",",
    "    \"number\": \"https://incubating.orkg.org/property/CSVW_Number\",",
    "    \"rows\": \"https://incubating.orkg.org/property/CSVW_Rows\",",
    "    \"cells\": \"https://incubating.orkg.org/property/CSVW_Cells\",",
    "    \"value\": \"https://incubating.orkg.org/property/CSVW_Value\",",
    "    \"column\": \"https://incubating.orkg.org/property/CSVW_Column\",",
    "    \"columns\": \"https://incubating.orkg.org/property/CSVW_Columns\",",
    "    \"titles\": \"https://incubating.orkg.org/property/CSVW_Titles\"",
    "  }",
    "} ")
  expect_equal(json_string, expected_json)
})
