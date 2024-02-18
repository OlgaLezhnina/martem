test_that("differ_length works", {
  expect_equal(differ_length(4, sqrt), 2)
  expect_equal(differ_length(list(4, 9), sqrt), list(2, 3))
})

test_that("turn_json works", {
  result_load <- load_reference_classes("R937648")
  my_instance <- result_load$measurement_scale(label = "my_scale")
  json_result <- turn_json(my_instance)
  json_string <- capture.output(print(json_result))
  expected_json <- c(
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

test_that("differ_type works", {
  the$uid <- generate_uid()
  my_df = data.frame(c(1,2), c(3,4))
  colnames(my_df) = c("A", "B")
  differ_df = differ_type(my_df)
  df_string = capture.output(print(differ_df))
  expected_df = c(
    "$`@type`",                                         "$`@type`[[1]]",
    "[1] \"https://incubating.orkg.org/class/Table\"",  "",
    "",                                                 "$label",
    "[1] \"Table\"",                                    "",
    "$columns",                                         "$columns[[1]]",
    "$columns[[1]]$`@type`",                            "$columns[[1]]$`@type`[[1]]",
    "[1] \"https://incubating.orkg.org/class/Column\"", "",
    "",                                                 "$columns[[1]]$titles",
    "[1] \"A\"",                                        "",
    "$columns[[1]]$number",                             "[1] 1",
    "",                                                 "$columns[[1]]$`@id`",
    "[1] \"_:n1\"",                                     "",
    "",                                                 "$columns[[2]]",
    "$columns[[2]]$`@type`",                            "$columns[[2]]$`@type`[[1]]",
    "[1] \"https://incubating.orkg.org/class/Column\"", "",
    "",                                                 "$columns[[2]]$titles",
    "[1] \"B\"",                                        "",
    "$columns[[2]]$number",                             "[1] 2",
    "",                                                 "$columns[[2]]$`@id`",
    "[1] \"_:n2\"",                                     "",
    "",                                                 "",
    "$rows",                                            "$rows[[1]]",
    "$rows[[1]]$`@type`",                               "$rows[[1]]$`@type`[[1]]",
    "[1] \"https://incubating.orkg.org/class/Row\"",    "",
    "",                                                 "$rows[[1]]$number",
    "[1] 1",                                            "",
    "$rows[[1]]$titles",                                "[1] \"1\"",
    "",                                                 "$rows[[1]]$cells",
    "$rows[[1]]$cells[[1]]",                            "$rows[[1]]$cells[[1]]$`@type`",
    "$rows[[1]]$cells[[1]]$`@type`[[1]]",               "[1] \"https://incubating.orkg.org/class/Cell\"",
    "",                                                 "",
    "$rows[[1]]$cells[[1]]$value",                      "[1] \"1\"",
    "",                                                 "$rows[[1]]$cells[[1]]$column",
    "[1] \"_:n1\"",                                     "",
    "",                                                 "$rows[[1]]$cells[[2]]",
    "$rows[[1]]$cells[[2]]$`@type`",                    "$rows[[1]]$cells[[2]]$`@type`[[1]]",
    "[1] \"https://incubating.orkg.org/class/Cell\"",   "",
    "",                                                 "$rows[[1]]$cells[[2]]$value",
    "[1] \"3\"",                                        "",
    "$rows[[1]]$cells[[2]]$column",                     "[1] \"_:n2\"",
    "",                                                 "",
    "",                                                 "",
    "$rows[[2]]",                                       "$rows[[2]]$`@type`",
    "$rows[[2]]$`@type`[[1]]",                          "[1] \"https://incubating.orkg.org/class/Row\"",
    "",                                                 "",
    "$rows[[2]]$number",                                "[1] 2",
    "",                                                 "$rows[[2]]$titles",
    "[1] \"2\"",                                        "",
    "$rows[[2]]$cells",                                 "$rows[[2]]$cells[[1]]",
    "$rows[[2]]$cells[[1]]$`@type`",                    "$rows[[2]]$cells[[1]]$`@type`[[1]]",
    "[1] \"https://incubating.orkg.org/class/Cell\"",   "",
    "",                                                 "$rows[[2]]$cells[[1]]$value",
    "[1] \"2\"",                                        "",
    "$rows[[2]]$cells[[1]]$column",                     "[1] \"_:n1\"",
    "",                                                 "",
    "$rows[[2]]$cells[[2]]",                            "$rows[[2]]$cells[[2]]$`@type`",
    "$rows[[2]]$cells[[2]]$`@type`[[1]]",               "[1] \"https://incubating.orkg.org/class/Cell\"",
    "",                                                 "",
    "$rows[[2]]$cells[[2]]$value",                      "[1] \"4\"",
    "",                                                 "$rows[[2]]$cells[[2]]$column",
    "[1] \"_:n2\"",                                     "",
    "",                                                 "",
    "",                                                 "",
    "$`@id`",                                           "[1] \"_:n3\"",
    ""
  )
  expect_equal(df_string, expected_df)
})
