test_that("hostname functions work", {
  expect_equal(
    change_hostname("https://incubating.orkg.org/"),
    "https://incubating.orkg.org/"
  )
  expect_equal(show_hostname(), "https://incubating.orkg.org/")
})
