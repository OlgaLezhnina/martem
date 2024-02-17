test_that("change_hostname works", {
  expect_equal(
    change_hostname("https://incubating.orkg.org/"),
    "https://incubating.orkg.org/"
  )
})
