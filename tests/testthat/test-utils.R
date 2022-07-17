test_that("Test dropNulls", {
  expect_equal(
    shinyBound:::dropNulls(
      list(numeric = 1, string = "value", removed = NULL, "non-named")
    ),
    list(numeric = 1, string = "value", "non-named")
  )
})

test_that("Test validators", {
  mock_session <- shiny::MockShinySession$new()

  expect_false(shinyBound:::is.ShinySession(""))
  expect_false(shinyBound:::is.ShinySession(1))
  expect_false(shinyBound:::is.ShinySession(c(1, 2, 3)))

  expect_true(shinyBound:::is.ShinySession(mock_session))

  expect_error(shinyBound:::validateSessionObject(""))
  expect_invisible(shinyBound:::validateSessionObject(mock_session))
})
