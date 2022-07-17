test_that("List components", {
  expect_identical(
    listComponents(),
    getOption("shinyBound.components")
  )
})



test_that("Register and unregister component", {

  expect_true(
    is.null(getOption("shinyBound.components")[["testClass"]])
  )

  # Register component
  registerComponent("testClass", div())

  expect_true(
    !is.null(getOption("shinyBound.components")[["testClass"]])
  )

  # unregister component
  expect_error(unregisterComponent("nonExistingComponent"))

  unregisterComponent("testClass")
  expect_true(
    is.null(getOption("shinyBound.components")[["testClass"]])
  )
})

test_that("Get registered component", {
  expect_error(shinyBound:::getComponent("nonExistingComponent"))

  registerComponent("testClass", div())
  expect_identical(
    shinyBound:::getComponent("testClass"),
    getOption("shinyBound.components")[["testClass"]]
  )
  unregisterComponent("testClass")
})

test_that("Use registered component", {
  expect_error(useComponent("test", "testClass", NULL))

  registerComponent("testClass", div())
  expect_identical(
    as.character(useComponent("test", "testClass", NULL)),
    '<wc-testclasstest id="test" class="shinywc-component wc-testclasstest"></wc-testclasstest>'
  )
  unregisterComponent("testClass")
})

test_that("Make unregistered component", {
  expect_error(component("test", div()))

  expect_identical(
    as.character(component("test", div(), NULL)),
    '<wc-test id="test" class="shinywc-component wc-test"></wc-test>'
  )
})

test_that("Test Update component", {
  expect_error(updateComponent("fake_session", "testId", property = "value"))

  mock_session <- shiny::MockShinySession$new()


  expect_invisible(
    updateComponent(mock_session, "testId", property = "value")
  )
})

test_that("Test component script", {
  expect_error(componentScript("fake_session", "testId", callback = "some_JS"))

  mock_session <- shiny::MockShinySession$new()

  expect_invisible(
    componentScript(mock_session, "testId", callback = "some_JS")
  )
})
