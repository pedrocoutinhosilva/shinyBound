test_that("Test createDependency", {
  expect_identical(
    as.character(shinyBound:::createDependency("JScontent")),
    '<script>JScontent</script>'
  )
  expect_identical(
    as.character(htmltools::doRenderTags(shinyBound:::createDependency("JScontent", "global"))),
    '<head>\n  <script>JScontent</script>\n</head>'
  )
  expect_identical(
    as.character(htmltools::doRenderTags(shinyBound:::createDependency("JScontent", "global", "module"))),
    '<head>\n  <script type=\"module\">JScontent</script>\n</head>'
  )
  expect_identical(
    as.character(shinyBound:::createDependency("CSScontent", "local", "style")),
    '<style>CSScontent</style>'
  )
  expect_identical(
    as.character(htmltools::doRenderTags(shinyBound:::createDependency("CSScontent", "global", "style"))),
    '<head>\n  <style>CSScontent</style>\n</head>'
  )
})
test_that("Test shinyBindings", {
  expect_snapshot(
    htmltools::doRenderTags(shinyBound:::shinyBindings(
      system.file("templates/shiny-bindings.js", package = "shinyBound"),
      "testClass",
      "testTag"
    ))
  )
})
test_that("Test webComponentBindings", {
  expect_snapshot(
    shinyBound:::webComponentBindings(
      system.file("templates/webcomponent-stateful.js", package = "shinyBound"),
      "testClass",
      "testTag",
      "<div></div>",
      NULL
    )
  )
})
test_that("Test scaffoldWC", {
  expect_snapshot(
    htmltools::doRenderTags(shinyBound:::scaffoldWC(
      "testId",
      "<div></div>",
      "testClass",
      "testTag",
      NULL
    ))
  )
  expect_snapshot(
    htmltools::doRenderTags(shinyBound:::scaffoldWC(
      "testId",
      "<div><slot name='testSlot'></slot></div>",
      "testClass",
      "testTag",
      NULL
    ))
  )
  expect_snapshot(
    htmltools::doRenderTags(shinyBound:::scaffoldWC(
      "testId",
      "<script>JS_content</script><div></div>",
      "testClass",
      "testTag",
      NULL
    ))
  )
  expect_snapshot(
    htmltools::doRenderTags(shinyBound:::scaffoldWC(
      "testId",
      "<script src='test.js'><div></div>",
      "testClass",
      "testTag",
      NULL
    ))
  )
  expect_snapshot(
    htmltools::doRenderTags(shinyBound:::scaffoldWC(
      "testId",
      "<script src='https://www.anatomyofcode.com/shinyBound/deps/jquery-3.6.0/jquery-3.6.0.min.js'><div></div>",
      "testClass",
      "testTag",
      NULL
    ))
  )
  expect_snapshot(
    htmltools::doRenderTags(shinyBound:::scaffoldWC(
      "testId",
      "<script src='https://www.anatomyofcode.com/shinyBound/deps/nope.js'><div></div>",
      "testClass",
      "testTag",
      NULL
    ))
  )
})
