test_that("Test htmlComponentTag builder", {
  expect_error(shinyBound:::htmlComponentTag())
  expect_error(shinyBound:::htmlComponentTag("tag"))

  expect_equal(
    as.character(shinyBound:::htmlComponentTag("tag", "input")),
    '<tag id="input" class="shinywc-component tag"></tag>'
  )

  expect_equal(
    as.character(
      shinyBound:::htmlComponentTag("tag", "input", attribute = "value")
    ),
    '<tag id="input" class="shinywc-component tag" attribute="value"></tag>'
  )
})

test_that("Test htmlWCTagName builder", {
  expect_error(shinyBound:::htmlWCTagName())
  expect_error(shinyBound:::htmlWCTagName(""))
  expect_equal(shinyBound:::htmlWCTagName("tag"), "wc-tag")
  expect_equal(shinyBound:::htmlWCTagName("tagName"), "wc-tagname")
  expect_equal(shinyBound:::htmlWCTagName("tag Name"), "wc-tag-name")
})

test_that("Test htmlClassName builder", {
  expect_error(shinyBound:::htmlClassName())
  expect_error(shinyBound:::htmlClassName(""))
  expect_equal(shinyBound:::htmlClassName("tag"), "Tag")
  expect_equal(shinyBound:::htmlClassName("tagName"), "TagName")
  expect_equal(shinyBound:::htmlClassName("tag Name"), "TagName")
})

test_that("Test cns placeholders", {
  expect_error(shinyBound::cns())
  expect_equal(shinyBound::cns("test"), "webComponentIdPlaceholder_test")
})

test_that("Test slot tag", {
  expect_error(shinyBound::slot())
  expect_equal(
    as.character(shinyBound::slot("test")),
    "<slot name=\"test\"></slot>"
  )
})

test_that("Test slotIn wrapper", {
  expect_error(shinyBound::slotIn())
  expect_equal(
    as.character(shinyBound::slotIn("test", "content")),
    '<div slot="test">content</div>'
  )
})

test_that("Test tagAppendBinds", {
  expect_error(shinyBound::tagAppendBinds())
  expect_error(shinyBound::tagAppendBinds("not-html"))
  expect_equal(
    as.character(shinyBound::tagAppendBinds(shiny::div())),
    '<div></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      fromShinyProperty = "test"
    )),
    '<div data-from-shiny-property="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      fsProperty = "test"
    )),
    '<div data-from-shiny-property="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      fromShinyStyle = "test"
    )),
    '<div data-from-shiny-style="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      fsStyle = "test"
    )),
    '<div data-from-shiny-style="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      fromShinyAttribute = "test"
    )),
    '<div data-from-shiny-attribute="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      fsAttribute = "test"
    )),
    '<div data-from-shiny-attribute="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      fromShinyClass = "test"
    )),
    '<div data-from-shiny-class="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      fsClass = "test"
    )),
    '<div data-from-shiny-class="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      toShinyProperty = "test"
    )),
    '<div data-to-shiny-property="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      tsProperty = "test"
    )),
    '<div data-to-shiny-property="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      toShinyStyle = "test"
    )),
    '<div data-to-shiny-style="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      tsStyle = "test"
    )),
    '<div data-to-shiny-style="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      toShinyAttribute = "test"
    )),
    '<div data-to-shiny-attribute="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      tsAttribute = "test"
    )),
    '<div data-to-shiny-attribute="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      toShinyClass = "test"
    )),
    '<div data-to-shiny-class="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      tsClass = "test"
    )),
    '<div data-to-shiny-class="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      tsEvent = "test"
    )),
    '<div data-to-shiny-event="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      toShinyEvent = "test"
    )),
    '<div data-to-shiny-event="test"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      tsClass = list(
        foo = "value",
        bar = "value"
      )
    )),
    '<div data-to-shiny-class="foo:value|bar:value"></div>'
  )
  expect_equal(
    as.character(shinyBound::tagAppendBinds(
      shiny::div(),
      tsClass = list(
        "foo",
        bar = "value"
      )
    )),
    '<div data-to-shiny-class="foo|bar:value"></div>'
  )
})

test_that("Test replacePlaceholders", {
  expect_error(shinyBound:::replacePlaceholders())
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(id = "webComponentIdPlaceholder"),
      "id"
    )),
    '<div id="id"></div>'
  )
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(fsProperty = "test"),
      "id"
    )),
    '<div data-from-shiny-property="test"></div>'
  )
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(fsStyle = "test"),
      "id"
    )),
    '<div data-from-shiny-style="test"></div>'
  )
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(fsAttribute = "test"),
      "id"
    )),
    '<div data-from-shiny-attribute="test"></div>'
  )
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(fsClass = "test"),
      "id"
    )),
    '<div data-from-shiny-class="test"></div>'
  )
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(tsProperty = "test"),
      "id"
    )),
    '<div data-to-shiny-property="test"></div>'
  )
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(tsStyle = "test"),
      "id"
    )),
    '<div data-to-shiny-style="test"></div>'
  )
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(tsAttribute = "test"),
      "id"
    )),
    '<div data-to-shiny-attribute="test"></div>'
  )
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(tsClass = "test"),
      "id"
    )),
    '<div data-to-shiny-class="test"></div>'
  )
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(tsEvent = "test"),
      "id"
    )),
    '<div data-to-shiny-event="test"></div>'
  )
  expect_equal(
    as.character(shinyBound:::replacePlaceholders(
      shiny::div(
        fsProperty = "test",
        fsStyle = "test",
        fsAttribute = "test",
        fsClass = "test",
        tsProperty = "test",
        tsStyle = "test",
        tsAttribute = "test",
        tsClass = "test",
      ),
      "id"
    )),
    '<div data-from-shiny-property="test" data-from-shiny-style="test" data-from-shiny-attribute="test" data-from-shiny-class="test" data-to-shiny-property="test" data-to-shiny-style="test" data-to-shiny-attribute="test" data-to-shiny-class="test"></div>'
  )
})
