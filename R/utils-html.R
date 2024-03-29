#' Generate a custom HTML tag.
#'
#' @description
#'   Generates a HTML custom tag with a defined ID, content and attributes.
#'   Used in scaffoldWC().
#'
#' @param htmlWCTagName A valid HTML tag name to use in the template.
#' @param inputId The tag ID attribute.
#' @param ... Attributes and content of the web component HTML wrapper tag.
#'
#' @importFrom magrittr "%>%"
#' @importFrom htmltools tag
#'
#' @keywords html internal
#' @return A HTML tagList.
htmlComponentTag <- function(htmlWCTagName, inputId, ...) {
  htmlWCTagName %>% tag(list(
    id = inputId,
    class = paste("shinywc-component", htmlWCTagName),
    ...
  ))
}

#' Generate a valid HTML tag name.
#'
#' @description
#'   Converts a string into a valid format that can be used as a HTML tag name.
#'
#' @param string The string to parse into a valid HTML tag name.
#'
#' @importFrom magrittr "%>%"
#'
#' @keywords html internal
#' @return A valid HTML tag name.
htmlWCTagName <- function(string) {
  if (!nzchar(string)) {
    stop("Error: String arg must be a character string")
  }


  string %>%
    tolower() %>%
    paste(sep = "-") %>%
    gsub(" ", "-", .) %>%
    paste0("wc-", .)
}

#' Generate a valid HTML class name.
#'
#' @description
#'   Converts a string into a valid format that can be used as a class name.
#'
#' @param string The string to parse into a valid class name.
#'
#' @importFrom magrittr "%>%"
#'
#' @keywords html internal
#' @return A valid HTML class name (letters only, no spaces).
htmlClassName <- function(string) {
  if (!nzchar(string)) {
    stop("Error: String arg must be a character string")
  }

  partials <- string %>%
    strsplit("[- ]+") %>%
    .[[1]]

  partials %>%
    substring(1, 1) %>%
    toupper() %>%
    paste(substring(partials, 2), sep = "", collapse = "")
}

#' Get namespace id placeholder.
#'
#' @description
#'   Generates a namespaced id placeholder to allow traditional shiny widgets
#'   to be used as part of web component templates. Allows those widgets
#'   to be accessed in the shiny server function via input${componentID}_{id}.
#'
#' @param id The inputId of the shiny widget.
#'
#' @keywords utils namespaces internal
#' @return A namespaced Id
cns <- function(id) {
  paste0(paste0("webComponentIdPlaceholder_", id))
}

#' HTML slot tag.
#'
#' @description
#'   A slot HTML tag with a given slot name. Can be used to define template
#'   areas where content will be added in the future.
#'
#' @param name The name of the slot that can recieve future content.
#'
#' @importFrom shiny tags
#'
#' @keywords utils namespaces internal
#' @return a HTML tag that can be added to the page.
slot <- function(name) {
  tags$slot(name = name)
}

#' Slot into component slot.
#'
#' @description
#'   Wraps a given HTML tag into a valid HTML element what can be slotIn into a
#'   specific slot in a web component.
#'
#' @param name The web component slot where the element should be added.
#' @param ... The content and attributes to be slotIn into the web component.
#'
#' @importFrom shiny tags
#' @importFrom shiny div
#'
#' @keywords utils namespaces internal
#' @return a HTML tag that can be added to the page.
slotIn <- function(name, ...) {
  div(slot = name, ...)
}

#' Add shinyBound binds to tag.
#'
#' @description
#'   Links diferent bindings to the given tag. For all list arguments, instead
#'   of a name = value pair, a simple string can be used instead if name and
#'   value are the same.
#'   Both long and short versions of the attributes can be used.
#'
#' @param tag html fragment to add attributes to.
#' @param fromShinyProperty Named list where the name is the JS property to
#'   update and the value the corresponding named argument used in the
#'   updateWebComponent or as part of the initial component state.
#' @param fsProperty Short hand version of the previous argument.
#' @param fromShinyStyle Named list where the name is the css property to
#'   update and the value the corresponding named argument used in the
#'   updateWebComponent or as part of the initial component state.
#' @param fsStyle Short hand version of the previous argument.
#' @param fromShinyAttribute Named list where the name is the css property to
#'   update and the value the corresponding named argument used in the
#'   updateWebComponent or as part of the initial component state.
#' @param fsAttribute Short hand version of the previous argument.
#' @param fromShinyClass Named list where the name is the class to
#'   update and the value the corresponding named argument used in the
#'   updateWebComponent or as part of the initial component state.
#' @param fsClass Short hand version of the previous argument.
#' @param toShinyProperty Named list where the name is the JS property to
#'   get and the value the corresponding named attribute that can be read
#'   using input$inputId or directly via input$inputId_attribute.
#' @param tsProperty Short hand version of the previous argument.
#' @param toShinyStyle Named list where the name is the css property to
#'   get and the value the corresponding attribute that can be read
#'   using input$inputId or directly via input$inputId_attribute.
#' @param tsStyle Short hand version of the previous argument.
#' @param toShinyAttribute Named list where the name is the HTML attribute to
#'   get and the value the corresponding attribute that can be read
#'   using input$inputId or directly via input$inputId_attribute.
#' @param tsAttribute Short hand version of the previous argument.
#' @param toShinyClass Named list where the name is the class to
#'   get and the value the corresponding named attribute that can be read
#'   using input$inputId or directly via input$inputId_attribute.
#' @param tsClass Short hand version of the previous argument.
#' @param toShinyEvent Named list where the name is the HTML event to
#'   trigger an update and the value the corresponding named attribute that can
#'   be read using input$inputId or directly via input$inputId_attribute.
#' @param tsEvent Short hand version of the previous argument.
#'
#' @importFrom shiny tagAppendAttributes
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom shiny tags
#' @importFrom shiny div
#'
#' @keywords html template utils
#' @return a HTML tag that can be added to the page.
#' @export
tagAppendBinds <- function(tag,
                           fromShinyProperty = NULL,
                           fsProperty = NULL,
                           fromShinyStyle = NULL,
                           fsStyle = NULL,
                           fromShinyAttribute = NULL,
                           fsAttribute = NULL,
                           fromShinyClass = NULL,
                           fsClass = NULL,
                           toShinyProperty = NULL,
                           tsProperty = NULL,
                           toShinyStyle = NULL,
                           tsStyle = NULL,
                           toShinyAttribute = NULL,
                           tsAttribute = NULL,
                           toShinyClass = NULL,
                           tsClass = NULL,
                           toShinyEvent = NULL,
                           tsEvent = NULL) {
  if (!identical(names(tag), names(div()))) {
    stop("tag argument must be a valid HTML tag")
  }

  arguments <- environment() %>%
    as.list()
  arguments$tag <- NULL

  for (type in c("Property", "Style", "Attribute", "Class", "Event")) {
    arguments[paste0("fromShiny", type)] <- ifelse(
      is.null(arguments[[paste0("fromShiny", type)]]),
      arguments[paste0("fs", type)],
      arguments[paste0("fromShiny", type)]
    )
    arguments[paste0("toShiny", type)] <- ifelse(
      is.null(arguments[[paste0("toShiny", type)]]),
      arguments[paste0("ts", type)],
      arguments[paste0("toShiny", type)]
    )

    arguments[paste0("fs", type)] <- NULL
    arguments[paste0("ts", type)] <- NULL
  }

  arguments %<>%
    dropNulls()

  attributes <- list()
  for (name in names(arguments)) {
    parsed_name <- name %>%
      gsub("([[:upper:]])", "-\\1", .) %>%
      tolower() %>%
      paste0("data-", .)

    if (is.character(arguments[[name]])) {
      attributes[[parsed_name]] <- arguments[[name]]
    } else {
      attributes[[parsed_name]] <- seq_len(length(arguments[[name]])) %>%
        lapply(. %>%
          {
            if (identical(names(arguments[[name]][.])[[1]], "") ||
              is.null(names(arguments[[name]][.])[[1]])) {
              return(arguments[[name]][[.]])
            }
            paste(
              names(arguments[[name]][.])[[1]],
              arguments[[name]][[.]],
              sep = ":"
            )
          }) %>%
        paste(collapse = "|")
    }
  }

  tagAppendAttributes %>%
    do.call(modifyList(list(tag), attributes))
}

#' Converts bind argument names to html data attributes.
#'
#' @importFrom stringr str_replace_all
#' @importFrom shiny HTML
#'
#' @return A HTML tagList.
#' @keywords html internal
replacePlaceholders <- function(html, id) {
  suppressWarnings({
    str_replace_all(html, c(
      webComponentIdPlaceholder = id,
      fsProperty = "data-from-shiny-property",
      fsStyle = "data-from-shiny-style",
      fsAttribute = "data-from-shiny-attribute",
      fsClass = "data-from-shiny-class",
      tsProperty = "data-to-shiny-property",
      tsStyle = "data-to-shiny-style",
      tsAttribute = "data-to-shiny-attribute",
      tsClass = "data-to-shiny-class",
      tsEvent = "data-to-shiny-event",
      fsRepeater = "data-from-shiny-repeater"
    )) %>% HTML()
  })
}

#' Searches a HTML fragment for dependencies.
#'
#' @importFrom htmltools findDependencies
#'
#' @return A HTML tagList.
#' @keywords html internal
attachStateDependencies <- function(...) {
  findDependencies(list(...))
}
