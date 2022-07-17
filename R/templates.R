#' Generate a dependency tag.
#'
#' @description
#' Generates a dependency HTML tag with a given content, scope and type.
#'
#' @param content The content of the tag
#' @param scope The scope of the tag. Can be local or global. Depending on the
#'   scope the return will either be a inline HTML tag to a header HTML tag.
#' @param type The type of content. Can be either a JS script, a JS module or
#'   simply valid CSS.
#'
#' @return A HTML tagList.
createDependency <- function(content, scope = "local", type = "script") {
  wrappedContent <- switch(
    type,
    "script" = tags$script(content),
    "module" = tags$script(type = "module", content),
    "style" = tags$style(content)
  )

  return(switch(
    scope,
    "global" = tags$head(wrappedContent),
    "local" = wrappedContent,
  ))
}

#' Generate a Shiny Bind.
#'
#' @description
#' Generates a HTML fragment that can be added to a page to initialize a
#' specific set of shiny bindings for a component. Used in scaffoldWC().
#'
#' @param template Path to an HTML template file
#' @param htmlClassName A valid web class name to use in the template
#' @param htmlWCTagName A valid HTML tag name to use in the template
#'
#' @importFrom magrittr "%>%"
#' @importFrom htmltools htmlTemplate
#' @importFrom utils modifyList
#' @importFrom shiny singleton
#'
#' @return A HTML tagList.
shinyBindings <- function(template, htmlClassName, htmlWCTagName) {
  options <- list(
    className = htmlClassName,
    htmlWCTagName = htmlWCTagName
  )

  htmlTemplate %>%
    do.call(modifyList(list(template), options)) %>%
    createDependency(scope = "global") %>%
    singleton()
}

#' Generate a Web component Bind.
#'
#' @description
#' Generates a HTML fragment with the required web component creation code.
#' Used in scaffoldWC().
#'
#' @param template Path to an HTML template file
#' @param htmlClassName A valid web class name to use in the template
#' @param htmlWCTagName A valid HTML tag name to use in the template
#' @param innerHTML A valid HTML fragment to use as the component shadow DOM
#' @param initialState Initial state of the component binds
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom htmltools htmlTemplate
#' @importFrom jsonlite toJSON
#' @importFrom utils modifyList
#' @importFrom shiny singleton
#'
#' @return A HTML tagList.
webComponentBindings <- function(template,
                                 htmlClassName,
                                 htmlWCTagName,
                                 innerHTML,
                                 initialState) {
  initialState %<>%
    dropNulls() %>%
    toJSON(auto_unbox = TRUE)

  options <- list(
    className = htmlClassName,
    tagName = htmlWCTagName,
    innerHTML = innerHTML,
    initialState = initialState
  )

  htmlTemplate %>%
    do.call(modifyList(list(template), options)) %>%
    createDependency(type = "module", scope = "global") %>%
    singleton()
}

#' Generate a shiny bound component.
#'
#' @description
#' Generates a HTML tag with the required web component creation code,
#' global dependencies, shiny bindings and tag content.
#'
#' @param inputId Component Id
#' @param innerHTML A valid HTML fragment to use as the component shadow DOM
#' @param htmlClassName A valid web class name to use in the template
#' @param htmlWCTagName A valid HTML tag name to use in the template
#' @param initialState Initial state of the component binds
#' @param ... Attributes and content of the web component HTML wrapper tag
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom htmltools tagList
#' @importFrom rvest html_attrs
#' @importFrom rvest html_node
#' @importFrom rvest html_nodes
#' @importFrom rvest read_html
#' @importFrom xml2 xml_remove
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace_all
#' @importFrom shiny addResourcePath
#' @importFrom stringi stri_rand_strings
#'
#' @return A HTML tagList.
scaffoldWC <- function(inputId,
                       innerHTML,
                       htmlClassName,
                       htmlWCTagName,
                       initialState,
                       ...) {

  # Extract script tags to be parsed differently
  script_nodes <- innerHTML %>%
    toString() %>%
    HTML() %>%
    read_html() %>%
    html_nodes("script")

  # Extract script tags to be parsed differently
  slot_names <- innerHTML %>%
    toString() %>%
    HTML() %>%
    read_html() %>%
    html_nodes("slot") %>%
    html_attrs() %>%
    lapply(\(node) { node[["name"]] }) %>%
    unlist(use.names = FALSE)

  wc_tag_arguments <- list(...)

  for (name in slot_names) {
    slot_in <- slotIn(name, wc_tag_arguments[[name]])

    wc_tag_arguments[[name]] <- NULL

    wc_tag_arguments <- c(wc_tag_arguments, slot_in)
  }

  # Remove script tags from the full content
  innerHTML %<>%
    toString() %>%
    str_replace_all("<script((.|\\s)*?)\\/script>", "")

  # scripts folder
  # TODO switch to html dep
  dep_dir <- file.path(tempdir(), "www", "shinyBound", "wc", inputId)
  web_dir <- file.path("wc", inputId)

  dir.create(dep_dir, recursive = TRUE, showWarnings = FALSE)
  addResourcePath(web_dir, dep_dir)

  autoSlotScripts <- lapply(script_nodes, function(script) {
      attributes <- script %>%
        html_attrs() %>%
        as.list()

      # Inline scripts require no additional parsing
      if (is.null(attributes$src)) {
        return(as.character(script))
      }

      # If the src is relative, load the file from the base shiny www
      if (file.exists(file.path("www", attributes$src))) {
          parsed_content <- file.path("www", attributes$src) %>%
            readr::read_file() %>%
            writeLines(file.path(dep_dir, basename(attributes$src)))
      } else {
        request <- httr::GET(attributes$src)

        # If the src is not relative, check for a valid http request
        if (httr::http_error(request)) {
          return(as.character(script))
        }

        parsed_content <- request %>%
          httr::content(as = "text") %>%
          writeLines(file.path(dep_dir, basename(attributes$src)))
      }

      attributes$src <- web_dir %>%
        file.path(basename(attributes$src))

      do.call(tags$script, attributes) %>%
        as.character()
  }) %>% unlist()

  tagList(
    htmltools::htmlDependency(
      name = "shinyBound",
      version = "0.1.0",
      src = list(file = "dependencies"),
      package = "shinyBound",
      script = c(
        "shinyBound.js"
      )
    ),
    webComponentBindings(
      system.file("templates/webcomponent-stateful.js", package = "shinyBound"),
      htmlClassName,
      htmlWCTagName,
      innerHTML %>% replacePlaceholders(inputId),
      initialState
    ),
    shinyBindings(
      system.file("templates/shiny-bindings.js", package = "shinyBound"),
      htmlClassName,
      htmlWCTagName
    ),
    htmlComponentTag(htmlWCTagName, inputId, wc_tag_arguments),
    tags$head(HTML(paste0(autoSlotScripts, collapse = " ")))
  )
}
