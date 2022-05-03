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
#' @param webClassName A valid web class name to use in the template
#' @param htmlTagName A valid HTML tag name to use in the template
#'
#' @importFrom magrittr "%>%"
#' @importFrom htmltools htmlTemplate
#' @importFrom utils modifyList
#' @importFrom shiny singleton
#'
#' @return A HTML tagList.
shinyBindings <- function(template, webClassName, htmlTagName) {
  options <- list(
    className = webClassName,
    htmlTagName = htmlTagName
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
#' @param webClassName A valid web class name to use in the template
#' @param htmlTagName A valid HTML tag name to use in the template
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
                                 webClassName,
                                 htmlTagName,
                                 innerHTML,
                                 initialState) {
  initialState %<>%
    dropNulls() %>%
    toJSON(auto_unbox = TRUE)

  options <- list(
    className = webClassName,
    tagName = htmlTagName,
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
#' @param webClassName A valid web class name to use in the template
#' @param htmlTagName A valid HTML tag name to use in the template
#' @param initialState Initial state of the component binds
#' @param ... Attributes and content of the web component HTML wrapper tag
#'
#' @importFrom magrittr "%>%"
#' @importFrom htmltools tagList
#' @importFrom rvest html_attrs
#' @importFrom rvest html_node
#'
#' @return A HTML tagList.
scaffoldWC <- function(inputId,
                       innerHTML,
                       webClassName,
                       htmlTagName,
                       initialState,
                       ...) {

  autoSlotScripts <- stringr::str_extract_all(toString(innerHTML), pattern = "<script((.|\\s)*?)\\/script>", simplify = TRUE)
  innerHTML <- stringr::str_replace_all(toString(innerHTML), pattern = "<script((.|\\s)*?)\\/script>", "")

  dir.create(paste0(tempdir(), "/www/wc/", inputId), recursive = TRUE, showWarnings = FALSE)
  shiny::addResourcePath(paste0("wc/", inputId), paste0(tempdir(), "/www/wc/", inputId))


  #
  # for (script in autoSlotScripts) {
  #   attributes <- rvest::read_html(HTML(script)) %>%
  #     html_node('script') %>%
  #     html_attrs() %>%
  #     as.list()
  #
  #   if (file.exists(paste0("www/", attributes$src))) {
  #     content <- read_html(paste0("www/", attributes$src))
  #   } else {
  #     request <- httr::GET(attributes$src)
  #     if (!httr::http_error(request)) {
  #       content <- request %>% httr::content(as = 'text')
  #     }
  #   }
  #
  #   parsed_url <- paste0("/www/wc/", inputId, "/", basename(attributes$src))
  #
  #   writeLines(
  #     content %>%
  #       stringr::str_replace_all(
  #         "document",
  #         "document.querySelector('#testCard').shadowRoot"
  #       ),
  #     paste0(tempdir(), "/www/wc/", inputId,"/", basename(attributes$src))
  #   )
  #   tags$script(src = parsed_url)
  # }

  autoSlotScripts <- lapply(autoSlotScripts, function(script) {
      attributes <- rvest::read_html(HTML(script)) %>% html_node('script') %>% html_attrs() %>% as.list()

      shadowSelector <- paste0("document.querySelector('#", inputId, "').shadowRoot")
      shadowSelector <- "document"

      if (file.exists(paste0("www/", attributes$src))) {
          parsed <- readr::read_file(paste0("www/", attributes$src)) %>%
              stringr::str_replace_all(
                  "document",
                  shadowSelector
              )

          parsed_url <- paste0("/wc/", inputId,"/", basename(attributes$src))

          # file.create(paste0(tempdir(), "/www/wc/", inputId,"/", basename(attributes$src)))
          writeLines(parsed, paste0(tempdir(), "/www/wc/", inputId,"/", basename(attributes$src)))

          attributes$src <- parsed_url

          return(as.character(do.call(tags$script, attributes)))
      } else {
        request <- httr::GET(attributes$src)
        if (!httr::http_error(request)) {
            parsed <- request %>% httr::content(as = 'text') %>%
                stringr::str_replace_all(
                    "document",
                    shadowSelector
                )

            parsed_url <- paste0("/wc/", inputId,"/", basename(attributes$src))

            # file.create(paste0(tempdir(), "/www/wc/", inputId,"/", basename(attributes$src)))
            writeLines(parsed, paste0(tempdir(), "/www/wc/", inputId,"/", basename(attributes$src)))

            attributes$src <- parsed_url

            return(as.character(do.call(tags$script, attributes)))
        }
      }

      script
  }) %>% unlist()

  tagList(
    htmltools::htmlDependency(
      name = "shinybound",
      version = "0.1.0",
      src = list(file = "dependencies"),
      package = "shinybound",
      script = c(
        "shinyBound.js"
      )
    ),
    webComponentBindings(
      system.file("templates/webcomponent-stateful.js", package = "shinybound"),
      webClassName,
      htmlTagName,
      innerHTML %>% replacePlaceholders(inputId),
      initialState
    ),
    shinyBindings(
      system.file("templates/shiny-bindings.js", package = "shinybound"),
      webClassName,
      htmlTagName
    ),
    webComponentTag(htmlTagName, inputId, ...),
    tags$head(HTML(paste0(autoSlotScripts, collapse = " ")))
  )
}
