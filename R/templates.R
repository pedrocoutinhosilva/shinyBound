#' Generate a dependency tag.
#'
#' @description
#'   Generates a dependency HTML tag with a given content, scope and type.
#'
#' @param content The content of the tag.
#' @param scope The scope of the tag. Can be local or global. Depending on the
#'   scope the return will either be a inline HTML tag to a header HTML tag.
#' @param type The type of content. Can be either a JS script, a JS module or
#'   simply valid CSS.
#' @param path_only TRUE or FALSE. If TRUE only the path (not the HTML tag) will
#'   be returned after creating the dependency.
#'
#' @importFrom rlang hash
#' @importFrom shiny resourcePaths
#' @importFrom fs path
#' @importFrom readr write_file
#'
#' @keywords templates dependencies internal
#' @return A HTML tagList.
createDependency <- function(content,
                             scope = "local",
                             type = "script",
                             path_only = FALSE) {
  if ("boundDeps" %in% names(resourcePaths())) {
    dependency_dir <- resourcePaths()[["boundDeps"]]
  } else {
    dependency_dir <- path(tempdir(), "boundDeps")

    if (!dir.exists(dependency_dir)) {
      dir.create(dependency_dir, recursive = TRUE)
    }

    addResourcePath("boundDeps", dependency_dir)
  }

  content_hash <- content |> hash()

  if (type %in% c("script", "module")) {
    if (!file.exists(path(dependency_dir, paste0(content_hash, ".js")))) {
      content <- content |>
        as.character()

      content <- paste(
        "(function () {",
        content,
        "})();"
      )


      write_file(content, path(dependency_dir, paste0(content_hash, ".js")))
    }
  }

  if (type %in% c("style")) {
    if (!file.exists(path(dependency_dir, paste0(content_hash, ".css")))) {
      write_file(
        content |> as.character(),
        path(dependency_dir, paste0(content_hash, ".css"))
      )
    }
  }

  if (path_only) {
    return(path("boundDeps", paste0(content_hash, ".js")))
  }

  wrappedContent <- switch(type,
    "script" = tags$script(
      package = "shinyBound",
      src = path(
        "boundDeps",
        paste0(content_hash, ".js")
      )
    ),
    "module" = tags$script(
      package = "shinyBound",
      type = "module",
      src = path(
        "boundDeps",
        paste0(content_hash, ".js")
      )
    ),
    "style" = tags$link(
      package = "shinyBound",
      rel = "stylesheet",
      href = path(
        "boundDeps",
        paste0(content_hash, ".css")
      )
    )
  )

  return(switch(scope,
    "global" = tags$head(wrappedContent),
    "local" = wrappedContent,
  ))
}

#' Generate a Shiny Bind.
#'
#' @description
#'   Generates a HTML fragment that can be added to a page to initialize a
#'   specific set of shiny bindings for a component. Used in scaffoldWC().
#'
#' @param template Path to an HTML template file.
#' @param htmlClassName A valid web class name to use in the template.
#' @param htmlWCTagName A valid HTML tag name to use in the template.
#'
#' @importFrom magrittr "%>%"
#' @importFrom htmltools htmlTemplate
#' @importFrom utils modifyList
#' @importFrom shiny singleton
#'
#' @keywords templates dependencies internal
#' @return A HTML tagList.
shinyBindings <- function(template, htmlClassName, htmlWCTagName) {
  options <- list(
    className = htmlClassName,
    htmlWCTagName = htmlWCTagName
  )

  htmlTemplate %>%
    do.call(modifyList(list(template), options)) %>%
    createDependency(scope = "local") %>%
    singleton()
}

#' Generate a Web component Bind.
#'
#' @description
#'   Generates a HTML fragment with the required web component creation code.
#'   Used in scaffoldWC().
#'
#' @param template Path to an HTML template file.
#' @param htmlClassName A valid web class name to use in the template.
#' @param htmlWCTagName A valid HTML tag name to use in the template.
#' @param innerHTML A valid HTML fragment to use as the component shadow DOM.
#' @param initialState Initial state of the component binds.
#' @param numberDependencies Number of dependecy scripts the web component will
#'   have. Defaults to 0.
#' @param onRenderCallbacks Additional JS callbacks to run after the component
#'   is rendered.
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom htmltools htmlTemplate
#' @importFrom jsonlite toJSON
#' @importFrom utils modifyList
#' @importFrom shiny singleton
#'
#' @keywords templates dependencies internal
#' @return A HTML tagList.
webComponentBindings <- function(template,
                                 htmlClassName,
                                 htmlWCTagName,
                                 innerHTML,
                                 initialState,
                                 numberDependencies = 0,
                                 onRenderCallbacks = c()) {
  initialState %<>%
    dropNulls() %>%
    sapply(function(single) {
      vals <- single %>%
        listRenderTags() %>%
        dropNulls() %>%
        as.character()

      return(vals)
    }, simplify = FALSE, USE.NAMES = TRUE)

  initialState %<>%
    toJSON(auto_unbox = TRUE)

  onRenderCallbacks %<>%
    dropNulls() %>%
    lapply(function(entry) {
      entry %>% toJSON()
    }) %>%
    toJSON()

  options <- list(
    className = htmlClassName,
    tagName = htmlWCTagName,
    innerHTML = innerHTML,
    initialState = initialState,
    numberDependencies = numberDependencies,
    onRenderCallbacks = onRenderCallbacks
  )

  htmlTemplate %>%
    do.call(modifyList(list(template), options)) %>%
    createDependency(type = "module", scope = "local") %>%
    singleton()
}

#' Generate a shiny bound component.
#'
#' @description
#'   Generates a HTML tag with the required web component creation code,
#'   global dependencies, shiny bindings and tag content.
#'
#' @param inputId The Component Id.
#' @param innerHTML A valid HTML fragment to use as the component shadow DOM.
#' @param htmlClassName A valid web class name to use in the template.
#' @param htmlWCTagName A valid HTML tag name to use in the template.
#' @param initialState Initial state of the component binds.
#' @param ... Attributes and content of the web component HTML wrapper tag.
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom htmltools tagList
#' @importFrom htmltools doRenderTags
#' @importFrom rvest html_attrs
#' @importFrom rvest html_node
#' @importFrom rvest html_nodes
#' @importFrom rvest read_html
#' @importFrom rvest html_text
#' @importFrom readr read_file
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_count
#' @importFrom stringr str_split
#' @importFrom shiny addResourcePath
#' @importFrom shiny resourcePaths
#' @importFrom httr GET
#' @importFrom httr http_error
#' @importFrom httr content
#' @importFrom utils tail
#'
#' @keywords templates internal
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

  # Extract head tags to be parsed differently
  head_nodes <- str_extract_all(innerHTML, "<head((.|\\s)*?)\\/head>")[[1]] %>%
    paste(collapse = "")

  if (paste(head_nodes) == "character(0)") {
    head_nodes <- NULL
  }

  # Extract slot tags to be parsed differently
  slot_names <- innerHTML %>%
    toString() %>%
    HTML() %>%
    read_html() %>%
    html_nodes("slot") %>%
    html_attrs() %>%
    lapply(function(node) {
      node[["name"]]
    }) %>%
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
    str_replace_all("<script((.|\\s)*?)\\/script>", "") %>%
    str_replace_all("<head((.|\\s)*?)\\/head>", "")

  innerHTML <- paste(
    '<link href="shinyBound-0.1.0/shinyBound.css" rel="stylesheet">',
    innerHTML
  )

  numberDependencies <- (innerHTML %>% str_count("<link")) +
    (innerHTML %>% str_count("<style"))

  # scripts folder
  dep_dir <- file.path(tempdir(), "www", "shinyBound", "wc", inputId)
  web_dir <- file.path("wc", inputId)

  dir.create(dep_dir, recursive = TRUE, showWarnings = FALSE)
  addResourcePath(web_dir, dep_dir)

  autoSlotScripts <- sapply(script_nodes, function(script) {
    attributes <- script %>%
      html_attrs() %>%
      as.list()

    # Inline scripts require no additional parsing
    if (is.null(attributes$src)) {
      content <- script |>
        html_text()

      content %<>%
        str_replace_all(
          "document",
          paste0('document.querySelector("#', inputId, '").shadowRoot')
        )

      return(createDependency(content, path_only = TRUE))
    }

    # If the src is relative, check in dependencies
    file_path <- file.path("www", attributes$src)
    resource_paths <- str_split(attributes$src, "/")[[1]][1]

    if (resource_paths %in% names(resourcePaths())) {
      file_path <- file.path(
        resourcePaths()[[resource_paths]],
        do.call(
          file.path,
          as.list(tail(str_split(attributes$src, "/")[[1]], -1))
        )
      )
    }

    if (file.exists(file_path)) {
      file_path %>%
        read_file() %>%
        writeLines(file.path(dep_dir, basename(attributes$src)))
    } else {
      request <- GET(attributes$src)

      # If the src is not relative, check for a valid http request
      if (http_error(request)) {
        return(as.character(script))
      }

      request %>%
        content(as = "text") %>%
        writeLines(file.path(dep_dir, basename(attributes$src)))
    }

    attributes$src <- web_dir %>%
      file.path(basename(attributes$src))

    attributes
  }, simplify = FALSE, USE.NAMES = TRUE)

  onRenderCallbacks <- autoSlotScripts

  componentClass <- webComponentBindings(
    system.file("templates/webcomponent-stateful.js", package = "shinyBound"),
    htmlClassName,
    htmlWCTagName,
    innerHTML %>% replacePlaceholders(inputId),
    initialState,
    numberDependencies,
    onRenderCallbacks %>% dropNulls()
  )

  componentBinding <- shinyBindings(
    system.file("templates/shiny-bindings.js", package = "shinyBound"),
    htmlClassName,
    htmlWCTagName
  )

  fragment <- tagList(
    htmlComponentTag(htmlWCTagName, inputId, wc_tag_arguments),
    tags$head(HTML(paste0(head_nodes, collapse = " "))),
  )

  tagList(
    fragment,
    useShinyBound(),
    tag("template", list(
      id = htmlClassName,
      HTML(innerHTML %>% replacePlaceholders(inputId))
    )),
    componentBinding,
    componentClass
  )
}

#' Generate a shiny bound html dependency.
#'
#' @description
#'   Generates a html dependency to load and track shinyBound web dependencies.
#'
#' @importFrom htmltools htmlDependency
#'
#' @keywords templates dependencies internal
#' @return A HTML dependency object that can be added to a UI defnition.
useShinyBound <- function() {
  htmlDependency(
    name = "shinyBound",
    version = "0.1.0",
    src = list(file = "dependencies"),
    package = "shinyBound",
    script = c(
      "shinyBound.js"
    ),
    stylesheet = c(
      "shinyBound.css"
    )
  )
}
