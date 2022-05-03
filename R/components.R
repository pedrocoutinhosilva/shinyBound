#' Register a component.
#'
#' @description
#' Registers a component template to make it available globally. After
#' registered its componentClass can be used with [useComponent()].
#'
#' @param componentClass A unique identifier for the component template. Used
#'   later to generate instances of the component. Most be unique among all
#'   registered components. If the componentClass provided is already used,
#'   it will overwrite the current registered componentClass.
#' @param innerHTML The HTML structure of the component.
#'
#' @return No return value, called for side effects.
#' @keywords components
#'
#' @export
registerComponent <- function(componentClass, innerHTML) {
  components <- getOption("shinywebcomponents.components")
  components[[componentClass]] <- list(
    innerHTML = innerHTML
  )

  options(shinywebcomponents.components = components)
}

#' Get a registered component.
#'
#' @description
#' Returns a object form of a registered component by its componentClass.
#'
#' @param componentClass The componentClass of a registered component.
#'
#' @return A component object.
#' @keywords components
getComponent <- function(componentClass) {
  stopifnot(
    "No registered component with given componentClass" = {
      componentClass %in% names(getOption("shinywebcomponents.components"))
    }
  )

  getOption("shinywebcomponents.components")[[componentClass]]
}

#' Use a registered component.
#'
#' @description
#' Allows using a registered component in any place where a shiny UI can be
#' used. Usage is similar to base shiny UI input widgets.
#'
#' @param inputId The input slot that will be used to access the value
#' @param componentClass The class of the component that was used in
#'   registerWebComponent to register the component.
#' @param defaultState Initial state for the component
#' @param ... Aditional attributes to be added to the component HTML tag or/and
#'   slotted components
#'
#' @return A HTML tagList.
#' @export
useComponent <- function(inputId, componentClass, defaultState, ...) {
  htmlTagName <- htmlTagName(paste0(componentClass, inputId))

  scaffoldWC(
    inputId = inputId,
    innerHTML = getComponent(componentClass)$innerHTML,
    webClassName = webClassName(htmlTagName),
    htmlTagName = htmlTagName,
    initialState = defaultState,
    ...
  )
}

#' Create a component.
#'
#' @description
#' Creates a web component instance associated with the given inputId with
#' the given HTML and state. For reusable components, it is adviced to use
#' registerWebComponent() and  useWebComponent() instead.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param innerHTML The HTML structure of the component.
#' @param defaultState Initial state for the component.
#' @param ... Named attributes to update and corresponding values.
#'
#' @return A HTML tagList.
#' @export
component <- function(inputId, innerHTML, defaultState, ...) {
  htmlTagName <- htmlTagName(inputId)
  webClassName <- webClassName(htmlTagName)

  scaffoldWC(inputId,  innerHTML, webClassName, htmlTagName, defaultState, ...)
}

#' Update a component state.
#'
#' @description
#' Update the component state with the corresponding inputId.
#'
#' @param session The session object passed to function given to shinyServer.
#'   Default is getDefaultReactiveDomain().
#' @param inputId The id of the `component` object.
#' @param ... Named attributes to update and corresponding values.
#'
#' @importFrom shiny getDefaultReactiveDomain
#'
#' @return No return value, called for side effects.
#' @export
updateComponent <- function(session = getDefaultReactiveDomain(),
                            inputId,
                            ...) {
  validateSessionObject(session)

  session$sendInputMessage(inputId, dropNulls(list(...)))
}

#' Run JS code under the component scope.
#'
#' @description
#' Run JS code under the component scope. The provided callback will have
#' access to the `this` JavaScript property as if running in the component root.
#'
#' @param session The session object passed to function given to shinyServer.
#'   Default is getDefaultReactiveDomain().
#' @param inputId The type of the component that was used in registerComponent
#'   to register the component.
#' @param callback Named attributes to update and corresponding values.
#'
#' @importFrom shiny getDefaultReactiveDomain
#'
#' @export
componentScript <- function(session = getDefaultReactiveDomain(),
                            inputId,
                            callback) {
  validateSessionObject(session)

  session$sendCustomMessage("shinyBoundScopedScript", list(
    id = inputId,
    callback = callback
  ))
}
