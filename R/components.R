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
component <- function(inputId, innerHTML, defaultState = list(), ...) {
  htmlWCTagName <- htmlWCTagName(inputId)
  htmlClassName <- htmlClassName(htmlWCTagName)

  scaffoldWC(inputId,  innerHTML |> toString(), htmlClassName, htmlWCTagName, defaultState, ...)
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
