#' Remove NULL entries.
#'
#' @description
#'   Removes null values from a given list or vector. If the given object is of
#'   a diferent type, the same object is returned with no changes.
#'
#' @param x The object to process. Non list or vector objects are returned
#'   with no changes.
#'
#' @keywords utils internal
#' @return The give object x with all NULL entries removed.
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


#' Tests whether the object is a valid ShinySession object.
#'
#' @param object Any R object.
#'
#' @keywords utils internal
#' @return TRUE or FALSE depending if the object is a valid ShinySession.
is.ShinySession <- function(object) { # nolint
  inherits(object, c("ShinySession", "MockShinySession", "session_proxy"))
}

#' Is session a ShinySession object.
#'
#' @description
#'   Check that an object is a ShinySession object, and give an informative
#    error. The default label is the caller function's name.
#'
#' @param session Any session object.
#' @param label The caller function's name.
#'
#' @keywords utils internal
#' @return No return value, called for side effects.
validateSessionObject <- function(session,
                                  label = as.character(
                                    sys.call(sys.parent())[[1]]
                                  )) {
  if (missing(session) || !is.ShinySession(session)) {
    message <- paste(
      "`session` must be a 'ShinySession' object.",
      "Did you forget to pass `session` to `%s()`?"
    )

    stop(call. = FALSE, sprintf(message, label))
  }
}

#' Get renderable tags from a object.
#'
#' @description
#'  Extracts and returns all HTML tags of an object that can be rendered.
#'
#' @param object A object containing multiple entries. Tipically a HTML object.
#'
#' @keywords utils internal
#' @return A HTML tagList.
listRenderTags <- function(object) {
  vals <- object |>
    sapply(function(item) {
      if (inherits(item, c("shiny.tag", "shiny.tag.list"))) {
        return(as.character(item))
      }

      if (inherits(item, "list")) {
        return(listRenderTags(item))
      }

      item
    }, simplify = FALSE, USE.NAMES = TRUE)

  return(vals)
}
