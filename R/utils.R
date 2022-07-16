#' Remove NULL entries.
#'
#' @description
#' Removes null values from a given list or vector. If the given object is of
#' a diferent type, the same object is returned with no changes.
#'
#' @param x The object to process. Non list or vector objects are returned
#'   with no changes.
#'
#' @return The give object x with all NULL entries removed.
#' @keywords utils internal
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


#' Tests whether the object is a valid ShinySession object.
#'
#' @param object Any R object.
#'
#' @return TRUE or FALSE depending if the object is a valid ShinySession.
#' @keywords utils internal
is.ShinySession <- function(object) {
  inherits(object, c("ShinySession", "MockShinySession", "session_proxy"))
}

#' Is session a ShinySession object.
#'
#' @description
#' Check that an object is a ShinySession object, and give an informative
#  error. The default label is the caller function's name.
#'
#' @param session Any session object.
#' @param label The caller function's name
#'
#' @return No return value, called for side effects.
#' @keywords utils internal
validateSessionObject <- function (session,
                                   label = as.character(
                                     sys.call(sys.parent())[[1]])
                                   ) {
  if (missing(session) || !is.ShinySession(session)) {
    message <- paste(
      "`session` must be a 'ShinySession' object.",
      "Did you forget to pass `session` to `%s()`?"
    )

    stop(call. = FALSE, sprintf(message, label))
  }
}
