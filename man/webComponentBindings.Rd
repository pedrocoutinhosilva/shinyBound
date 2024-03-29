% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templates.R
\name{webComponentBindings}
\alias{webComponentBindings}
\title{Generate a Web component Bind.}
\usage{
webComponentBindings(
  template,
  htmlClassName,
  htmlWCTagName,
  innerHTML,
  initialState,
  numberDependencies = 0,
  onRenderCallbacks = c()
)
}
\arguments{
\item{template}{Path to an HTML template file.}

\item{htmlClassName}{A valid web class name to use in the template.}

\item{htmlWCTagName}{A valid HTML tag name to use in the template.}

\item{innerHTML}{A valid HTML fragment to use as the component shadow DOM.}

\item{initialState}{Initial state of the component binds.}

\item{numberDependencies}{Number of dependecy scripts the web component will
have. Defaults to 0.}

\item{onRenderCallbacks}{Additional JS callbacks to run after the component
is rendered.}
}
\value{
A HTML tagList.
}
\description{
Generates a HTML fragment with the required web component creation code.
  Used in scaffoldWC().
}
\keyword{dependencies}
\keyword{internal}
\keyword{templates}
