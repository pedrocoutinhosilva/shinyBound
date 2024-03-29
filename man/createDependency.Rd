% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templates.R
\name{createDependency}
\alias{createDependency}
\title{Generate a dependency tag.}
\usage{
createDependency(content, scope = "local", type = "script", path_only = FALSE)
}
\arguments{
\item{content}{The content of the tag.}

\item{scope}{The scope of the tag. Can be local or global. Depending on the
scope the return will either be a inline HTML tag to a header HTML tag.}

\item{type}{The type of content. Can be either a JS script, a JS module or
simply valid CSS.}

\item{path_only}{TRUE or FALSE. If TRUE only the path (not the HTML tag) will
be returned after creating the dependency.}
}
\value{
A HTML tagList.
}
\description{
Generates a dependency HTML tag with a given content, scope and type.
}
\keyword{dependencies}
\keyword{internal}
\keyword{templates}
