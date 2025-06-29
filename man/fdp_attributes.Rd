% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attrs.R
\name{fdp_attributes}
\alias{fdp_attributes}
\alias{fdp_line}
\alias{fdp_point}
\alias{fdp_name}
\title{Control plot type}
\usage{
fdp_attributes(x)

fdp_line(x)

fdp_point(x, hide = FALSE)

fdp_name(x, nm)
}
\arguments{
\item{x}{An object to which drawing attributes are added.}

\item{hide}{Logical (only for \code{fdp_point}) indicating whether the point should be hidden.}

\item{nm}{If set, the name of the f-DP object is set to this string, otherwise the current name is returned.}
}
\value{
The original object with added attributes (invisibly returned).
}
\description{
Marks trade-off function objects with drawing attributes
}
\details{
The \code{fdp_line} function adds the attribute \code{fdp_draw} set to \code{"line"}
to its input, indicating that it should be drawn as a line.

The \code{fdp_point} function adds the attribute \code{fdp_draw} set to \code{"point"}
and an optional \code{"fdp_hide_point"} attribute, allowing control over whether the
point should be hidden.

Note in docs \code{fdp_name} is useful in case of pasting stuff, eg fdp_name(..., paste0(...))
}
\examples{
# fdp_line

}
