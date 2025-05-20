#' Control plot type
#'
#' Marks trade-off function objects with drawing attributes
#'
#' The `fdp_line` function adds the attribute `fdp_draw` set to `"line"`
#' to its input, indicating that it should be drawn as a line.
#'
#' The `fdp_point` function adds the attribute `fdp_draw` set to `"point"`
#' and an optional `"fdp_hide_point"` attribute, allowing control over whether the
#' point should be hidden.
#'
#' @param x
#'        An object to which drawing attributes are added.
#' @param hide
#'        Logical (only for `fdp_point`) indicating whether the point should be hidden.
#' @param nm
#'        If set, the name of the f-DP object is set to this string, otherwise the current name is returned.
#'
#' @return The original object with added attributes (invisibly returned).
#'
#' @examples
#' # fdp_line
#'
#' @name fdp_attributes
NULL

#' @rdname fdp_attributes
#' @export
fdp_attributes <- function(x) {
  atts <- attributes(x)
  if (is.null(atts)) return(NULL)
  atts[startsWith(names(atts), "fdp_")]
}

#' @rdname fdp_attributes
#' @export
fdp_line <- function(x) {
  attr(x, "fdp_draw") <- "line"
  invisible(x)
}

#' @rdname fdp_attributes
#' @export
fdp_point <- function(x, hide = FALSE) {
  attr(x, "fdp_draw") <- "point"
  attr(x, "fdp_hide_point") <- hide
  invisible(x)
}

#' @rdname fdp_attributes
#' @export
fdp_name <- function(x, nm) {
  if (missing(nm)) {
    return(attr(x, "fdp_name"))
  }
  attr(x, "fdp_name") <- nm
  invisible(x)
}

# Utility to copy all attributes
copy_atts <- function(to, from, overwrite = FALSE) {
  atts <- attributes(from)
  if (is.null(atts)) return(to)
  for (att in names(atts)) {
    if (startsWith(att, "fdp_") && (overwrite || is.null(attr(to, att)))) {
      attr(to, att) <- atts[[att]]
    }
  }
  invisible(to)
}
