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
#'
#' @return The original object with added attributes (invisibly returned).
#'
#' @examples
#' # fdp_line
#'
#' @name fdp_line
NULL

#' @rdname fdp_line
#' @export
fdp_line <- function(x) {
  attr(x, "fdp_draw") <- "line"
  invisible(x)
}

#' @rdname fdp_line
#' @export
fdp_point <- function(x, hide = FALSE) {
  attr(x, "fdp_draw") <- "point"
  attr(x, "fdp_hide_point") <- hide
  invisible(x)
}
