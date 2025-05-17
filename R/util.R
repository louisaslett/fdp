preprocess_args <- function(x) {
  for (i in seq_along(x)) {
    # Pre-process to extract lines for functions
    if (is.function(x[[i]])) {
      x[[i]] <- eval_tof(x[[i]], attr(x[[i]], "fdp_name"), attr(x[[i]], "fdp_draw"), attr(x[[i]], "fdp_hide_point"))
    }
    # Setup name
    if (!is.null(names(x)) && nzchar(names(x)[i])) {
      attr(x[[i]], "fdp_name") <- names(x)[i]
    }
    if (is.null(attr(x[[i]], "fdp_name"))) {
      cli::cli_abort(c(x = "Argument {i} is unnamed and does not have an {.code fdp_name} attribute"))
    }
    # Setup drawing type
    attr(x[[i]], "fdp_draw") <- attr(x[[i]], "fdp_draw") %||% ifelse(nrow(x[[i]]) < 100L, "point", "line")
    # Sort by x-axis
    x[[i]] <- x[[i]][order(x[[i]][, 1L]), ]
    # Eliminiate any axis hugging, which will only happen on x-axis, only needed when drawing line
    if (attr(x[[i]], "fdp_draw") == "line")
      x[[i]] <- eliminate_axis_hugging(x[[i]])
  }
  x
}

eliminate_axis_hugging <- function(x) {
  zeros <- which(x$beta == 0.0)
  if (length(zeros) > 0L) {
    x <- x[seq_len(min(zeros)), ]
  }
  x
}

eval_tof <- function(f, fdp_name, fdp_draw, fdp_hide_point) {
  res <- data.frame(alpha = seq(0.0, 1.0, length.out = 100L),
                    beta = f(seq(0.0, 1.0, length.out = 100L)))
  if (any(res$beta > 1.0 - res$alpha)) {
    cli::cli_abort(c(x = "Function does not evaluate to valid type-I and type-II trade offs."))
  }
  if (!is.null(fdp_name))
    attr(res, "fdp_name") <- fdp_name
  if (!is.null(fdp_draw))
    attr(res, "fdp_draw") <- fdp_draw
  if (!is.null(fdp_hide_point))
    attr(res, "fdp_hide_point") <- fdp_hide_point
  res
}
