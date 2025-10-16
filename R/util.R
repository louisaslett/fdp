# Utility for debugging environments(!)
print_env_recursive <- function(e, level = 0L) {
  # Stop if e is the global environment
  if (identical(e, globalenv())) return()

  indent <- strrep("  ", level)
  cat(indent, "Environment:", environmentName(e), "\n")
  objs <- ls(envir = e, all.names = TRUE)
  if (length(objs) > 0L) {
    for (obj in objs) {
      cat(indent, "  ", obj, "\n")
    }
  } else {
    cat(indent, "  <no objects>\n")
  }
  parent_e <- parent.env(e)
  if (!identical(parent_e, globalenv()))
    print_env_recursive(parent_e, level + 1L)
}

# This is to allow us to call fdp() with unevaluated arguments.
# So,
#   * fdp(my_dp)
#   * fdp(my_dp(alpha))
#   * fdp(my_dp(alpha, caller_env_var))
# are all fine. Yet also
#   * fdp(X)
#   * fdp(data.frame(...))
# also fine, even if ... references stuff in the calling environment
preprocess_args <- function(args, alpha, tol = sqrt(.Machine$double.eps)) {
  x <- list()
  for (i in seq_along(args)) {
    arg <- args[[i]]
    nm <- names(args)[i]
    # Setup environment for evaluation of arguments
    env <- new.env(parent = parent.frame(2L)) # 1 frame up is fdp; 2 frames up is caller
    env$alpha <- alpha

    # Establish what to do with this argument
    if (is.symbol(arg)) {
      env$fdp_function_to_call <- tryCatch(eval(arg, parent.frame(2L)),
                                           error = function(e) {
                                             cli::cli_abort(c(x = "Error when trying to evaluate argument {.code {as.character(as.expression(arg))}}",
                                                              ">" = e[["message"]]),
                                                            call = parent.frame(5L))
                                           })
      if (is.function(env$fdp_function_to_call)) {
        # CASE 1: Found bare function symbol, so evaluate it on alpha eg fdp(my_dp)
        res <- tryCatch(eval(expression(fdp_function_to_call(alpha)), envir = env),
                        error = function(e) {
                          cli::cli_abort(c(x = "Error when trying to evaluate argument {.code {as.character(as.expression(arg))}}",
                                           ">" = e[["message"]]),
                                         call = parent.frame(5L))
                        })
        res <- copy_atts(res, env$fdp_function_to_call) # Need to see if attributes were set on the function, rather than output of function and copy over (without overwriting)
        res2 <- fixup_type(res, alpha)
        res2 <- copy_atts(res2, res)
        res2 <- fixup_name_draw(res2, nm, arg)
        res2 <- fixup_axis_hugging(res2)
        check_typei_ii(res2, tol)
        x <- c(x, list(res2))
        next
      } else {
        # CASE 2: Found bare symbol that is not a function (ie user passed a variable with df) so just return it eg fdp(X)
        rm("alpha", "fdp_function_to_call", envir = env)
        res <- tryCatch(eval(arg, envir = env),
                        error = function(e) {
                          cli::cli_abort(c(x = "Error when trying to evaluate argument {.code {as.character(as.expression(arg))}}",
                                           ">" = e[["message"]]),
                                         call = parent.frame(5L))
                        })
        res2 <- fixup_type(res) # alpha not involved in this evaluation
        res2 <- copy_atts(res2, res)
        res2 <- fixup_name_draw(res2, nm, arg)
        res2 <- fixup_axis_hugging(res2)
        check_typei_ii(res2, tol)
        x <- c(x, list(res2))
        next
      }
    } else {
      # CASE 3: We have either an unevaluated function call eg fdp(my_dp(alpha, 2))
      #         or an inline defined data frame eg fdp(data.frame(alpha=....,beta=....))
      #         or indirectly a function if it has been modified en-route eg fdp(fdp_point(\(x) 1-x))
      res <- tryCatch(eval(arg, envir = env),
                      error = function(e) {
                        cli::cli_abort(c(x = "Error when trying to evaluate argument {.code {as.character(as.expression(arg))}}",
                                         ">" = e[["message"]]),
                                       call = parent.frame(5L))
                      })
      if (is.function(res)) {
        # Ah! it's an indirect function -- reevaluate in the right environment
        env$fdp_function_to_call <- tryCatch(eval(arg, parent.frame(2L)),
                                             error = function(e) {
                                               cli::cli_abort(c(x = "Error when trying to evaluate argument {.code {as.character(as.expression(arg))}}",
                                                                ">" = e[["message"]]),
                                                              call = parent.frame(5L))
                                             })
        res <- tryCatch(eval(expression(fdp_function_to_call(alpha)), envir = env),
                        error = function(e) {
                          cli::cli_abort(c(x = "Error when trying to evaluate argument {.code {as.character(as.expression(arg))}}",
                                           ">" = e[["message"]]),
                                         call = parent.frame(5L))
                        })
        res <- copy_atts(res, env$fdp_function_to_call) # Need to see if attributes were set on the function, rather than output of function and copy over (without overwriting)
      }
      res2 <- fixup_type(res, alpha)
      res2 <- copy_atts(res2, res)
      res2 <- fixup_name_draw(res2, nm, arg)
      res2 <- fixup_axis_hugging(res2)
      check_typei_ii(res2, tol)
      x <- c(x, list(res2))
      next
    }
  }
  x
}

# This checks if x is a data frame with alpha beta columns, and if not it sees
# whether it can be made so for a given alpha vector
# Do not supply alpha vector if just checking if it is already the right thing
# without automatic fixing up
# NB *only* called from preprocess_arg as errors assume that caller environment
fixup_type <- function(x, alpha = NULL) {
  if (is.data.frame(x)) {
    if (all(c("alpha", "beta") %in% names(x))) {
      return(x)
    } else {
      cli::cli_abort("Argument {.code {as.character(as.expression(arg))}} provides a data frame, but missing required {.code alpha} and {.code beta} columns.",
                     call = parent.frame(2L), # ie fdp()
                     .envir = parent.frame(1L)) # ie preprocess_arg()
      return() # to satisfy linter
    }
  }
  if (is.atomic(x) && !is.list(x)) {
    if (length(x) == length(alpha)) {
      return(data.frame(alpha = alpha, beta = x))
    } else {
      cli::cli_abort("Argument {.code {as.character(as.expression(arg))}} provides a vector but of the wrong length.",
                     call = parent.frame(2L), # ie fdp()
                     .envir = parent.frame(1L)) # ie preprocess_arg()
      return() # to satisfy linter
    }
  }
  cli::cli_abort("Argument {.code {as.character(as.expression(arg))}} provides neither a data frame not vector.",
                 call = parent.frame(2L), # ie fdp()
                 .envir = parent.frame(1L)) # ie preprocess_arg()
}

fixup_name_draw <- function(x, nm, arg = NULL) {
  if (!is.null(nm) && nzchar(nm)) {
    x <- fdp_name(x, nm)
  } else if (is.null(attr(x, "fdp_name"))) {
    # Try to extract function name from the argument as a last resort
    if (!is.null(arg)) {
      if (is.symbol(arg)) {
        # If arg is a bare symbol (e.g., my_fdp), use it as the name
        x <- fdp_name(x, as.character(arg))
      } else if (is.call(arg)) {
        # If arg is a function call (e.g., my_fdp(alpha, 0.1)), extract the function name
        func_name <- as.character(arg[[1L]])
        x <- fdp_name(x, func_name)
      } else {
        cli::cli_abort("Argument {.code {as.character(as.expression(arg))}} is unnamed and does not have a {.code fdp_name} attribute.",
                       call = parent.frame(2L), # ie fdp()
                       .envir = parent.frame(1L)) # ie preprocess_arg()
      }
    } else {
      cli::cli_abort("Argument {.code {as.character(as.expression(arg))}} is unnamed and does not have a {.code fdp_name} attribute.",
                     call = parent.frame(2L), # ie fdp()
                     .envir = parent.frame(1L)) # ie preprocess_arg()
    }
  }

  current_draw <- attr(x, "fdp_draw")
  if (!is.null(current_draw) && !(current_draw %in% c("point", "line"))) {
    cli::cli_abort("Argument {.code {as.character(as.expression(arg))}} has invalid {.code fdp_draw} attribute.",
                   call = parent.frame(2L), # ie fdp()
                   .envir = parent.frame(1L)) # ie preprocess_arg()
  }
  attr(x, "fdp_draw") <- current_draw %||% ifelse(nrow(x) < 100L, "point", "line")
  x
}

fixup_axis_hugging <- function(x) {
  # Sort by x-axis
  x <- x[order(x$alpha), ]
  # Eliminiate any axis hugging, which will only happen on x-axis, only needed when drawing line
  if (attr(x, "fdp_draw") == "line")
    x <- eliminate_axis_hugging(x)
  x
}

check_typei_ii <- function(x, tol) {
  if (any(x$beta > 1.0 - x$alpha + tol)) {
    i <- which(x$beta > 1.0 - x$alpha + tol)[1L]
    cli::cli_abort(c(x = "Argument {.code {as.character(as.expression(arg))}} does not define valid type-I and type-II trade offs.",
                     ">" = paste0("For example, \u03B1 = ", x$alpha[i], " => \u03B2(\u03B1) = ", x$beta[i])),
                   call = parent.frame(2L), # ie fdp()
                   .envir = parent.frame(1L)) # ie preprocess_arg()
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

check_alpha <- function(alpha) {
  if (!is.atomic(alpha) || is.list(alpha)) {
    cli::cli_abort(c(x = "{.code alpha} must be an atomic vector (not a list)."),
                   call = parent.frame(1L))
  }
  if (!is.numeric(alpha)) {
    cli::cli_abort(c(x = "{.code alpha} must contain numeric values."),
                   call = parent.frame(1L))
  }
  if (anyNA(alpha)) {
    cli::cli_abort(c(x = "{.code alpha} must contain numeric values."),
                   call = parent.frame(1L))
  }
  if (any(alpha < 0.0 | alpha > 1.0)) {
    cli::cli_abort(c(x = "All values in {.code alpha} must be in the interval [0, 1]."),
                   call = parent.frame(1L))
  }
  invisible(TRUE)
}

check_scalar <- function(x, min = NULL, max = NULL) {
  name <- deparse(substitute(x))

  if (length(x) != 1L) {
    cli::cli_abort("Argument {.code {name}} must be a scalar value.", call = parent.frame(1L))
  }
  if (is.null(x) || is.na(x)) {
    cli::cli_abort("Argument {.code {name}} must not be NULL or NA.", call = parent.frame(1L))
  }
  if (!is.null(min) && x < min) {
    cli::cli_abort("Argument {.code {name}} must be at least {.val {min}}.", call = parent.frame(1L))
  }
  if (!is.null(max) && x > max) {
    cli::cli_abort("Argument {.code {name}} must be at most {.val {max}}.", call = parent.frame(1L))
  }
  invisible(name)
}
