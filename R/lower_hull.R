# Vector cross product between vectors defined by o to a and by a to b
cross <- function(o, a, b) {
  (a[1L] - o[1L]) * (b[2L] - o[2L]) - (a[2L] - o[2L]) * (b[1L] - o[1L])
}

# Andrew's monotone chain convex hull algorithm (lower only)
# Andrew, A. M. (1979). “Another efficient algorithm for convex hulls in two dimensions”. _Information Processing Letters_, **9**(5), 216-219. \doi{10.1016/0020-0190(79)90072-3}
lower_hull <- function(pts) {
  pts <- pts[order(pts[, 1L]), ]
  lower <- list()
  for (i in seq_len(nrow(pts))) {
    while (length(lower) >= 2L &&
             cross(lower[[length(lower) - 1L]], lower[[length(lower)]], pts[i, ]) <= 0.0) {
      lower <- lower[-length(lower)]
    }
    lower[[length(lower) + 1L]] <- pts[i, ]
  }
  do.call(rbind, lower)
}
