cor_pair <- function(frames, a, b, controls, method) {
  complete <- lapply(frames, function(d) stats::na.omit(d[c(a, b, controls)]))
  ns <- vapply(complete, nrow, integer(1))
  n <- if (all(ns == ns[1])) ns[1] else NA_integer_
  # Fisher pooling needs n > 3; partial tests need positive residual df.
  min_n <- if (length(frames) > 1L) 4L else if (length(controls)) length(controls) + 3L else 2L
  varies <- function(d) all(vapply(d, function(z) any(z != z[1L]), logical(1)))
  if (any(ns < min_n) || !all(vapply(complete, varies, logical(1))))
    return(c(r = NA_real_, p = NA_real_, n = n))
  if (length(frames) > 1L) {
    # Give the pooling routine the actual pairwise sample sizes.
    pooled <- miceadds::micombine.cor(complete, c(a, b), method = method)
    return(c(r = pooled$r[1], p = pooled$p[1], n = n))
  }
  d <- complete[[1]]
  stat <- if (length(controls))
    ppcor::pcor.test(d[[a]], d[[b]], d[controls], method = method)
  else stats::cor.test(d[[a]], d[[b]], method = method)
  c(r = unname(stat$estimate), p = stat$p.value, n = n)
}

# Numeric results for the requested, distinct pairs. Presentation is handled by ctrx().
cor_statistics <- function(frames, x, y, idx, partial, method, n.matrix) {
  if (!nrow(idx)) return(list(r = numeric(), p = numeric(), n = integer()))
  if (!n.matrix && (length(frames) > 1L || length(partial) || method != "pearson")) {
    if (length(frames) > 1L && !requireNamespace("miceadds", quietly = TRUE))
      stop("Install 'miceadds' for pooled correlation estimates.", call. = FALSE)
    if (length(partial) && !requireNamespace("ppcor", quietly = TRUE))
      stop("Install 'ppcor' for partial correlation estimates.", call. = FALSE)
    res <- vapply(seq_len(nrow(idx)), function(i)
      cor_pair(frames, y[idx[i, 1]], x[idx[i, 2]], partial, method), numeric(3))
    return(list(r = res[1, ], p = res[2, ], n = res[3, ]))
  }

  # Count all requested pairs together. A control missing on a row excludes
  # that row from every partial pair, independently of x and y.
  pair_counts <- function(d) {
    if (length(partial)) d <- d[stats::complete.cases(d[partial]), , drop = FALSE]
    if (!anyNA(d)) return(rep.int(nrow(d), nrow(idx)))
    observed <- !is.na(as.matrix(d))
    crossprod(observed[, y, drop = FALSE], observed[, x, drop = FALSE])[idx]
  }
  d <- frames[[1]]
  n <- pair_counts(d)
  for (other in frames[-1]) n[which(n != pair_counts(other))] <- NA_integer_
  if (n.matrix) return(list(n = n))
  rhs <- if (!identical(x, y)) as.matrix(d[x]) else NULL
  r <- stats::cor(as.matrix(d[y]), rhs, use = if (anyNA(d)) "pairwise.complete.obs" else "everything")[idx]
  r[n < 3L] <- NA_real_
  df <- n - 2
  p <- 2 * stats::pt(-abs(r) * sqrt(pmax(df, 0) / pmax(1 - r^2, 0)), df = pmax(df, 1))
  list(r = r, p = p, n = n)
}

#' Build correlation tables
#'
#' Calculate Pearson, Spearman, or Kendall correlations for numeric columns.
#' Return a formatted table or use \code{output = "long"} for unrounded
#' statistics suitable for further analysis. \code{correlatrix()} is an
#' alias for \code{ctrx()}.
#'
#' @aliases correlatrix
#' @param data A data frame or completed data sets in an imputation list
#'   or Amelia object. Selected columns must be numeric vectors.
#' @param x Column names for the table's horizontal axis. Defaults to all columns.
#' @param y Column names for the vertical axis. Defaults to \code{x}.
#' @param triangle \code{"both"}, \code{"lower"}, or \code{"upper"}. The
#'   triangular layouts require identical \code{x} and \code{y}.
#' @param round Number of decimal places in formatted output.
#' @param method One of \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.
#' @param n.matrix If \code{TRUE}, show complete-pair counts instead of
#'   correlations in the formatted table. A count is \code{NA} if it
#'   differs across imputations.
#' @param abbreviate Minimum label length passed to \code{base::abbreviate()}.
#' @param stars P-value thresholds. Each threshold strictly greater than the
#'   p-value adds one star. Use \code{NULL}
#'   to omit stars.
#' @param partial Numeric control variables, identified by column name.
#'   They are removed from the table axes, and observations must be complete
#'   on both variables and all controls. Partial correlations are not
#'   supported across multiple imputations.
#' @param describe \code{FALSE}, \code{TRUE} for mean and standard deviation
#'   (with missing values removed), or a named list of functions that each
#'   return one numeric value. Adds columns for the row variables.
#'   For imputed data, each descriptive statistic is averaged across imputations.
#' @param leading.zero If \code{FALSE}, omit leading zeros in formatted correlations.
#' @param ... \code{var.names} is accepted for compatibility but is deprecated.
#'   Use \code{x} instead.
#' @param output \code{"table"} for formatted output, or \code{"long"} for
#'   one unrounded result per distinct off-diagonal pair. With \code{"long"},
#'   the axis and triangle selections still apply, but formatting and
#'   descriptive columns are not used. Counts are included regardless of
#'   \code{n.matrix}.
#'
#' @details For ordinary Pearson correlations, pairs with fewer than three
#'   complete observations have missing estimates. Constant variables or too
#'   few complete observations for the chosen test give missing statistics
#'   for that pair without dropping other pairs. With multiple imputations,
#'   estimates require the optional miceadds package and are pooled using \code{miceadds::micombine.cor()}.
#'   Partial estimates require ppcor. Count-only tables need neither package.
#' @return A data frame. Formatted output places \code{y} on the rows and
#'   \code{x} on the columns, or uses a triangular layout. Long output has
#'   columns \code{x}, \code{y}, \code{estimate}, \code{p.value}, and \code{n}.
#'   Pairs are not repeated in reverse order.
#' @seealso \code{\link{get.numeric}}
#' @examples
#' vars <- c("mpg", "wt", "hp")
#' ctrx(mtcars, x = vars, triangle = "lower", describe = TRUE)
#' ctrx(mtcars, x = "mpg", y = c("wt", "hp"), method = "spearman", n.matrix = TRUE)
#' ctrx(mtcars, x = vars, output = "long")
#' @export correlatrix
#' @export ctrx

ctrx <- function(data, x = NULL, y = NULL, triangle = "both", round = 2,
                 method = "pearson", n.matrix = FALSE, abbreviate = 100,
                 stars = c(0.05, 0.01, 0.001), partial = c(), describe = FALSE,
                 leading.zero = TRUE, ..., output = c("table", "long")) {
  output <- match.arg(output)
  dots <- list(...)
  if ("var.names" %in% names(dots)) {
    warning("var.names is deprecated; use x.", call. = FALSE)
    x <- dots$var.names
  }
  extra <- setdiff(names(dots), "var.names")
  if (length(extra)) warning("Unused arguments: ", paste(extra, collapse = ", "))
  method <- match.arg(method, c("pearson", "kendall", "spearman"))
  triangle <- match.arg(triangle, c("both", "lower", "upper"))
  if (!is.numeric(stars) && !is.null(stars) || anyNA(stars)) stop("Invalid significance thresholds.")
  frames <- data_frames(data)
  if (is.null(x)) x <- names(frames[[1]])
  if (is.null(y)) y <- x
  cols <- unique(c(x, y, partial))
  frames <- select_frames(frames, cols, numeric_only = TRUE)
  if (length(frames) > 1L && length(partial))
    stop("Partial correlations are not supported for multiple imputations.")
  x <- setdiff(x, partial)
  y <- setdiff(y, partial)
  if (!length(x) || !length(y)) stop("Select at least one variable on each axis.")
  if (!identical(x, y) && triangle != "both") {
    warning("Triangle settings require identical axes.")
    triangle <- "both"
  }
  # Compute only visible pairs; share results between symmetric cells.
  diag_cells <- outer(y, x, "==")
  visible <- switch(triangle, both = !diag_cells, lower = lower.tri(diag_cells), upper = upper.tri(diag_cells))
  cells <- which(visible, arr.ind = TRUE)
  a <- match(y[cells[, 1]], cols)
  b <- match(x[cells[, 2]], cols)
  # Encode unordered pairs numerically; double arithmetic avoids integer overflow.
  keys <- pmin(a, b) + length(cols) * as.double(pmax(a, b))
  first <- which(!duplicated(keys))
  stat <- cor_statistics(frames, x, y, cells[first, , drop = FALSE], partial, method, n.matrix && output == "table")
  if (output == "long")
    return(data.frame(x = x[cells[first, 2]], y = y[cells[first, 1]], estimate = stat$r,
                      p.value = stat$p, n = as.integer(stat$n), row.names = NULL))
  tab <- matrix("", length(y), length(x),
                dimnames = list(base::abbreviate(y, abbreviate), base::abbreviate(x, abbreviate)))
  tab[diag_cells] <- "-"
  if (n.matrix) {
    values <- stat$n
    if (all(visible)) tab <- matrix(NA_real_, nrow(tab), ncol(tab), dimnames = dimnames(tab))
  } else {
    values <- formatC(stat$r, format = "f", digits = round)
    if (!leading.zero) values <- sub("^(-?)0\\.", "\\1.", values)
    nstars <- integer(length(stat$p))
    for (cutoff in stars) nstars <- nstars + as.integer(!is.na(stat$p) & stat$p < cutoff)
    values <- paste0(values, strrep("*", nstars))
  }
  tab[visible] <- values[match(keys, keys[first])]
  if (triangle != "both") {
    dimnames(tab) <- list(paste0(seq_along(y), ". ", y), as.character(seq_along(x)))
    tab <- tab[, -if (triangle == "upper") 1L else length(x), drop = FALSE]
  }
  out <- as.data.frame(tab, stringsAsFactors = FALSE, optional = TRUE)
  if (identical(describe, TRUE))
    describe <- list(M = function(v) mean(v, na.rm = TRUE), SD = function(v) stats::sd(v, na.rm = TRUE))
  if (!identical(describe, FALSE)) {
    if (!is.list(describe) || !length(describe) || is.null(names(describe)) ||
        any(!nzchar(names(describe))) || !all(vapply(describe, is.function, logical(1))))
      stop("describe must be TRUE, FALSE, or a named list of summary functions.")
    for (nm in names(describe)) out[[nm]] <- vapply(y, function(col)
      base::round(mean(vapply(frames, function(d) describe[[nm]](d[[col]]), numeric(1)), na.rm = TRUE), round),
      numeric(1))
  }
  out
}

correlatrix <- ctrx
