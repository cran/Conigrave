#' Keep numeric columns
#'
#' Remove non-numeric columns from a data frame or from each completed
#' data set. A single numeric column is kept as a data frame, not a vector.
#'
#' @param x A data frame, imputation list, or Amelia object.
#' @return A data frame for ordinary input, or an imputation list for
#'   imputed input. For imputed input, the retained column names and
#'   their order must agree across completed data sets.
#' @seealso \code{\link{ctrx}}
#' @examples
#' get.numeric(iris)
#' @export

get.numeric <- function(x) {
  keep <- function(d) d[, vapply(d, is.numeric, logical(1)), drop = FALSE]
  if (!is_imputed(x)) return(keep(x))
  frames <- lapply(data_frames(x), keep)
  if (!all(vapply(frames, function(d) identical(names(d), names(frames[[1]])), logical(1))))
    stop("Numeric columns differ across imputations.", call. = FALSE)
  mitools::imputationList(frames)
}
