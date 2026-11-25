#' Predict from a selected regression model
#'
#' Generate point predictions on the original response scale from raw
#' predictors, using the transformations stored by \code{\link{autoModel}}.
#'
#' @param object A fitted \code{autoModel} object.
#' @param newdata A data frame containing the predictors in the winning
#'   formula. Extra columns are ignored and the outcome is not required.
#'   For imputed predictions, supply either one frame to reuse across fits
#'   or one completed frame per fit, with identical row names in the same order.
#' @param ... Additional arguments to the underlying prediction method.
#'   A custom fitter's prediction method must return one numeric response
#'   prediction per row.
#' @param pooled If \code{TRUE}, average response predictions across fits.
#'   If \code{FALSE}, keep the predictions from each fit separately.
#' @param type Prediction scale. Only \code{"response"} is supported.
#'
#' @details Supply numeric predictors in their original units. Factor
#'   predictors may be supplied as factors or character vectors with known
#'   levels. The method applies each fit's training transformations and
#'   reverses response standardization for built-in linear models. For GLMs,
#'   the averaging is on the response scale, not the link scale.
#'
#' This method returns point predictions, not standard errors or prediction
#' intervals. It does not pool prediction uncertainty.
#'
#' @return A numeric vector in new-data row order, or, with \code{pooled = FALSE},
#'   a matrix with one row per observation and one column per fit.
#' @seealso \code{\link{autoModel}}, \code{\link{int.plot}}
#' @importFrom stats predict
#' @export
predict.autoModel <- function(object, newdata, ..., pooled = TRUE, type = "response") {
  if (!identical(type, "response")) stop("Only response-scale predictions are supported.")
  if (!is.logical(pooled) || length(pooled) != 1L || is.na(pooled))
    stop("pooled must be TRUE or FALSE.")
  frames <- data_frames(newdata)
  m <- length(object$fits)
  if (length(frames) == 1L) frames <- rep(frames, m)
  if (length(frames) != m || !all(vapply(frames, function(d)
    identical(row.names(d), row.names(frames[[1]])), logical(1))))
    stop("Supply one new data frame or one aligned frame per fit.")
  frames <- select_frames(frames, object$predictors)
  values <- matrix(NA_real_, nrow(frames[[1]]), m,
                   dimnames = list(row.names(frames[[1]]), paste0("fit", seq_len(m))))
  for (i in seq_along(frames)) {
    tr <- object$transforms[[i]]
    d <- transform_newdata(frames[[i]], object$prototype, tr, allow_character = TRUE)
    f <- object$fits[[i]]
    p <- if (inherits(f, "glm")) stats::predict(f, newdata = d, type = "response", ...)
         else stats::predict(f, newdata = d, ...)
    if (!is.numeric(p) || !is.null(dim(p)) || length(p) != nrow(d))
      stop("Expected one numeric response prediction per row.")
    values[, i] <- p * tr[object$outcome, "scale"] + tr[object$outcome, "center"]
  }
  if (pooled) rowMeans(values) else values
}
