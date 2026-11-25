#' Inspect a selected model
#'
#' Print the winning formula, scores, and search diagnostics without dumping
#' the fitted models. Use the accessors to work with unrounded results.
#'
#' @param x,object An object returned by \code{\link{autoModel}}; for the
#'   summary print method, the result of \code{summary()} on that object.
#' @param digits Number of significant digits in printed scores and estimates.
#' @param ... Additional arguments passed to the ggplot print method by
#'   \code{plot()}; otherwise unused.
#' @details Numeric predictors are standardized within each training data
#'   set. The built-in linear model also standardizes the response; GLM
#'   coefficients are on the link scale. With multiple imputations,
#'   estimates and standard errors are pooled on the modeling scale.
#'   Standard errors do not account for model selection. Bootstrap stability
#'   describes variation in the training score, not predictive accuracy.
#' @return \code{formula()} returns the selected R formula and \code{coef()}
#'   its numeric coefficient vector. \code{summary()} returns a list with
#'   the formula, training metric, winning row of \code{history} (\code{winner}),
#'   held-out score, search diagnostics, imputation count, and a coefficient
#'   matrix. Standard errors are included for built-in and pooled fits, and
#'   for custom fits with a compatible covariance matrix. Printing returns
#'   its input invisibly. \code{plot()} draws and invisibly returns the
#'   ggplot of improving candidates; use \code{x$plot} to customize it first.
#' @name autoModel-methods
NULL

#' @rdname autoModel-methods
#' @importFrom stats formula
#' @export
formula.autoModel <- function(x, ...) x$formula

#' @rdname autoModel-methods
#' @importFrom stats coef
#' @export
coef.autoModel <- function(object, ...) object$coefficients

model_report <- function(x) {
  list(formula = x$formula, metric = x$metric,
       winner = x$history[nrow(x$history), ], validation_score = x$validation_score,
       diagnostics = x$diagnostics, n_imputations = length(x$fits))
}

print_model_report <- function(report, digits) {
  d <- report$diagnostics
  winner <- report$winner
  cat("autoModel")
  if (report$n_imputations > 1L) cat(" |", report$n_imputations, "imputations")
  cat("\n  ", paste(deparse(report$formula, backtick = TRUE), collapse = "\n  "), "\n\n", sep = "")
  scores <- c(winner$score, winner$fitness, report$validation_score)
  labels <- c(paste0("Training (", report$metric, ")"), "Penalized fitness",
              if (!is.null(report$validation_score)) "Held-out score")
  cat(paste0("  ", format(labels, justify = "left"), " : ",
             format(scores, digits = digits, trim = TRUE)), sep = "\n")
  reason <- switch(d$stop_reason, budget = "evaluation budget reached",
                   stagnation = "no further improvement", complete = "all candidates considered")
  cat("\n  Search: ", d$attempted, " attempted, ", d$succeeded, " scored, ", d$failed,
      " failed\n  Stopped: ", reason, " (generations: ", d$generations, ")\n", sep = "")
}

#' @rdname autoModel-methods
#' @export
print.autoModel <- function(x, ..., digits = 3L) {
  print_model_report(model_report(x), digits)
  invisible(x)
}

#' @rdname autoModel-methods
#' @export
summary.autoModel <- function(object, ...) {
  report <- model_report(object)
  b <- stats::coef(object)
  tab <- cbind(Estimate = b)
  se <- if (report$n_imputations > 1L) {
    object$result$se
  } else if (inherits(object$result, c("summary.lm", "summary.glm"))) {
    object$result$coefficients[, "Std. Error"]
  } else {
    v <- tryCatch(stats::vcov(object$fits[[1]]), error = function(e) NULL)
    if (is.matrix(v) && identical(dim(v), rep(length(b), 2L)) &&
        (is.null(rownames(v)) || identical(rownames(v), names(b)))) sqrt(diag(v))
  }
  if (!is.null(se)) tab <- cbind(tab, "Std. Error" = se)
  report$coefficients <- tab
  structure(report, class = "summary.autoModel")
}

#' @rdname autoModel-methods
#' @export
print.summary.autoModel <- function(x, ..., digits = 3L) {
  print_model_report(x, digits)
  cat("  Bootstrap stability:", format(x$winner$stability, digits = digits), "\n")
  cat("\nCoefficients", if (x$n_imputations > 1L) " (pooled)", " - numeric predictors standardized",
      if (x$metric == "adjusted R2") "; response standardized"
      else if (x$metric == "explained deviance") "; link scale", ":\n", sep = "")
  print.default(x$coefficients, digits = digits)
  invisible(x)
}

#' @rdname autoModel-methods
#' @importFrom graphics plot
#' @export
plot.autoModel <- function(x, ...) {
  print(x$plot, ...)
  invisible(x$plot)
}
