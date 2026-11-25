#' Search for a parsimonious regression model
#'
#' Search predictor sets and hierarchical interactions, retaining candidates
#' that improve the fit score after subtracting a complexity penalty.
#' Supports linear regression, generalized linear models, and custom fitters.
#'
#' @param data A data frame, a \code{mitools::imputationList()}, or an Amelia
#'   object. Completed data sets must have the same rows in the same order,
#'   with matching row names, column types, and factor levels.
#' @param outcome Name of the response column.
#' @param genepool Character vector of eligible predictors. By default, use
#'   all columns except the outcome and any excluded predictors.
#' @param extinction Stop after this many generations without improvement.
#' @param children Number of candidate proposals per generation.
#' @param penalty Non-negative cost per non-intercept coefficient. A term
#'   that adds more than one coefficient incurs the cost for each one.
#' @param samples Number of bootstrap resamples for stability. Use zero to skip.
#' @param include Required predictors or comma-separated interactions. For
#'   example, \code{include = "x,z"} requires \code{x}, \code{z}, and \code{x:z}.
#'   Required names must be eligible and cannot also be excluded.
#' @param exclude Character vector of predictor names to omit from the search.
#' @param set.seed Optional search seed. If supplied, the caller's random-number
#'   state is restored on exit.
#' @param family A GLM family object, such as \code{stats::binomial()} or
#'   \code{stats::poisson()}. Use \code{NULL} for ordinary linear regression,
#'   or leave it unset when supplying a custom fitter.
#' @param fit Optional deterministic function \code{function(formula, data)}.
#'   Supply it with \code{score}, not \code{family}. Its fitted object needs
#'   \code{coef()} (returning a numeric vector) and \code{summary()} methods,
#'   and a \code{vcov()} method for pooling across imputations.
#' @param score For a custom fit, a function of the fitted model that returns
#'   one finite number to maximize, for example negative AIC.
#' @param max_evaluations Maximum number of distinct training candidates
#'   attempted, including failed fits. Fitting the winner again and running
#'   optional stability calculations are additional.
#' @param max_order Maximum interaction order, including for required terms.
#'   Use one to search only main effects.
#' @param validation Optional independent data with the outcome and all
#'   eligible predictors. Supply a single frame, reused for each fit, or one
#'   completed data set per fit. These data do not influence model selection.
#' @param validation_score Optional function \code{function(fitted_model, newdata)}
#'   returning one numeric held-out score. Required when validating a custom
#'   fitter. \code{newdata} is transformed using the training means and scales.
#'
#' @details
#' \strong{Search and scoring.} Every candidate uses the same complete rows
#' across the outcome, eligible predictors, and all imputations. Numeric
#' predictors are standardized within each completed data set. Factors are
#' kept as factors even if their labels look numeric. Only the built-in
#' linear model standardizes the response.
#'
#' The search averages adjusted R-squared (linear models), the fraction of
#' null deviance explained (GLMs), or the supplied custom score across
#' imputations. Fitness is this score minus the penalty times the number of
#' non-intercept coefficients. If nothing is required, the search starts with
#' an intercept-only candidate. That candidate counts toward the budget and
#' can win. The search is heuristic; it does not guarantee an exhaustive
#' search or the best possible model.
#'
#' \strong{Validation and stability.} The default held-out score is one
#' minus the ratio of prediction loss to the loss from predicting the
#' training response mean. Loss is squared error for linear models and
#' family deviance for GLMs. The result can be negative. With multiple
#' imputations, this is a mean of per-fit scores, not a score for the mean
#' prediction. Training scores are not estimates of predictive accuracy.
#'
#' Stability is one minus the bootstrap standard deviation of the training
#' score divided by the absolute full-data score. Each replicate samples
#' row indices with replacement and uses the same indices for every
#' imputation. A zero full-data score or an undefined calculation gives
#' \code{NA}. Stability is not held-out performance.
#'
#' @return An object of class \code{autoModel}, stored as a list. Its main
#'   components are listed below. Print the object for a compact report, or
#'   use \code{summary()} for coefficient estimates and bootstrap stability.
#'   \describe{
#'     \item{\code{formula}, \code{coefficients}}{The selected formula and
#'       coefficient estimates (pooled across imputations). Use \code{formula()}
#'       and \code{coef()} to extract them.}
#'     \item{\code{history}}{One row per strict improvement in chronological
#'       order; the last row is the winner. Columns are \code{generation}
#'       (zero for initial candidates), \code{formula} (text),
#'       \code{n_coefficients} (excluding the intercept), \code{score}
#'       (unpenalized), \code{fitness} (penalized), and \code{stability}.}
#'     \item{\code{diagnostics}}{Counts of distinct training candidates
#'       \code{attempted}, \code{succeeded}, and \code{failed}, the number of
#'       search \code{generations} after initialization, and \code{stop_reason}:
#'       \code{"complete"} (all zero- or one-predictor candidates considered),
#'       \code{"budget"} (evaluation limit), or \code{"stagnation"} (no
#'       improvement for \code{extinction} generations). Counts exclude
#'       bootstrap, validation, and the final fit.}
#'     \item{\code{result_history}}{A tibble of improving candidates, best first.
#'       \code{call} is the model formula, \code{length} counts non-intercept
#'       coefficients, and \code{fitness} includes the penalty. The unpenalized
#'       score is \code{R2} (adjusted R-squared) for built-in linear models and
#'       \code{score} otherwise. \code{stability} is the optional bootstrap summary.}
#'     \item{\code{result}}{The winning fit summary, or the pooled coefficient
#'       summary for multiple imputations. Coefficients refer to the modeling
#'       scales, which include standardized numeric predictors.}
#'     \item{\code{fits}, \code{transforms}}{One fitted model and table of
#'       training centers and scales per completed data set. Use
#'       \code{\link{predict.autoModel}} with raw data rather than applying
#'       these transformations yourself.}
#'     \item{\code{plot}}{A plot of improving candidates' penalized training
#'       scores by search generation.}
#'     \item{\code{evaluations}, \code{metric}, \code{validation_score}}{The
#'       number of attempted training candidates, the training metric name,
#'       and the mean held-out score (\code{NULL} if no validation was requested).}
#'   }
#' @seealso \code{\link{autoModel-methods}}, \code{\link{predict.autoModel}}, \code{\link{int.plot}}
#' @examples
#' \dontrun{
#' # Run manually: the search repeatedly fits regression models.
#' autoModel(mtcars, "mpg", set.seed = 10, children = 3)
#'
#' m <- autoModel(
#'   mtcars, "am", genepool = c("wt", "hp"),
#'   include = "wt,hp", family = binomial(),
#'   max_evaluations = 8, samples = 0, set.seed = 11
#' )
#' predict(m, newdata = data.frame(wt = 3.5, hp = 150))
#' int.plot(m, predictor = "wt", moderator = "hp")$plot
#' }
#' @export
autoModel <- function(data, outcome, genepool = NULL, extinction = 30, children = 20,
                      penalty = 0.03, samples = 5, include = c(), exclude = c(),
                      set.seed = NULL, family = NULL, fit = NULL, score = NULL,
                      max_evaluations = 1000L, max_order = 3L, validation = NULL,
                      validation_score = NULL) {
  counts <- list(extinction = extinction, children = children, samples = samples,
                 max_evaluations = max_evaluations, max_order = max_order)
  for (nm in names(counts)) {
    k <- counts[[nm]]
    if (!is.numeric(k) || length(k) != 1L || !is.finite(k) ||
        k != floor(k) || k < as.integer(nm != "samples")) stop("Invalid ", nm)
  }
  if (length(penalty) != 1L || !is.finite(penalty) || penalty < 0) stop("Invalid penalty.")
  if (!is.null(family) && !inherits(family, "family")) stop("Supply a family object, e.g. binomial().")
  if ((!is.null(fit) || !is.null(score)) && !(is.function(fit) && is.function(score) && is.null(family)))
    stop("Custom models require fit and score functions instead of family.")
  if (!is.null(validation_score) && !is.function(validation_score))
    stop("validation_score must be a function or NULL.", call. = FALSE)
  if (!is.null(validation) && !is.null(fit) && is.null(validation_score))
    stop("Supply validation_score for a custom model.")
  frames <- data_frames(data)
  if (!is.character(outcome) || length(outcome) != 1L) stop("Supply one outcome name.")
  if (is.null(genepool)) genepool <- names(frames[[1]])
  check_names(c(outcome, genepool, exclude), frames[[1]])
  pool <- setdiff(genepool, c(outcome, exclude))
  if (any(c(outcome, pool) %in% c(".", "..."))) stop("Rename formula-reserved columns before modelling.")
  included <- lapply(include, function(s) if (s %in% pool) s else trimws(strsplit(s, ",", fixed = TRUE)[[1]]))
  if (!all(unlist(included) %in% pool)) stop("Included predictors must be eligible and not excluded.")
  required <- lapply(included, match, table = pool)
  if (any(lengths(required) > max_order)) stop("Required interaction exceeds max_order.")
  linear <- is.null(family) && is.null(fit)
  inputs <- model_data(frames, c(outcome, pool), outcome, linear, validation)
  frames <- inputs$train
  evaluate <- model_engine(frames, outcome, pool, family, fit, score)
  if (!is.null(set.seed)) {
    old <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
            else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
    set.seed(set.seed)
  }

  seen <- new.env(parent = emptyenv())
  evaluations <- failed <- 0L
  best <- NULL
  best_value <- -Inf
  history <- list()
  last_error <- ""
  consider <- function(proposal, generation) {
    genes <- canonical_genes(c(proposal, required))
    key <- if (length(genes)) paste(vapply(genes, term_key, ""), collapse = ";") else "intercept"
    if (evaluations >= max_evaluations || !is.null(seen[[key]])) return(FALSE)
    seen[[key]] <- TRUE
    evaluations <<- evaluations + 1L
    s <- tryCatch(evaluate(genes), error = function(e) { last_error <<- conditionMessage(e); NULL })
    if (is.null(s)) {
      failed <<- failed + 1L
      return(FALSE)
    }
    fitness <- unname(s["score"] - penalty * s["size"])
    if (fitness <= best_value) return(FALSE)
    best <<- genes
    best_value <<- fitness
    history[[length(history) + 1L]] <<- list(genes = genes, score = unname(s["score"]),
                                            size = unname(s["size"]), fitness = fitness, generation = generation)
    TRUE
  }

  consider(required, 0L)
  for (g in sample.int(length(pool))) {
    if (evaluations >= max_evaluations) break
    consider(list(g), 0L)
  }
  stale <- generation <- 0L
  while (length(pool) > 1L && stale < extinction && evaluations < max_evaluations) {
    generation <- generation + 1L
    parent <- best
    improved <- FALSE
    for (i in seq_len(children)) {
      genes <- parent
      if (length(genes) && stats::runif(1) < 0.5) genes <- genes[-sample.int(length(genes), 1)]
      g <- sample.int(length(pool), 1)
      if (length(parent) && stats::runif(1) < 0.35) {
        h <- union(parent[[sample.int(length(parent), 1)]], g)
        if (length(h) <= max_order) g <- h
      }
      improved <- consider(c(genes, list(g)), generation) || improved
      if (evaluations >= max_evaluations) break
    }
    stale <- if (improved) 0L else stale + 1L
  }
  if (is.null(best)) stop("No estimable candidate: ", last_error, call. = FALSE)
  fits <- evaluate(best, full = TRUE)
  if (length(fits) == 1L) {
    coefficients <- stats::coef(fits[[1]])
    result <- summary(fits[[1]])
  } else {
    pooled <- mitools::MIcombine(fits)
    coefficients <- stats::coef(pooled)
    # summary.MIresult prints its table even when its value is assigned.
    invisible(utils::capture.output(result <- summary(pooled)))
  }
  if (linear && length(fits) > 1L) result$adj.r.squared <- history[[length(history)]]$score
  n <- nrow(frames[[1]])
  resamples <- lapply(seq_len(samples), function(i) sample.int(n, max(2L, round(0.75 * n)), replace = TRUE))
  rows <- lapply(history, function(h) {
    s <- if (h$score == 0) numeric() else vapply(resamples, function(idx)
      tryCatch(unname(evaluate(h$genes, rows = idx)["score"]), error = function(e) NA_real_), numeric(1))
    stability <- if (length(s) >= 2L && h$score != 0) 1 - stats::sd(s) / abs(h$score) else NA_real_
    f <- model_formula(outcome, lapply(h$genes, function(g) pool[g]))
    data.frame(call = paste(deparse(f, backtick = TRUE), collapse = " "), length = h$size,
               fitness = h$fitness, stability = stability, score = h$score, generation = h$generation)
  })
  hist <- do.call(rbind, rows)
  plot <- search_plot(hist)
  improvements <- tibble::as_tibble(hist[c("generation", "call", "length", "score", "fitness", "stability")])
  names(improvements)[2:3] <- c("formula", "n_coefficients")
  hist <- hist[rev(seq_len(nrow(hist))), setdiff(names(hist), "generation"), drop = FALSE]
  if (linear) names(hist)[names(hist) == "score"] <- "R2"
  held_out <- NULL
  if (!is.null(validation)) {
    held_out <- mean(vapply(seq_along(fits), function(i) {
      v <- inputs$validation[[i]]
      if (!is.null(validation_score)) return(validation_score(fits[[i]], v))
      y <- v[[outcome]]
      train <- frames[[i]][[outcome]]
      if (is.factor(train)) {
        if (nlevels(train) != 2L) stop("Held-out scoring requires a binary or numeric outcome.")
        y <- as.integer(y) - 1L
        train <- as.integer(train) - 1L
      }
      pred <- stats::predict(fits[[i]], newdata = v, type = "response")
      loss <- if (linear) function(mu) sum((y - mu)^2) else
        function(mu) sum(family$dev.resids(y, mu, rep(1, length(y))))
      null <- loss(rep(mean(train), length(y)))
      if (!is.finite(null) || null == 0) NA_real_ else 1 - loss(pred) / null
    }, numeric(1)))
  }
  predictors <- pool[sort(unique(unlist(best)))]
  stop_reason <- if (length(pool) <= 1L && evaluations == 1L + as.integer(length(pool) == 1L && !length(required))) {
    "complete"
  } else if (evaluations >= max_evaluations) "budget" else "stagnation"
  diagnostics <- list(attempted = evaluations, succeeded = evaluations - failed, failed = failed,
                      generations = generation, stop_reason = stop_reason)
  structure(list(outcome = outcome, predictors = predictors,
       prototype = frames[[1]][FALSE, predictors, drop = FALSE],
       result_history = tibble::as_tibble(hist), result = result, plot = plot, fits = fits,
       transforms = inputs$transforms, evaluations = evaluations, metric = if (linear) "adjusted R2" else if (is.null(fit))
         "explained deviance" else "custom", validation_score = held_out,
       formula = model_formula(outcome, lapply(best, function(g) pool[g])), coefficients = coefficients,
       history = improvements, diagnostics = diagnostics), class = "autoModel")
}

search_plot <- function(hist) {
  generation <- fitness <- NULL
  ggplot2::ggplot(hist, ggplot2::aes(generation, fitness)) + ggplot2::geom_point() +
    ggplot2::labs(x = "Generation", y = "Penalized training score") + ggplot2::theme_classic()
}
