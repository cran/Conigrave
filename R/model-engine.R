# All candidates see the same observations. Save training transformations for
# validation; new observations must never supply training means or scales.
model_data <- function(frames, cols, outcome, linear, validation) {
  frames <- select_frames(frames, cols)
  aligned <- vapply(frames, function(d) identical(row.names(d), row.names(frames[[1]])), logical(1))
  if (!all(aligned)) stop("Imputations must have aligned rows.")
  rows <- Reduce("&", lapply(frames, stats::complete.cases))
  if (sum(rows) < 3L) stop("Too few common complete observations.")
  if (!is.null(validation)) {
    validation <- select_frames(data_frames(validation), cols)
    if (length(validation) == 1L) validation <- rep(validation, length(frames))
    if (length(validation) != length(frames)) stop("Validation imputations must match training imputations.")
  }
  transforms <- vector("list", length(frames))
  reference <- frames[[1]]
  for (i in seq_along(frames)) {
    transform <- data.frame(center = rep(0, length(cols)), scale = rep(1, length(cols)), row.names = cols)
    d <- frames[[i]][rows, , drop = FALSE]
    for (col in cols) {
      z <- d[[col]]
      ref <- reference[[col]]
      if (is.factor(z) && is.factor(ref)) {
        if (!identical(levels(z), levels(ref)) || is.ordered(z) != is.ordered(ref) ||
            !identical(attr(z, "contrasts"), attr(ref, "contrasts")))
          stop("Factor levels differ between imputations: ", col)
        if (linear && col == outcome) stop("Linear regression requires a numeric outcome.")
        # Match the model frame used by lm/glm, including its reference level.
        if (any(tabulate(z, nbins = nlevels(z)) == 0L)) d[[col]] <- droplevels(z)
        if (i > 1L && !identical(levels(d[[col]]), levels(frames[[1]][[col]])))
          stop("Observed factor levels differ between imputations: ", col)
      } else if (is.numeric(z) && is.null(dim(z)) && is.numeric(ref)) {
        if (!all(is.finite(z))) stop("Non-finite training values: ", col)
        if (linear || col != outcome) {
          center <- mean(z)
          spread <- stats::sd(z)
          if (!spread) spread <- 1
          transform[col, ] <- c(center, spread)
          d[[col]] <- (z - center) / spread
        }
      } else stop("Expected a numeric or factor column in every imputation: ", col)
    }
    transforms[[i]] <- transform
    frames[[i]] <- d
    if (!is.null(validation)) validation[[i]] <- transform_newdata(validation[[i]], d, transform,
      unscaled = if (!linear) outcome)
  }
  list(train = frames, validation = validation, transforms = transforms)
}

# Each gene is an integer vector: c(1, 3) denotes main effects 1, 3
# and their interaction. Canonical keys make equivalent models share a score.
term_key <- function(g) paste(g, collapse = ":")
canonical_genes <- function(genes) {
  genes <- unique(lapply(genes, function(g) if (length(g) > 1L) sort.int(unique(g)) else g))
  for (h in genes[lengths(genes) > 1L])
    genes <- genes[!vapply(genes, function(g) length(g) < length(h) && all(g %in% h), logical(1))]
  genes[order(vapply(genes, term_key, ""))]
}

expand_gene <- function(g) {
  if (length(g) == 1L) return(list(g))
  do.call(c, lapply(seq_along(g), function(k) utils::combn(g, k, simplify = FALSE)))
}

model_engine <- function(frames, outcome, pool, family, fitter, scorer) {
  linear <- is.null(family) && is.null(fitter)
  # For numeric lm/glm searches, columns are shared across thousands of
  # candidate QR fits. Cache interaction products lazily, not the model space.
  fast <- is.null(fitter) && all(vapply(frames[[1]][pool], is.numeric, logical(1)))
  designs <- lapply(frames, function(d) new.env(parent = emptyenv()))
  if (is.null(fitter)) fitter <- if (linear) stats::lm else function(f, d) stats::glm(f, data = d, family = family)
  function(genes, rows = NULL, full = FALSE) {
    if (fast && !full) {
      n <- if (is.null(rows)) nrow(frames[[1]]) else length(rows)
      if (length(genes) && 2^max(lengths(genes)) >= n) stop("Interaction exceeds available degrees of freedom.")
      terms <- unique(do.call(c, lapply(genes, expand_gene)))
    } else f <- model_formula(outcome, lapply(genes, function(g) pool[g]))
    fits <- if (full) vector("list", length(frames))
    scores <- sizes <- numeric(length(frames))
    for (i in seq_along(frames)) {
      d <- frames[[i]]
      if (fast && !full) {
        columns <- lapply(terms, function(g) {
          key <- term_key(g)
          cache <- designs[[i]]
          if (is.null(cache[[key]])) cache[[key]] <- Reduce(`*`, d[pool[g]])
          cache[[key]]
        })
        x <- do.call(cbind, c(list(rep(1, nrow(d))), columns))
        y <- d[[outcome]]
        if (!is.null(rows)) { x <- x[rows, , drop = FALSE]; y <- y[rows] }
        fit <- if (linear) stats::lm.fit(x, y) else stats::glm.fit(x, y, family = family)
        sizes[i] <- fit$rank - 1
        if (fit$rank < ncol(x) || !fit$df.residual || (!linear && !fit$converged))
          stop("Singular, saturated or non-convergent model.")
        scores[i] <- if (linear) 1 - (sum(fit$residuals^2) / fit$df.residual) / stats::var(y)
                     else 1 - fit$deviance / fit$null.deviance
      } else {
        if (!is.null(rows)) d <- d[rows, , drop = FALSE]
        fit <- fitter(f, d)
        b <- stats::coef(fit)
        if (!is.numeric(b) || !is.null(dim(b)) || !length(b) || any(!is.finite(b)) ||
            (inherits(fit, "glm") && !isTRUE(fit$converged))) stop("Unestimable coefficients.")
        if (i == 1L) coefficient_names <- names(b)
        else if (!identical(names(b), coefficient_names)) stop("Coefficient names differ across imputations.")
        sizes[i] <- length(b) - as.integer("(Intercept)" %in% names(b))
        score <- if (!is.null(scorer)) scorer(fit) else if (linear) summary(fit)$adj.r.squared
                 else 1 - fit$deviance / fit$null.deviance
        if (!is.numeric(score) || length(score) != 1L) stop("Expected a single numeric score.")
        scores[i] <- score
        if (full) fits[[i]] <- fit
      }
    }
    # The null model is its own reference. Avoid round-off creating spurious
    # improvements or a bootstrap stability ratio with a near-zero divisor.
    if (!length(genes) && is.null(scorer)) scores[] <- 0
    if (any(!is.finite(scores))) stop("Non-finite model score.")
    if (full) fits else c(score = mean(scores), size = mean(sizes))
  }
}
