is_imputed <- function(x) inherits(x, c("imputationList", "amelia"))

data_frames <- function(data) {
  frames <- if (is_imputed(data)) data$imputations else list(data)
  if (!length(frames) || !all(vapply(frames, is.data.frame, logical(1))))
    stop("Expected a data frame or imputations containing data frames.", call. = FALSE)
  lapply(frames, as.data.frame)
}

select_frames <- function(frames, columns, numeric_only = FALSE) {
  lapply(frames, function(d) {
    check_names(columns, d)
    if (anyDuplicated(names(d))) stop("Column names must be unique.", call. = FALSE)
    d <- d[columns]
    if (numeric_only) {
      bad <- !vapply(d, function(z) is.numeric(z) && is.null(dim(z)), logical(1))
      if (any(bad)) stop("These columns must be numeric vectors: ",
                         paste(names(d)[bad], collapse = ", "), call. = FALSE)
    }
    d
  })
}

# Construct calls from symbols, not by parsing user-supplied column names.
model_formula <- function(outcome, terms) {
  if (any(c(outcome, unlist(terms)) %in% c(".", "...")))
    stop("Rename formula-reserved columns '.' or '...' before modelling.")
  product <- function(vars) Reduce(function(a, b) call("*", a, b), lapply(vars, as.name))
  rhs <- if (!length(terms)) 1 else Reduce(function(a, b) call("+", a, b), lapply(terms, product))
  stats::as.formula(call("~", as.name(outcome), rhs), env = baseenv())
}

standardize <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || !s) stop("Cannot standardize a constant or empty column.")
  (x - mean(x, na.rm = TRUE)) / s
}

# Apply training scales and factor levels to held-out or prediction data.
transform_newdata <- function(d, prototype, tr, allow_character = FALSE, unscaled = NULL) {
  for (nm in names(d)) {
    ref <- prototype[[nm]]
    z <- d[[nm]]
    if (is.factor(ref)) {
      if (!(is.factor(z) || (allow_character && is.character(z))) || any(!is.na(z) & !z %in% levels(ref)))
        stop("Incompatible factor: ", nm)
      d[[nm]] <- factor(z, levels = levels(ref), ordered = is.ordered(ref))
    } else {
      if (!is.numeric(z) || !is.null(dim(z))) stop("Expected a numeric column: ", nm)
      if (!identical(nm, unscaled)) d[[nm]] <- (z - tr[nm, "center"]) / tr[nm, "scale"]
    }
  }
  d
}
