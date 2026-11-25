#' Plot a two-way or three-way interaction
#'
#' Fit a standardized linear interaction from data, or plot predictions
#' from an existing model without refitting it.
#'
#' @param data A data frame, completed data sets in an imputation list or
#'   Amelia object, or a fitted \code{autoModel}, \code{lm}, or \code{glm}.
#' @param outcome Name of the response column. Required when supplying data.
#'   Inferred from the fitted model if omitted.
#' @param predictor Name of the numeric predictor on the horizontal axis.
#' @param moderator One or two moderator names. With raw data, moderators
#'   must be numeric. With an existing fit, they can also be categorical.
#' @param y.lim Vertical axis limits. Defaults to \code{c(-1, 1)} for
#'   the standardized data mode and is inferred from predictions for fits.
#' @param x.lim Horizontal axis limits. Defaults to \code{c(-1, 1)} for
#'   the standardized data mode. With a fit and no \code{newdata}, supplied
#'   limits also determine the range of the prediction grid.
#' @param x.lab,y.lab Axis labels. Use \code{"auto"} for the variable names.
#' @param title Plot title. Use \code{"auto"} for a title based on the variables.
#' @param title.size Plot title size.
#' @param SDs Number of standard deviations below and above the mean at
#'   which to draw lines for numeric moderators when no grid is supplied.
#' @param legend.name Legend title. Use \code{"auto"} for the moderator names.
#' @param colour High end of the colour gradient for observed points.
#' @param show.points If \code{TRUE}, add observed points from the first
#'   completed data set. Only supported when supplying data rather than a fit.
#' @param newdata Optional prediction grid for an existing fit. Supply
#'   one row per prediction and all required predictors on their input scales.
#'   A grid is useful for transformed formulas or custom covariate values.
#'
#' @details With data, \code{int.plot()} standardizes the outcome, predictor,
#'   and moderators within each completed data set before fitting a linear
#'   interaction. Both axes are in standard-deviation units.
#'
#' With a fit, the axes use the predictor input scale and model response
#' scale. For an \code{autoModel}, the function uses the stored training
#' transformations, so supply raw predictor values. Without an explicit
#' grid, the numeric predictor ranges from one SD below to one SD above
#' its mean (unless \code{x.lim} is supplied). Lines use numeric moderator
#' values at the mean plus or minus \code{SDs} standard deviations; with one
#' numeric moderator, a line is also drawn at its mean. Categorical
#' moderators use all levels. Other predictors are held at their mean or
#' first level. For imputed fits, these defaults come from the first completed
#' data set, and plotted predictions are averaged across fits.
#'
#' An existing model is used as-is. The plot does not add an interaction term
#' or test whether an interaction is present.
#'
#' To save a figure, pass the returned plot to
#' \code{ggplot2::ggsave("interaction.png", plot = result$plot)}.
#'
#' @return A list with \code{plot} (a ggplot) and \code{summary} (the model
#'   summary). When supplying a fit, \code{predictions} contains the
#'   plotting grid with an added \code{.prediction} column.
#' @seealso \code{\link{autoModel}}, \code{\link{predict.autoModel}}
#' @examples
#' int.plot(mtcars, "mpg", "disp", "cyl", y.lim = c(-2.5, 2.5))$plot
#'
#' fit <- glm(am ~ wt * hp, data = mtcars, family = binomial())
#' int.plot(fit, predictor = "wt", moderator = "hp")$plot
#' @export

int.plot <- function(data, outcome, predictor, moderator, y.lim = c(-1, 1),
                     x.lim = c(-1, 1), x.lab = "auto", y.lab = "auto", title = "auto",
                     title.size = 15, SDs = 1, legend.name = "auto", colour = "ghostwhite",
                     show.points = FALSE, newdata = NULL) {
  from_fit <- inherits(data, c("autoModel", "lm"))
  if (from_fit && missing(outcome))
    outcome <- if (inherits(data, "autoModel")) data$outcome else all.vars(stats::formula(data)[[2L]])[1L]
  if (length(outcome) != 1L || length(predictor) != 1L ||
      !length(moderator) %in% 1:2 || anyDuplicated(c(outcome, predictor, moderator)))
    stop("Supply a distinct outcome, predictor and one or two moderators.")
  if (from_fit) {
    if (show.points) stop("Observed points require a data frame, not a fitted model.")
    if (missing(y.lim)) y.lim <- NULL
    if (missing(x.lim)) x.lim <- NULL
    fitted_grid <- interaction_predictions(data, predictor, moderator, newdata, x.lim, SDs)
    lines <- fitted_grid$lines
    model_summary <- if (inherits(data, "autoModel")) data$result else summary(data)
  } else {
    if (!is.null(newdata)) stop("newdata requires a fitted model.")
    cols <- c(outcome, predictor, moderator)
    frames <- select_frames(data_frames(data), cols, numeric_only = TRUE)
    frames <- lapply(frames, function(d) { d[] <- lapply(d, standardize); d })
    formula <- model_formula(outcome, list(c(moderator, predictor)))
    fits <- lapply(frames, function(d) stats::lm(formula, data = d))
    levels <- if (length(moderator) == 1L) c(-1, 0, 1) else c(-1, 1)
    settings <- expand.grid(rep(list(levels), length(moderator)))
    names(settings) <- moderator
    labels <- if (length(moderator) == 1L) c(paste0("-", SDs, "SD"), "Mean", paste0("+", SDs, "SD"))
      else apply(settings, 1, function(z) paste(moderator, ifelse(z > 0, "high", "low"), collapse = "; "))
    grid <- settings[rep(seq_len(nrow(settings)), each = 2), , drop = FALSE] * SDs
    grid[[predictor]] <- rep(x.lim, nrow(settings))
    estimates <- lapply(fits, stats::predict, newdata = grid)
    lines <- data.frame(xvalue = grid[[predictor]], yvalue = Reduce("+", estimates) / length(fits),
                        group = factor(rep(labels, each = 2), levels = labels))
    fit <- if (length(fits) == 1L) fits[[1]] else mitools::MIcombine(fits)
    model_summary <- summary(fit)
  }
  xvalue <- yvalue <- group <- intensity <- NULL
  p <- ggplot2::ggplot()
  if (show.points) {
    d <- frames[[1]]
    pts <- data.frame(xvalue = d[[predictor]], yvalue = d[[outcome]], intensity = d[[moderator[1]]])
    p <- p + ggplot2::geom_jitter(ggplot2::aes(xvalue, yvalue, colour = intensity), data = pts,
                                  width = 0.025, height = 0) +
      ggplot2::scale_colour_gradient(low = "black", high = colour, guide = "none")
  }
  if (title == "auto") title <- paste(paste(c(predictor, moderator), collapse = " * "), "predicting", outcome)
  if (legend.name == "auto") legend.name <- paste(moderator, collapse = "; ")
  p <- p + ggplot2::geom_line(ggplot2::aes(xvalue, yvalue, linetype = group), data = lines) +
    ggplot2::coord_cartesian(xlim = x.lim, ylim = y.lim) + ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title.size, face = "bold")) +
    ggplot2::labs(x = if (x.lab == "auto") predictor else x.lab,
                  y = if (y.lab == "auto") outcome else y.lab, title = title, linetype = legend.name)
  # The default discrete linetype palette drops groups beyond six. Use colour
  # for larger grids so every prediction curve remains visible.
  if (nlevels(lines$group) > 6L)
    p <- p + ggplot2::scale_linetype_manual(values = rep("solid", nlevels(lines$group)), guide = "none") +
      ggplot2::aes(colour = group) + ggplot2::labs(colour = legend.name)
  result <- list(plot = p, summary = model_summary)
  if (from_fit) result$predictions <- fitted_grid$grid
  result
}

interaction_predictions <- function(fit, predictor, moderator, grid, xlim, sds) {
  auto <- inherits(fit, "autoModel")
  vars <- if (auto) fit$predictors else all.vars(stats::delete.response(stats::terms(fit)))
  check_names(c(predictor, moderator), vars)
  if (is.null(grid)) {
    frame <- if (auto) fit$prototype else stats::model.frame(fit)
    if (!all(vars %in% names(frame)))
      stop("Supply a raw newdata grid for transformed formula terms.")
    values <- lapply(vars, function(nm) {
      z <- frame[[nm]]
      if (is.factor(z) || is.character(z)) {
        if (nm == predictor) stop("The plotted predictor must be numeric.")
        lev <- if (is.factor(z)) levels(z) else unique(z)
        if (!nm %in% moderator) lev <- lev[1L]
        return(factor(lev, levels = if (is.factor(z)) levels(z) else lev, ordered = is.ordered(z)))
      }
      if (!is.numeric(z) || !is.null(dim(z))) stop("Supply an explicit newdata grid.")
      center <- if (auto) fit$transforms[[1]][nm, "center"] else mean(z)
      spread <- if (auto) fit$transforms[[1]][nm, "scale"] else stats::sd(z)
      if (nm == predictor) {
        lim <- if (is.null(xlim)) center + c(-1, 1) * spread else xlim
        return(seq(lim[1], lim[2], length.out = 101L))
      }
      if (!nm %in% moderator) return(center)
      signs <- if (length(moderator) == 1L) -1:1 else c(-1, 1)
      center + signs * sds * spread
    })
    names(values) <- vars
    grid <- expand.grid(values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }
  grid <- select_frames(data_frames(grid), vars)
  if (length(grid) != 1L) stop("Supply a single raw plotting grid.")
  grid <- grid[[1]]
  if (!is.numeric(grid[[predictor]])) stop("The plotted predictor must be numeric.")
  prediction <- if (auto) stats::predict(fit, grid) else
    stats::predict(fit, newdata = grid, type = "response")
  group <- do.call(interaction, c(unname(as.list(grid[moderator])), list(drop = TRUE, sep = "; ")))
  lines <- data.frame(xvalue = grid[[predictor]], yvalue = as.numeric(prediction), group = group)
  grid$.prediction <- as.numeric(prediction)
  list(lines = lines, grid = grid)
}
