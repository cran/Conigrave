# Conigrave 0.5.0

## Model search and prediction

* Printing an `autoModel` now shows the winning formula, training score,
  penalized fitness, optional held-out score, and why the search stopped.
  Use `formula()`, `coef()`, `summary()`, and `plot()` directly on the result.
  Coefficients and standard errors are pooled for multiply imputed data.
* `history` records strict improvements in search order with the same columns
  for all model types. `diagnostics` counts attempted, successful, and failed
  candidates. `result_history` remains best-first, with an `R2` column for
  linear models; scores and fitness are no longer rounded.
* `autoModel()` enforces required and excluded terms throughout the search and
  supports single predictors and categorical predictors. Unused factor levels
  are dropped on the common training rows so binary GLM scores and default
  interaction grids use the same levels as the final fit.
* Set `family` to fit a GLM, or provide a custom `fit` function and a
  higher-is-better `score`. Optional independent `validation` data score the
  winning model without participating in selection. An invalid
  `validation_score` callback is rejected before fitting.
* The search skips previously evaluated candidates, reuses numeric interaction
  columns, and accepts a `max_evaluations` budget. All candidates use the same
  complete training rows. Bootstrap stability uses the same row indices across
  imputations. Supplying a search seed no longer changes the caller's random
  number state.
* `predict()` accepts raw predictors and returns original-scale point
  predictions. For multiple imputations, use `pooled = FALSE` to see individual
  fit predictions instead of their response-scale mean.

## Dependencies

* Model histories still return tibbles. dplyr, stringr, and stringdist are
  no longer direct dependencies.
  The correlation backends miceadds (pooled estimates) and ppcor
  (partial estimates) are optional. Count-only tables need neither.

## Correlations and plots

* `ctrx()` computes ordinary Pearson tables in a matrix operation, avoids
  repeating symmetric tests, and counts complete pairs for every method.
  Partial count tables compute all pair counts together after excluding rows
  with missing controls. Pairs with constant variables or too few complete
  observations retain their counts without aborting the other estimates.
  Triangle output remains a data frame when only one column remains.
  `output = "long"` returns unrounded estimates, p-values, and complete-pair
  counts for distinct pairs.
* `int.plot()` accepts tibbles and can plot predictions from a fitted
  `autoModel`, `lm`, or `glm` without refitting. Lines average predictions
  across imputations.
* `get.numeric()` preserves a single numeric column as a data frame and
  checks that the selected columns agree across imputations. `check_names()`
  now checks each completed data set, not just the first.

## Compatibility

* In `autoModel()`, the default maximum interaction order is three. When
  no predictors are required, the intercept-only candidate can win and
  counts toward `max_evaluations`. Winners and seeded search paths can
  differ from version 0.4.4.
* `autoModel()` treats factor levels as categories, even when their labels
  look numeric. `ctrx()` rejects non-numeric columns rather than correlating
  arbitrary factor codes.
* `find_similar()` interprets `percent` as a minimum similarity: 100 means
  exact matches, and higher thresholds cannot add matches. It and
  `check_names()` use Levenshtein edit distance, so swapping adjacent letters
  now counts as two edits instead of one.
* `int.plot()` no longer accepts `save` or `path`. To save a figure, call
  `ggplot2::ggsave("interaction.png", plot = result$plot)` on its result.

## Documentation

* The `autoModel()` examples remain visible in help but are not run during
  automated package checks.
