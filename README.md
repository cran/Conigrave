# Conigrave

Conigrave searches regression models, builds correlation tables, and plots
interactions for ordinary data frames and multiply imputed data. It analyzes
completed data sets; it does not perform imputation.

## Installation

Install the [CRAN release](https://cran.r-project.org/package=Conigrave) with
`install.packages("Conigrave")`. The workflows below require version 0.5.0 or
later. To install this checkout, run `R CMD INSTALL .` from the package source
directory.

## Search, predict, and plot

This example searches logistic models for manual transmission (`am = 1`).
The required term `"wt,hp"` keeps both predictors and their interaction in
every candidate, so the fitted model can be used to plot that interaction.

```r
library(Conigrave)

m <- autoModel(
  mtcars, outcome = "am", genepool = c("wt", "hp", "qsec"),
  include = "wt,hp", family = binomial(),
  set.seed = 11, max_evaluations = 20, samples = 0
)

m                 # Winning formula, training scores, and why the search stopped
formula(m)        # An R formula, not a string
coef(m)           # Estimates on the modeling scale
summary(m)        # Adds standard errors and bootstrap stability
m$history         # Improvements in search order; the last row is the winner
plot(m)           # Penalized training score by generation

cars <- data.frame(wt = c(2.5, 3.5), hp = c(100, 150), qsec = c(17, 18))
predict(m, newdata = cars)
int.plot(m, predictor = "wt", moderator = "hp")$plot
```

Use raw predictor values with `predict()`: it applies the training
transformations and returns predictions on the original response scale.
Numeric predictors are standardized for fitting. In a logistic model, a
coefficient is therefore on the log-odds scale, not the probability scale.
`m$diagnostics` counts distinct training candidates, including failed fits,
but not bootstrap or validation work. The older `m$result_history` (best
first), `m$result`, and `m$plot` remain available.

Linear models are ranked by adjusted R-squared and GLMs by explained
deviance. The search subtracts a penalty per non-intercept coefficient, and
an intercept-only candidate can win when nothing is required. The score
used for selection is not an estimate of predictive accuracy. Supply
independent `validation` data to score the winner without using those data
to rank candidates. Use `max_evaluations` to bound the training search and
`samples = 0` to skip the optional bootstrap stability calculation.

For other regression models, pass a formula-based `fit` function and a
`score` function that returns a single number to maximize. For example,
with **MASS** installed, rank robust regressions by negative mean absolute
residual:

```r
robust <- autoModel(
  mtcars, outcome = "mpg", genepool = c("wt", "hp"),
  fit = function(formula, data) MASS::rlm(formula, data = data),
  score = function(model) -mean(abs(residuals(model))),
  set.seed = 11, max_evaluations = 8, samples = 0
)
```

## Analyze completed data sets

Pass completed data sets in a `mitools::imputationList()`, with the same
observations in the same order and compatible column types and factor levels.
`autoModel()` averages candidate scores across imputations and pools the winning
coefficients and standard errors. With an imputed model, `predict(m, newdata)`
instead averages the **response-scale predictions**; set `pooled = FALSE` to get
one column per fit. These are point predictions, not pooled prediction
intervals.

## Build correlation tables

```r
vars <- c("mpg", "wt", "hp")
ctrx(mtcars, x = vars, triangle = "lower")
ctrx(mtcars, x = vars, output = "long")
```

The table is formatted for reading. Use long output for unrounded estimates,
p-values, and complete-pair counts, with one row per distinct pair. If a pair
has different counts across imputations, its reported count is `NA`. Partial
correlations are available for a single completed data set, not across
multiple imputations.

For pooled correlation **estimates**, install `miceadds`. For partial
correlation **estimates** on one data frame, install `ppcor`. Neither
package is needed for ordinary single-data-frame correlations, count-only
tables (`n.matrix = TRUE` in table output), or model search and prediction.

See `?autoModel`, `?predict.autoModel`, `?int.plot`, and `?ctrx` for arguments,
defaults, and return values. See [NEWS.md](NEWS.md) for compatibility changes.
