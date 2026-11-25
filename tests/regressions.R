library(Conigrave)
# Exercise exported functions, including failures that can silently change results.
set.seed(117)
d <- data.frame(x1 = rnorm(80), x2 = rnorm(80), required = rnorm(80))
d$y <- d$x1 + d$x2 + rnorm(80, sd = 0.1)
cases <- list(
  include = function() {
    m <- autoModel(d, "y", include = "required", children = 3, extinction = 5, set.seed = 1)
    stopifnot(all(grepl("required", m$result_history$call, fixed = TRUE)))
  },
  factor = function() {
    m <- autoModel(iris, "Sepal.Length", genepool = "Species", extinction = 5, children = 2)
    stopifnot(nrow(m$result$coefficients) == 3L)
  },
  rank_n = function() {
    t <- ctrx(mtcars, "mpg", "wt", method = "spearman", n.matrix = TRUE)
    stopifnot(t[1, 1] == nrow(mtcars))
  },
  unordered = function() {
    stopifnot(inherits(try(ctrx(iris, "Species", "Sepal.Length"), silent = TRUE), "try-error"))
  },
  triangle = function() {
    stopifnot(is.data.frame(ctrx(mtcars, c("wt", "mpg"), triangle = "lower")))
  },
  numeric_shape = function() {
    stopifnot(is.data.frame(get.numeric(iris[, c("Sepal.Length", "Species")])))
  },
  plot_tibble = function() {
    p <- int.plot(tibble::as_tibble(mtcars), "mpg", "disp", c("cyl", "am"))
    stopifnot(inherits(p$plot, "ggplot"))
  }
)
failures <- vapply(cases, function(f) tryCatch({f(); ""}, error = conditionMessage), "")
if (any(nzchar(failures))) stop(paste(names(failures), failures, collapse = "\n"))

# A later completed data set is missing the requested variable.
uneven <- mitools::imputationList(list(mtcars, mtcars["hp"]))
stopifnot(inherits(try(check_names("mpg", uneven), silent = TRUE), "try-error"))
