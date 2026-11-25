library(Conigrave)
set.seed(27)
d <- data.frame(x = rnorm(60), strongest = rnorm(60), z = rnorm(60))
d$y <- 4*d$x + 6*d$strongest + rnorm(60)
search <- function(validation) autoModel(d, "y", c("x", "strongest", "z"),
  exclude = "strongest", validation = validation, set.seed = 5, samples = 0,
  children = 4, extinction = 2)
a <- search(d)
b <- search(transform(d, y = -y))
stopifnot(!any(grepl("strongest", a$result_history$call)),
          !"strongest" %in% all.vars(formula(a$fits[[1]])),
          identical(a$result_history, b$result_history),
          identical(coef(a$fits[[1]]), coef(b$fits[[1]])),
          !isTRUE(all.equal(a$validation_score, b$validation_score)))

# Reject a malformed callback rather than attempting to call it after fitting.
validate <- function(callback) autoModel(d, "y", "x", samples = 0,
                                         validation = d, validation_score = callback)
err <- tryCatch(validate(42), error = conditionMessage)
if (!identical(err, "validation_score must be a function or NULL."))
  stop("Unexpected validation_score error: ", err)
stopifnot(identical(validate(function(fitted_model, newdata) 0.5)$validation_score, 0.5))

# Observe actual bootstrap rows at the custom fitter boundary, rather than
# merely checking that the reported stability statistic is finite.
draws <- list()
record_fit <- function(formula, data) {
  if (nrow(data) < nrow(d)) draws[[length(draws)+1L]] <<- row.names(data)
  lm(formula, data)
}
mi <- mitools::imputationList(list(d, transform(d, y = y + z)))
invisible(autoModel(mi, "y", "x", include = "x", max_evaluations = 1,
                    fit = record_fit, score = function(f) -AIC(f), samples = 3, set.seed = 7))
stopifnot(length(draws) == 6L, identical(draws[c(1,3,5)], draws[c(2,4,6)]),
          length(unique(draws)) == 3L)
