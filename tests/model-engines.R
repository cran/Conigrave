library(Conigrave)
set.seed(55)
d <- data.frame(x = rnorm(150), group = factor(rep(letters[1:3], 50)))
d$binary <- rbinom(150, 1, plogis(d$x))
d$count <- rpois(150, exp(0.3 * d$x))
train <- d[1:100, ]
test <- d[101:150, ]
for (family in list(binomial(), poisson())) {
  y <- if (family$family == "binomial") "binary" else "count"
  reference <- glm(reformulate("x", y), data = train, family = family)
  before <- .Random.seed
  m <- autoModel(train, y, "x", family = family, samples = 0, set.seed = 1, validation = test)
  expected <- 1 - sum(family$dev.resids(test[[y]], predict(reference, test, type = "response"), rep(1, 50))) /
    sum(family$dev.resids(test[[y]], rep(mean(train[[y]]), 50), rep(1, 50)))
  stopifnot(identical(before, .Random.seed), m$evaluations == 2,
            all.equal(m$validation_score, expected),
            all.equal(m$result_history$score[1], 1 - reference$deviance / reference$null.deviance))
}

# Count calls at the adapter boundary: no refits of cached training candidates.
calls <- 0L
model <- autoModel(train, "count", c("x", "group"), fit = function(f, d) {
  calls <<- calls + 1L
  lm(f, data = d)
}, score = function(m) -AIC(m), max_evaluations = 3, samples = 0)
stopifnot(calls == model$evaluations + 1L, model$evaluations <= 3L,
          all.equal(model$result_history$score[1], -AIC(model$fits[[1]])))

# Fast hierarchical QR scoring matches the actual winning model.
d <- mtcars
names(d)[names(d) == "hp"] <- "horse power"
m <- autoModel(d, "mpg", c("wt", "horse power", "cyl"), include = "horse power,wt",
               exclude = "am", set.seed = 11, children = 3, extinction = 3)
stopifnot(all.equal(m$result_history$R2[1], m$result$adj.r.squared),
          all(c("horse power", "wt") %in% all.vars(formula(m$fits[[1]]))))
imps <- mitools::imputationList(list(d, transform(d, mpg = mpg + wt)))
mi <- autoModel(imps, "mpg", "wt", samples = 3, set.seed = 11)
stopifnot(length(mi$fits) == 2L, is.finite(mi$result_history$stability[1]),
          all.equal(mi$result_history$R2[1], mean(vapply(mi$fits, function(f) summary(f)$adj.r.squared, 0))))

# A singleton gene with index three is column three, not columns 1:3.
third <- autoModel(mtcars, "mpg", c("wt", "hp", "disp"), include = "disp",
                   samples = 0, max_evaluations = 1, set.seed = 42)
stopifnot(third$result_history$length == 1L,
          all.equal(third$result_history$R2, summary(lm(mpg ~ disp, mtcars))$adj.r.squared))

# Mixed predictors take the formula-based GLM path, with real factor contrasts.
mixed <- autoModel(train, "binary", c("x", "group"), include = "x,group",
                   family = binomial(), max_evaluations = 1, samples = 0)
ref <- glm(binary ~ x * group, train, family = binomial())
stopifnot(all.equal(mixed$result_history$score, 1 - ref$deviance / ref$null.deviance),
          all.equal(unname(fitted(mixed$fits[[1]])), unname(fitted(ref))))

# Subsetting can leave unused levels on a binary response. The search,
# final glm and held-out score must all agree on the reference category.
bin <- transform(mtcars, event = factor(am, levels = c(2, 0, 1)))
ref <- glm(event ~ wt, bin, family = binomial())
m <- autoModel(bin, "event", "wt", include = "wt", family = binomial(),
               validation = bin, max_evaluations = 1, samples = 0)
score <- 1 - ref$deviance / ref$null.deviance
stopifnot(all.equal(m$history$score, score), all.equal(m$validation_score, score),
          all.equal(unname(predict(m, bin)), unname(fitted(ref))))

# Both fits name their coefficients gC and gD, but those contrasts cannot
# be pooled if one imputation uses A as the baseline and the other uses B.
factor_frame <- function(baseline) data.frame(
  y = c(1:4, 8:11, 12:15),
  g = factor(rep(c(baseline, "C", "D"), each = 4), levels = LETTERS[1:4])
)
fit_groups <- function(a, b) autoModel(
  mitools::imputationList(list(a, b)), "y", "g", include = "g",
  samples = 0, max_evaluations = 1
)
a <- factor_frame("A")
err <- tryCatch({ fit_groups(a, factor_frame("B")); NULL }, error = conditionMessage)
stopifnot(!is.null(err), grepl("levels", err), grepl("\\bg\\b", err))

# A level absent from all imputations leaves the contrasts comparable.
stopifnot(identical(names(coef(fit_groups(a, a))), c("(Intercept)", "gC", "gD")))
