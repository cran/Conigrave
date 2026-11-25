library(Conigrave)
set.seed(19)
n <- 80
d <- data.frame(x = rnorm(n, 40, 9), z = rnorm(n, -2, 0.8),
                g = factor(rep(c("a", "b"), n/2)))
d$y <- 200 + 4*d$x + 5*d$z + rnorm(n)
d$event <- rbinom(n, 1, plogis((d$x - 40)/12 + d$z + 2))
d$count <- rpois(n, exp(0.01*d$x + 0.2*d$z))
frames <- list(d, transform(d, x = x * 1.4 + 7, y = y - 30))
mi <- mitools::imputationList(frames)
grid <- expand.grid(x = c(27, 40, 60), z = c(-3, -2, -1))
equal <- function(a, b) isTRUE(all.equal(unname(a), unname(b), tolerance = 1e-8))

for (response in c("y", "event", "count")) {
  family <- switch(response, event = binomial(), count = poisson(), NULL)
  m <- autoModel(mi, response, c("x", "z"), include = "x,z", family = family,
                 max_evaluations = 1, samples = 0)
  f <- reformulate("x*z", response)
  refs <- lapply(frames, function(data) if (is.null(family)) lm(f, data)
                else glm(f, data = data, family = family))
  expected <- vapply(refs, predict, numeric(nrow(grid)), newdata = grid, type = "response")
  stopifnot(equal(predict(m, grid, pooled = FALSE), expected),
            equal(predict(m, grid, type = "response"), rowMeans(expected)),
            equal(predict(m, grid[1L, , drop = FALSE]), mean(expected[1, ])))
  different <- transform(grid, x = x + 10)
  paired <- predict(m, mitools::imputationList(list(grid, different)), pooled = FALSE)
  stopifnot(equal(paired[, 1], expected[, 1]),
            equal(paired[, 2], predict(refs[[2]], different, type = "response")))
  # Both plotting entry points must use the original model and response scale.
  for (fitted in list(m, refs[[1]])) {
    p <- int.plot(fitted, predictor = "x", moderator = "z", newdata = grid)
    target <- if (inherits(fitted, "autoModel")) rowMeans(expected) else expected[, 1]
    stopifnot(equal(p$predictions$.prediction, target),
              equal(ggplot2::ggplot_build(p$plot)$data[[1]]$y, target))
  }
  if (!is.null(family)) {
    # MI coefficients and SEs are reported on the standardized predictor scale.
    standardized <- lapply(frames, function(data) {
      data[c("x", "z")] <- lapply(data[c("x", "z")], function(x) as.numeric(scale(x)))
      glm(f, data = data, family = family)
    })
    pooled <- mitools::MIcombine(standardized)
    stopifnot(equal(m$result$results, coef(pooled)),
              equal(m$result$se, sqrt(diag(vcov(pooled)))))
  }
}

fac <- autoModel(d, "y", c("x", "g"), include = "x,g", max_evaluations = 1, samples = 0)
p <- int.plot(fac, predictor = "x", moderator = "g")
stopifnot(nrow(p$predictions) == 202L,
          equal(p$predictions$.prediction, predict(lm(y ~ x*g, d), p$predictions)),
          inherits(try(predict(fac, transform(d, g = "unknown")), silent = TRUE), "try-error"))

# An intercept-only winner still predicts every new row on the original scale.
null <- autoModel(d, "y", genepool = character(), samples = 0)
stopifnot(equal(predict(null, grid), rep(mean(d$y), nrow(grid))))

# A default plot grid must not invent training observations in unused levels.
cars <- transform(mtcars, group = factor(cyl, levels = c(4, 6, 8, 10)))
m <- autoModel(cars, "mpg", c("wt", "group"), include = "wt,group", samples = 0,
               max_evaluations = 1)
g <- int.plot(m, predictor = "wt", moderator = "group")$predictions
stopifnot(identical(levels(g$group), c("4", "6", "8")),
          equal(g$.prediction, predict(lm(mpg ~ wt * group, cars), g)))

# The legacy save arguments must not make int.plot() write a file.
local({
  output <- tempfile("int-plot-")
  stopifnot(dir.create(output))
  on.exit(unlink(output, recursive = TRUE))
  result <- try(int.plot(mtcars, "mpg", "wt", "hp", save = TRUE, path = output),
                silent = TRUE)
  stopifnot(length(list.files(output, all.files = TRUE, no.. = TRUE)) == 0L,
            inherits(result, "try-error"))
})
