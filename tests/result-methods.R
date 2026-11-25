library(Conigrave)

# A required interaction exercises real formula syntax (including quoting).
d <- mtcars
names(d)[names(d) == "hp"] <- "horse power"
m <- autoModel(d, "mpg", c("wt", "horse power"), include = "wt,horse power",
               max_evaluations = 1, samples = 0, validation = d)
h <- m$history
s <- summary(m)
out <- capture.output(printed <- withVisible(print(m)))
stopifnot(inherits(formula(m), "formula"),
          identical(attr(terms(formula(m)), "term.labels"), attr(terms(m$fits[[1]]), "term.labels")),
          all.equal(coef(m), coef(m$fits[[1]])),
          all.equal(s$coefficients, coef(summary(m$fits[[1]]))[, 1:2]),
          identical(s$winner, h), identical(s$validation_score, m$validation_score),
          identical(printed$value, m), !printed$visible,
          any(grepl("horse power", out, fixed = TRUE)), any(grepl(m$metric, out, fixed = TRUE)),
          any(grepl(format(m$validation_score, digits = 3), out, fixed = TRUE)),
          identical(m$diagnostics$stop_reason, "budget"))

# Generation zero can have several improving candidates; only failed fits
# (not duplicate proposals or successfully scored losers) are failures.
d <- transform(mtcars, bad = 1)
m <- autoModel(d, "mpg", c("wt", "bad"), max_order = 1, samples = 0,
               extinction = 2, children = 2, set.seed = 1)
h <- m$history
stopifnot(nrow(h) >= 2L, all(diff(h$fitness) > 0), all(diff(h$generation) >= 0),
          identical(h$formula, rev(m$result_history$call)), identical(h$score, rev(m$result_history$R2)),
          identical(h$n_coefficients, rev(m$result_history$length)),
          identical(m$diagnostics$stop_reason, "stagnation"), m$diagnostics$failed >= 1L,
          m$diagnostics$attempted == m$evaluations,
          m$diagnostics$succeeded + m$diagnostics$failed == m$evaluations)
p <- tempfile(fileext = ".pdf")
grDevices::pdf(p)
drawn <- withVisible(plot(m))
grDevices::dev.off()
stopifnot(identical(drawn$value, m$plot), !drawn$visible, file.size(p) > 0)
unlink(p)

null <- autoModel(d, "mpg", "bad", samples = 0)
stopifnot(identical(all.vars(formula(null)), "mpg"), formula(null)[[3L]] == 1,
          null$diagnostics$failed == 1L, null$diagnostics$succeeded == 1L,
          identical(null$diagnostics$stop_reason, "complete"))
limited <- autoModel(d, "mpg", "wt", max_evaluations = 1, samples = 0)
stopifnot(identical(limited$diagnostics$stop_reason, "budget"))

# Pooled GLM summaries agree with Rubin pooling, not a single imputation.
imp <- mitools::imputationList(list(mtcars, transform(mtcars, wt = wt + hp / 100)))
out <- capture.output(mi <- autoModel(imp, "am", "wt", include = "wt", family = binomial(),
                                      max_evaluations = 1, samples = 0))
ref <- mitools::MIcombine(mi$fits)
s <- summary(mi)
sout <- capture.output(shown <- withVisible(print(s)))
stopifnot(!length(out), identical(shown$value, s), !shown$visible,
          any(grepl("pooled", sout, fixed = TRUE)), !any(grepl("Held-out", sout, fixed = TRUE)),
          all.equal(coef(mi), coef(ref)), all.equal(s$coefficients[, 2], sqrt(diag(vcov(ref)))),
          identical(mi$history$score, rev(mi$result_history$score)),
          identical(mi$diagnostics$stop_reason, "complete"))

# The custom-fit contract need not include formula() or vcov() for one fit.
calls <- 0L
custom <- autoModel(d, "mpg", "wt", include = "wt", samples = 0,
                    fit = function(f, d) {
                      calls <<- calls + 1L
                      list(coefficients = coef(lm(f, data = d)))
                    }, score = function(x) 1)
stopifnot(all.equal(coef(custom), coef(custom$fits[[1]])),
          identical(all.vars(formula(custom)), c("mpg", "wt")),
          ncol(summary(custom)$coefficients) == 1L, calls == custom$evaluations + 1L)
