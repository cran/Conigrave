library(Conigrave)
choices <- c("cat", "cot", "cast", "dog", "", NA_character_)
stopifnot(identical(find_similar("cat", choices, 100), "cat"))
sets <- lapply(c(0, 25, 50, 75, 100), function(k) find_similar("cat", choices, k))
for (i in 2:length(sets)) stopifnot(all(sets[[i]] %in% sets[[i-1]]))
stopifnot(identical(find_similar("", choices, 100), ""),
          identical(find_similar("cat", choices, 75), c("cast", "cat")),
          identical(find_similar(character(), choices), character()))

# A swapped pair of letters costs two edits, not one.
stopifnot(identical(find_similar("coluor", "colour", 80), character()),
          identical(find_similar("coluor", "colour", 65), "colour"))

# Suggestions use the same edit distance; there is nothing to suggest for
# a data frame without columns.
msg <- tryCatch(check_names("coluor", c("colour", "colur")), error = conditionMessage)
stopifnot(grepl("colur?", msg, fixed = TRUE))
msg <- tryCatch(check_names("coluor", data.frame()), error = conditionMessage)
stopifnot(!grepl("Did you mean", msg, fixed = TRUE))
