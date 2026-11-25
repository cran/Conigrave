#' Check that requested column names exist
#'
#' Check a data frame, each completed data set, or a supplied vector of
#' available names. Suggest similar names by Levenshtein edit distance
#' when a requested name is missing.
#'
#' @param x Character vector of requested names.
#' @param data A data frame, imputation list, Amelia object, or character
#'   vector of available names.
#' @return Invisibly returns \code{NULL} if all names exist. Otherwise,
#'   raises an error listing the missing names.
#' @seealso \code{\link{find_similar}}
#' @examples
#' check_names(c("mpg", "wt"), mtcars)
#' @export

check_names <- function(x, data) {
  if (is_imputed(data)) {
    for (d in data_frames(data)) check_names(x, names(d))
    return(invisible(NULL))
  }
  available <- if (is.character(data)) data else as.character(names(data))
  missing <- setdiff(x, available)
  if (!length(missing)) return(invisible(NULL))
  msg <- paste0(length(missing), if (length(missing) == 1L) " name" else " names",
                " could not be found: ", paste(missing, collapse = ", "), ".")
  guesses <- vapply(missing, function(s) {
    distance <- utils::adist(s, available)[1L, ]
    j <- which.min(distance)
    if (length(j) && distance[j] < 4 + nchar(s)) available[j] else NA_character_
  }, character(1))
  guesses <- unique(guesses[!is.na(guesses)])
  if (length(guesses)) msg <- paste0(msg, " Did you mean: ", paste(guesses, collapse = ", "), "?")
  stop(msg, call. = FALSE)
}
