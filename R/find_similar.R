#' Match strings by minimum similarity
#'
#' Find strings in \code{y} that are at least \code{percent} similar to
#' any string in \code{x}.
#'
#' @param x Character vector of query strings.
#' @param y Character vector of possible matches.
#' @param percent Minimum similarity, from 0 to 100. Use 100 for exact
#'   matches. The default is 50.
#' @details Similarity is 100 times one minus the Levenshtein edit distance
#'   divided by the longer string length. Swapping two adjacent letters
#'   counts as two edits. Two empty strings match exactly. Missing strings
#'   are ignored.
#' @return A sorted character vector of unique matches, or \code{character(0)}
#'   if none match.
#' @examples
#' find_similar("colour", c("color", "colour", "shape"), percent = 80)
#' @export
find_similar <- function(x, y, percent = 50) {
  if (!is.character(x) || !is.character(y)) stop("x and y must be character vectors.")
  if (!is.numeric(percent) || length(percent) != 1L || !is.finite(percent) ||
      percent < 0 || percent > 100) stop("percent must be between 0 and 100.")
  x <- unique(x[!is.na(x)])
  y <- unique(y[!is.na(y)])
  if (percent == 100) return(sort(intersect(x, y)))
  width <- nchar(y)
  matched <- rep(FALSE, length(y))
  for (query in x) {
    idx <- which(!matched)
    if (!length(idx)) break
    distance <- utils::adist(query, y[idx])[1L, ]
    matched[idx] <- 100 * distance <= (100 - percent) * pmax(nchar(query), width[idx])
  }
  sort(y[matched])
}
