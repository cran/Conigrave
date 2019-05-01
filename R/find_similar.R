#' find_similar
#'
#' Takes in two vectors of strings. This function will return strings in the second set, which are similar to those in the first.
#' @param x a vector of strings.
#' @param y a vector of strings. Strings will be returned from y which are similar to those in x
#' @param percent a numeric. Strings in y will be returned if they are at least this percent similar. Defaults to 50\%.
#' @return a vector containing strings from y, which are similar to those in x.
#' @export find_similar
#' @importFrom stringdist stringdist
#' @importFrom dplyr %>%

find_similar = function(x, y, percent = 50){
  results = lapply(x, function(i) {
    threshold = nchar(i)*(percent/100)
    if(threshold<1){
      threshold =1
    }
    y[stringdist(i, y) <= threshold]
  })  %>%  unlist
  results[order(results)] %>%
    unique %>%
    return()
}
