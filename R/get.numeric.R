#' get.numeric
#'
#' Takes in a data.frame or imputationList, removes non-numeric columns, and returns the object.
#' @param x a data.frame or 'imputationList'
#' @return Returns the original object with all non-numeric columns removed.
#' @export get.numeric
#' @importFrom mitools imputationList

get.numeric = function(x) {
        if (!any(c("imputationList", "amelia") %in% class(x))) {
                cols = unlist(lapply(x, is.numeric))
                return(x[, cols])
        } else{
                if (!"imputationList" %in% class(x)) {
                        x = imputationList(x$imputations)
                }
                imps = lapply(x$imputations, function(y) {
                        cols = unlist(lapply(y, is.numeric))
                        return(y[, cols])
                })
                return(imputationList(imps))

        }
}
