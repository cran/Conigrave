#' check_names
#'
#' Takes in a vector of colnames, and a data.frame or imputationList. This funciton will trigger an error if names are not in the data object. In addition, this name will try to predict which names they were trying to spell.
#' @param x a vector of names.
#' @param data a data.frame or imputationList.
#' @importFrom stringdist stringdist
#' @importFrom magrittr %>%
#' @return check_names will trigger an error if the supplied vector of names are not found in the supplied object. It will also predict which names the user was trying to spell.

check_names = function(x, data) {
  #this function checks to see if names are missing
  names = unique(x)
  if (any(c("amelia", "imputationList") %in% class(data))) {
    name_data = data$imputations[[1]]
  } else{
    name_data = data
  }
  error_names = names[!names %in% names(name_data)]
  likely_names = lapply(error_names, function(e) {
          distances = stringdist(e,names(name_data))
          likely_location = which.min(distances)
          if(distances[likely_location] < 4){
                names(name_data)[likely_location]
          }else{
                  NA
          }
  }) %>% unlist()
  if (length(error_names) > 0) {
          if(length(error_names)==1){
                  stem = "name"
          } else{
                  stem = "names"
          }


    warning_m = paste0(
      length(error_names),
        " ",stem," could not be found: ",
        paste(error_names, collapse = ", "),".")
      if(length(na.omit(likely_names))>0){
             warning_m = paste0(warning_m, " Did you mean: ",
                                paste(likely_names, collapse = ", "),
                                "?")
      }


    stop(warning_m, call. = F)
  }
}
