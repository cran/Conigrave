#' check_names
#'
#' Takes in a vector of colnames, and a data.frame or imputationList. This function will trigger an error if names are not in the data object. In addition, this function will try to predict which names the user was trying to spell.
#' @param x a vector of colnames.
#' @param data a data.frame or imputationList.
#' @return check_names will trigger an error if the supplied vector of names were not found in the supplied object. It will also predict which names the user was trying to spell.
#' @export check_names
#' @importFrom stringdist stringdist
#' @importFrom dplyr %>%

check_names = function(x, data) {
  names = unique(x)
  if (any(c("amelia", "imputationList") %in% class(data))) {
    name_data = data$imputations[[1]]
  } else{
    name_data = data
  }
  
  if(!"character" %in% class(data)){
    name_data = names(name_data)
  } else{
    name_data = data
  }
  
  error_names = names[!names %in% name_data]
  likely_names = lapply(error_names, function(e) {
          distances = stringdist(e,name_data)
          likely_location = which.min(distances)
          if(distances[likely_location] < (4 + nchar(e))){
                name_data[likely_location]
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
