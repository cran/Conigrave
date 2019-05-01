#' Correlatrix
#'
#' Takes in a dat#'a.frame or imputationList, a vector of variable names and produces a correlation matrix with customizable significance stars.
#' @aliases correlatrix
#' @param data a data.frame or imputationList.
#' @param x a vector of variable names to correlate (optional).
#' @param y a vector of column names for the creation of asymmetric correlation matrices.
#' @param triangle a string containing one of "lower" "upper" or "both". Indicates if correlations are to be displayed above or below the diagonal. "Both" is selected by default.
#' @param round a numeral indicating number of decimals.
#' @param method a string containing one of "pearson","spearman" or "kendall".
#' @param n.matrix logical. If TRUE, matrix of n returned.
#' @param abbreviate a number indicating the maximum length of variable names.
#' @param stars a numeric vector. For each numeral, a star will be assigned which indicates that the p-value for a given correlation was smaller than, that level. The default is 0.05, 0.01 and 0.001.
#' @param partial a vector of colnames. If supplied the function will output a matrix of partial correlations. All effects will be controlled for by the variables in this vector.
#' @param describe a list of functions with names or a logical. If functions are supplied to describe, a new column will be appended to the final data.frame for each argument in the list. If TRUE is supplied, means and standard deviation is appended with na.rm = T.
#' @param leading.zero a logical. If FALSE, leading zeros are removed.
#' @param ... the argument 'var.names' from previous versions has been deprecated, please use x instead.
#' @examples correlatrix(mtcars[,1:5])
#' library(dplyr)
#' mtcars %>%
#' ctrx(x = c("mpg","cyl","disp")
#' ,y = c("wt","drat"),
#' round = 2,
#' stars = c(0.05),
#' describe = list("mean" = function(x) mean(x,na.rm=TRUE)))
#' @export correlatrix
#' @export ctrx
#' @importFrom stats na.omit cor.test
#' @importFrom miceadds micombine.cor
#' @importFrom dplyr %>%
#' @importFrom mitools imputationList
#' @importFrom ppcor pcor.test
#' @return A data.frame containing a correlation matrix.

#TODO add n.matrix to getcor for partial correlations
 
ctrx =
  function(data,
           x = NULL,
           y = NULL,
           triangle = "both",
           round = 2,
           method = "pearson",
           n.matrix = F,
           abbreviate = 100,
           stars = c(0.05, 0.01, 0.001),
           partial = c(),
           describe = F,
           leading.zero = T,
           ...) {
 

    #check args
    
    if (!class(describe) %in% c("list", "logical")) {
      stop(
        "describe must be supplied a list of functions (with names), or a logical e.g. 'TRUE'",
        call. = F
      )
    }
    
    if (length(describe) < 1) {
      stop("describe must be given a list with at least one function in it.",
           call. = F)
    }
    if (class(leading.zero) != "logical") {
      stop("leading.zero must be supplied a logical", call. = F)
    }
    
    
    specify_decimal <-
      function(x, k)
        format(round(x, k), nsmall = k)
    .<-NULL #define global variables
    getCor = function(data,
                      x,
                      y,
                      method,
                      partial,
                      n.matrix) {
      ##function to provide correlation results
      if (x != y) {
        if (any(c("imputationList", "amelia") %in% class(data))) {
          if (!"imputationList" %in% class(data)) {
            data = imputationList(data$imputations)
          }
          if (length(partial) == 0) {
            result = micombine.cor(data,
                                   c(x, y),
                                   method = method)
            r = result$r[1]
            p = result$p[1]
            n = data$imputations[[1]][, c(x, y)] %>%
              na.omit() %>%
              nrow()
            
          } else{
            stop(
              "partial correlations are not currently supported for imputationLists",
              call. = F
            )
          }
          
        } else{
          x = make.names(x) #get cor requires conversion to data.frame, hence x and y are run through make.names
          y = make.names(y)
          
          #if is not imputation list
          if (length(partial) == 0) {
            #if not partial
            result = cor.test(as.numeric(data.frame(data)[, x]),
                              as.numeric(data.frame(data)[, y]),
                              method = method)
            r = result$estimate
            p = result$p.value
            n = as.numeric(result$parameter + 2)
          } else{
            #if partial
            partial_data = data[, c(x, y, partial)] %>%
              na.omit
            result = pcor.test(partial_data[, x],
                               partial_data[, y],
                               partial_data[, partial],
                               method = method)
            r = result$estimate
            p = result$p.value
            n = nrow(partial_data)
          }
        }
        r = specify_decimal(r, round)
        if (!is.na(p) & n.matrix == F) {
          r.final = r
          for (s in seq_along(stars)) {
            if (p < stars[s]) {
              r.final = paste0(r.final,
                               "*")
            }
          }
          r = r.final
        }
        
        if (n.matrix == T) {
          r = n
        }
        
        
      } else{
        r = "-"
      }
      
      
      return(r)
    } #end cor function
    if ("var.names" %in% names(list(...))) {
      x = list(...)$var.names
      warning("var.names is deprecated. Please use x instead.",
              call. = F)
    }
    extra_args = names(list(...))
    extra_args = extra_args[extra_args != "var.names"]
    if (length(extra_args) > 0) {
      warning(paste0("unused argument(s): "),
              paste0(extra_args, collapse = ", "),
              call. = F)
    }
    
    if (is.null(x)) {
      if (any(c("imputationList", "amelia") %in% class(data))) {
        x = names(data$imputations[[1]])
      } else{
        x = names(data)
      }
    }
    
    
    if (is.null(y)) {
      #if y is not set, it just equals x and defaults to a standard correlatrix
      y = x
    }
    
    check_names(x = c(x, y, partial), data = data)
    x = x[!x %in% partial] #remove partial from x
    y = y[!y %in% partial]
    ##check arguments
    if (!(triangle %in% c("lower", "upper", "both"))) {
      stop(paste("Check help file: triangle may not be equal to",
                 triangle))
    }
    
    matrix = data.frame(matrix(ncol = length(x), nrow = length(y)))#create empty results matrix
    row.names(matrix) = abbreviate(y, abbreviate) #label it
    names(matrix) = abbreviate(x, abbreviate) #...
    
    warn = c()
    for (r in seq_along(y)) {
      #print(paste0("r = ",r))
      #for every column
      for (c in seq_along(x)) {
        #print(paste0("c = ",c))
        #and every row
        
        matrix[r, c] = tryCatch({
          getCor(
            data,
            x[c],
            y[r],
            method = method,
            partial = partial,
            n.matrix = n.matrix
          )
        }, warning = function(w) {
          withCallingHandlers(suppressWarnings({
            warn <<- append(warn,
                            conditionMessage(w))
            return(getCor(
              data,
              x[c],
              y[r],
              method = method,
              partial = partial,
              n.matrix = n.matrix
            ))
          }))
          
        })
        
      }
      
    }
    
    if (identical(x, y)) {
      #only allow triangle settings if x == y
      
        if (triangle != "both") {
          #if triangle both is selected
          names(matrix) = 1:length(y) #if symmetrical get rid of col names
          rownames(matrix) = paste(1:length(x), ". ", x, sep =
                                     "") #put numbers in front of row.names
        }
      
      if (triangle == "upper") {
        #if triangle upper is selected
        for (c in seq_along(y)) {
          for (r in seq_along(x)) {
            if (r > c) {
              #when rows are larger than columns
              matrix[r, c] = "" #delete the results
            }
            
          }
          
        }
        matrix = matrix[,-1]
      }
      
      if (triangle == "lower") {
        # do the reverse if triangle lower is selected
        for (c in seq_along(y)) {
          for (r in seq_along(x)) {
            if (r < c) {
              matrix[r, c] = ""
            }
            
          }
          
        }
        matrix = matrix[,-(length(x))]
      }
      
     
    } else{
      if (triangle != "both") {
        warning("triangle settings only work when x is identical to y",
                call. = F)
      }
    }
    
    
    if (n.matrix == T) {

    } else{
      lapply(seq_along(stars), function(s) {
        paste0("p < ",
               stars[s],
               paste(rep(" *", s), collapse = ""))
      }) %>%
        unlist() %>%
        paste(collapse = "; ") %>%
        message()
      
      if (length(warn) > 0) {
        ties = which(warn == "Cannot compute exact p-value with ties")
        sd_zero = which(warn == "the standard deviation is zero")
        if (length(ties > 0)) {
          warning(
            paste0(
              length(ties),
              " correlations were affected by ties. For those correlations, exact p-values cannot be computed."
            ),
            call. = F
          )
        }
        if (length(sd_zero > 0)) {
          warning(
            paste0(
              length(sd_zero),
              " correlations could not be calculated as they involved a variable which had no variance."
            ),
            call. = F
          )
        }
        exclude = unique(c(ties, sd_zero))
        lapply(warn[-exclude], function(x) {
          warning(x, call. = F)
        })
      }
      
    }

    if (!leading.zero) {
      rownames = rownames(matrix)
      colnames = colnames(matrix)
      matrix = apply(matrix, 2, function(x)
        sub('^(-)?0[.]', '\\1.', x)) %>%
        data.frame()
      rownames(matrix) = rownames
      colnames(matrix) = colnames
    }
    
    #add in describe column.
    if (!identical(describe, F)) {
      if (identical(describe, T)) {
        describe = list(
          "M" = function(x)
            mean(x, na.rm = TRUE),
          "SD" = function(x)
            sd(x, na.rm = TRUE)
        )
      }
      if(!c("imputationList") %in% class(data)){
      data = data.frame(data)
      y = make.names(y)
      }
      for (i in seq_along(describe)) {
        matrix[[names(describe)[i]]] = lapply(y, function(x){
        paste0("with(data, describe[[i]]","(",x,"))") %>% 
          parse(text = .) %>% 
          eval %>% 
          unlist %>% 
          mean(na.rm=T) %>% 
          round(round)
      }) %>% unlist
      
    }
    }
    
    return(matrix)
  }

correlatrix <- ctrx
