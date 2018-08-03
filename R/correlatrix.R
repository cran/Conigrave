#' Correlatrix
#'
#' Takes in a data.frame or imputationList, a vector of variable names and produces a correlation matrix with customizable significance stars.
#' @param data a data.frame or imputationList.
#' @param x a vector of variable names to correlate (optional).
#' @param y a vector of column names for the creation of asymmetric correlation matrices.
#' @param triangle a string containing one of "lower" "upper" or "both". Indicates if correlations are to be displayed above or below the diagonal. "Both" is selected by default.
#' @param round a numeral indicating number of decimals.
#' @param method a string containing one of "pearson","spearman" or "kendall".
#' @param n.matrix logical. If TRUE, matrix of n returned.
#' @param abbreviate a number indicating the maximum length of variable names.
#' @param stars a numeric vector. For each numeral, a star will be assigned which indicates that the p-value for a given correlation was at, or smaller than, that level. The default is 0.05, 0.01 and 0.001.
#' @param ... the argument 'var.names' from previous versions has been deprecated, please use x instead.
#' @examples correlatrix(mtcars[,1:5])
#' library(magrittr)
#' mtcars %>%
#' correlatrix(x = c("mpg","cyl","disp")
#' ,y = c("wt","drat"),
#' round = 2,
#' stars = c(0.05))
#' @export correlatrix
#' @importFrom stats na.omit cor.test
#' @importFrom miceadds micombine.cor
#' @importFrom magrittr %>%
#' @importFrom mitools imputationList
#' @return A data.frame containing a correlation matrix.

correlatrix <-
        function(data,
                 x = NULL,
                 y = NULL,
                 triangle = "both",
                 round = 3,
                 method = "pearson",
                 n.matrix = F,
                 abbreviate = 100,
                 stars = c(0.05,0.01,0.001),
                 ...) {

                specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

                getCor = function(data,x,y){ ##function to provide correlation results

                        if(x != y){

                                if(any(c("imputationList","amelia")%in%class(data))){
                                        if(!"imputationList" %in% class(data)){
                                                data = imputationList(data$imputations)
                                        }
                                        result = micombine.cor(data,c(x,y))
                                        r = result$r[1]
                                        p = result$p[1]
                                } else{
                                        result = cor.test(as.numeric(data.frame(data)[,x]), as.numeric(data.frame(data)[,y]),method = method)
                                        r = result$estimate
                                        p = result$p.value
                                }
                                r = specify_decimal(r,round)
                                if(!is.na(p)){
                                        r.final = r
                                        for(s in seq_along(stars)){
                                                if(p <= stars[s]){
                                                        r.final = paste0(r.final,"*")
                                                }
                                        }
                                        r = r.final
                                }

                        } else{
                                r = "-"
                        }

                        if (n.matrix == T){
                                if (any(class(data) %in% c("data.frame"))) {
                                        cor.data <- data
                                } else{
                                        cor.data <- data$imputations[[1]][, x]
                                }

                                r <- nrow(na.omit(cor.data[,c(x,y)]))
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
                if(length(extra_args) > 0){
                        warning(paste0("unused argument(s): "),
                                paste0(extra_args,collapse = ", "), call. = F)
                }

                if(is.null(x)){
                        if(any(c("imputationList","amelia") %in% class(data))){
                                x = names(data$imputations[[1]])
                        }else{
                        x = names(data)
                        }
                }
                if(is.null(y)){ #if y is not set, it just equals x and defaults to a standard correlatrix
                        y = x
                }

                check_names(x = c(x,y), data = data)

                ##check arguments
                if (!(triangle %in% c("lower", "upper", "both"))) {
                        stop(paste(
                                "Check help file: triangle may not be equal to",
                                triangle
                        ))
                }

                matrix = data.frame(matrix(ncol = length(x),nrow=length(y)))#create empty results matrix
                row.names(matrix) = abbreviate(y,abbreviate) #label it
                names(matrix) = abbreviate(x,abbreviate) #...

                warn = c()
                for(r in seq_along(y)){#for every column
                        for(c in seq_along(x)){ #and every row

                                matrix[r,c]=tryCatch({
                                        getCor(data,x[c],y[r])
                                },warning=function(w) {
                                        withCallingHandlers(suppressWarnings({
                                                warn <<- append(warn, conditionMessage(w))
                                                return(getCor(data,x[c],y[r]))
                                        }))

                                })

                                }

                }

                if(identical(x,y)){ #only allow triangle settings if x == y

                        if(triangle == "upper"){ #if triangle upper is selected
                                for(c in seq_along(y)){
                                        for(r in seq_along(x)){
                                                if(r>c){ #when rows are larger than columns
                                                        matrix[r,c] = "" #delete the results
                                                }

                                        }

                                }

                        }

                        if(triangle == "lower"){# do the reverse if triangle lower is selected
                                for(c in seq_along(y)){
                                        for(r in seq_along(x)){
                                                if(r<c){
                                                        matrix[r,c] = ""
                                                }

                                        }

                                }

                        }

                        if(triangle != "both"){ #if triangle both is selected
                                names(matrix) = 1:length(y) #if symmetrical get rid of col names
                                rownames(matrix) = paste(1:length(x),".",x,sep="") #put numbers in front of row.names
                        }
                } else{
                        if (triangle != "both"){
                                warning("triangle settings only work when x is identical to y",call. = F)
                        }
                }


                if (n.matrix == T) {
                        message("matrix of n returned.")
                } else{
                        lapply(seq_along(stars), function(s){
                                paste0("signif at ",stars[s],paste(rep("*",s),collapse = ""))
                        }) %>%
                                unlist() %>%
                                paste(collapse = "; ") %>%
                                message()

                        if(length(warn) > 0){

                                ties = which(warn == "Cannot compute exact p-value with ties")
                                sd_zero = which(warn == "the standard deviation is zero")
                                if(length(ties > 0)){
                                warning(paste0(length(ties)," correlations were affected by ties. For those correlations, exact p-values cannot be computed."),call.=F)
                                }
                                if(length(sd_zero>0)){
                                warning(paste0(length(sd_zero)," correlations could not be calculated as they involved a variable which had no variance."),call. = F)
                                }
                                exclude = unique(c(ties,sd_zero))
                                lapply(warn[-exclude],function(x){
                                       warning(x,call.=F)
                                       })
                        }

                }
                return(matrix)
        }

