#' Correlatrix
#'
#' Takes in a data.frame or imputationList, a vector of variable names and produces a correlation matrix
#' @param data an object of class 'data.frame' or 'imputationList'
#' @param var.names a vector of variable names
#' @param triangle a string containing one of "lower" "upper" or "both". Indicates if correlations are to be displayed above or below the diagonal. "Both" is selected by default.
#' @param round a numeral indicating number of decimals
#' @param method a string containing one of "pearson","spearman" or "kendall"
#' @param n.matrix logical. If TRUE, matrix of n returned
#' @param abbreviate a number indicating the maximum length of variable names
#' @examples carsdata<-mtcars
#' correlatrix(carsdata,names(carsdata)[1:6],round = 2)
#' correlatrix(carsdata,c("mpg","cyl","disp"))
#' @export
#' @importFrom stats na.omit cor.test
#' @importFrom miceadds micombine.cor
#' @return A correlation matrix

correlatrix <- function(data, var.names, triangle = "both", round = 3, method = "pearson", n.matrix = F,
                        abbreviate = 100){
##check arguments
        if(!(triangle %in% c("lower","upper","both"))){
                stop(paste("Check help file: triangle may not be equal to", triangle))
        }


        specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

        result.matrix<-matrix(ncol=length(var.names),nrow=length(var.names)*1)
        result.matrix<-data.frame(result.matrix)

   ###results
        for (c in 1:length(var.names)){#collumn loop start
                for (r in 1:length(var.names)){#row loop start{
                        ###result
                        if(c<r){
                        ##get result
                                if(class(data) == "data.frame"){
                                cor.result<-cor.test(as.numeric(data[,var.names[c]]),as.numeric(data[,var.names[r]]),method = method)
                                result<-cor.result$estimate
                                p<-cor.result$p.value }

                                if(class(data) == "imputationList"){
                                        cor.result<-micombine.cor(data,c(var.names[c],var.names[r]), method = method)
                                        result<-cor.result$r[1]
                                        p<-cor.result$p[1]}

                                result<-specify_decimal(result,round)
                        }else{
                          result = " "
                          p = 1
                        }
                        ### place result in data.frame
                        if(triangle == "upper"){
                                if(r<c){

                                        if(class(data) == "data.frame"){
                                                cor.result<-cor.test(as.numeric(data[,var.names[c]]),as.numeric(data[,var.names[r]]),method = method)
                                                result<-cor.result$estimate
                                                p<-cor.result$p.value }

                                        if(class(data) == "imputationList"){
                                                cor.result<-micombine.cor(data,c(var.names[c],var.names[r]), method = method)
                                                result<-cor.result$r[1]
                                                p<-cor.result$p[1]}

                                        result<-specify_decimal(result,round)
                                }else{
                                        result = " "
                                        p = 1
                                }
                        }

                        result.final<-result##add significance stars



                        if(p<0.05){
                                result.final<-paste(result,"*",sep="")}
                        if(p<0.01){
                                result.final<-paste(result,"**",sep="")}
                        if(p<0.001){
                                result.final<-paste(result,"***",sep="")}

                        if(n.matrix == T){
                                        if(class(data) == "data.frame"){
                                                cor.data<- data[,var.names]
                                        }else{
                                                cor.data<-data$imputations[[1]][,var.names]
                                        }

                        result.final <- length(na.omit(cor.data[,var.names[c]][is.na(cor.data[,var.names[r]])==F]))
                        }

                if(triangle == "lower"){
                        if(c<r){
                        result.matrix[r,c]<-result.final
                        }
                        if(c>r){
                                result.matrix[r,c]<-" "
                        }
                        names(result.matrix)<-1:length(var.names)
                        for(r in seq_along(var.names)){
                                row.names(result.matrix)[r] <- paste(r,".",abbreviate(var.names[r],abbreviate), sep = "")
                                }

                        }

                if(triangle == "upper"){
                        if(c>r){
                                result.matrix[r,c]<-result.final
                        }
                        if(c<r){
                                result.matrix[r,c]<-" "
                        }
                        names(result.matrix)<-1:length(var.names)
                        for(r in seq_along(var.names)){
                                row.names(result.matrix)[r] <- paste(r,".",abbreviate(var.names[r],abbreviate), sep = "")
                        }
                }

                if(triangle == "both"){
                        if(c<r){
                                result.matrix[r,c]<-result.final
                                result.matrix[c,r]<-result.final
                        }
                        ###collumn and row names
                        names(result.matrix) <- as.vector(lapply(var.names, function(x) abbreviate(x,abbreviate)))
                        row.names(result.matrix) <- as.vector(lapply(var.names, function(x) abbreviate(x,abbreviate)))
                }


                        #leave diagonal blank
                        for(b in 1:length(var.names)){
                                result.matrix[b,b]<-"-"
                        }#diagonal loop end
                }#row loop end
        }#collumn loop end

        if(n.matrix == T){
        message("n.matrix == T, therefore matrix of n returned.")
                }else{
        message("signif at 0.05*; signif at 0.01**; signif at 0.001***")
                }
        return(result.matrix)
}

