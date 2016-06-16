#' Correlatrix
#'
#' Takes in a data.frame or imputationList, a vector of collumn names and produces a correlation matrix
#' @param data an object of class 'data.frame' or 'imputationList'
#' @param var.names a vector of collumn names
#' @param round a numeral indicating number of decimals
#' @param method a string containing one of "pearson","spearman" or "kendall"
#' @param n.matrix logical. If TRUE, matrix of n returned
#' @param name.max a number indicating the maximum length of variable names
#' @examples carsdata<-mtcars
#' correlatrix(carsdata,names(carsdata),round = 2)
#' correlatrix(carsdata,c("mpg","cyl","disp"))
#' @export
#' @import stats
#' @importFrom miceadds micombine.cor
#' @return A correlation matrix

correlatrix <- function(data, var.names, round=3, method = "pearson", n.matrix = F,
                        name.max = 100){
        specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

        result.matrix<-matrix(ncol=length(var.names)+1,nrow=length(var.names)*1)
        result.matrix<-data.frame(result.matrix)
        names(result.matrix)<-c("Vars",1:length(var.names))
        ###create row names
        for(i in 1:length(var.names)){
                        result.matrix[i*+1,1]<-substr(var.names[i],1,name.max)
        }
        for (c in 1:length(var.names)){#collumn loop start
                for (r in 1:length(var.names)){#row loop start{
                        if(c<r){

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
                        result.final<-result##add significance stars



                        if(p<0.05){
                                result.final<-paste(result,"*",sep="")}
                        if(p<0.01){
                                result.final<-paste(result,"**",sep="")}

                        if(n.matrix == T){
                                        if(class(data) == "data.frame"){
                                                cor.data<- data[,var.names]
                                        }else{
                                                cor.data<-data$imputations[[1]][,var.names]
                                        }

                        result.final <- length(na.omit(cor.data[,var.names[c]][is.na(cor.data[,var.names[r]])==F]))
                                }

                        result.matrix[r,c+1]<-result.final
                        if(c>r){
                                result.matrix[r,c+1]<-" "
                        }

                        #leave diagonal blank
                        for(b in 1:length(var.names)){
                                result.matrix[b,b+1]<-"-"
                        }#diagonal loop end
                }#row loop end
        }#collumn loop end

        if(n.matrix == T){
        message("n.matrix == T, therefore matrix of n returned.")
                }else{
        message("signif at 0.05*; signif at 0.01**")
                }
        return(result.matrix)
}

