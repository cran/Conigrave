#' Interaction plot
#'
#' Calculates a standardized two way or three way interaction and plots using ggplot2.
#' @param data an object of class 'data.frame' or 'imputationList'.
#' @param outcome a string with the name of the outcome variable.
#' @param predictor a string with the name of the predictor variable.
#' @param moderator a vector of the names of up to two moderating variables.
#' @param y.lim vector of numerals indicating y axis bounds.
#' @param x.lim vector of numerals indicating x axis bounds.
#' @param x.lab a string with the label of the x axis.
#' @param y.lab a string with the label of the y axis.
#' @param title a string containing title text.
#' @param title.size a numeral containing the font size of the title.
#' @param SDs a numeral indicating the standard deviations of the moderators.
#' @param legend.name a character string indicating the title of the legend.
#' @param colour a character string containing the colour of the data points.
#' @param show.points logical to determine whether or not to include points.
#' @param save logical as to whether or not to save the plot.
#' @param path string containing path of where to save plot. Defaults to working directory.

#' @examples carsdata<-mtcars
#' int.plot(carsdata,"mpg","disp","cyl", y.lim = c(-2.5,2.5))
#' int.plot(carsdata,"mpg","disp", c("cyl","am"), y.lim = c(-5.0,2.0))
#' @export
#' @import ggplot2
#' @importFrom mitools MIcombine imputationList
#' @importFrom stats cor.test lm na.omit
#' @return A ggplot

################################################################################
int.plot <- function(data, outcome, predictor, moderator, y.lim = c(-1,1),
                     x.lim=c(-1,1), x.lab = "auto", y.lab = "auto",title = "auto",
                     title.size = 15, SDs = 1,legend.name = "auto", colour = "ghostwhite",
                     show.points = FALSE, save = F, path = getwd()){
################################################################################
#TODO make output a list with summary and plot.

check_names(c(outcome,predictor,moderator),data = data)
###############
###INTERACTION SUMMARIES
 ###2 way interaction
        if(class(data) == "amelia"){
                data = imputationList(data$imputations)
        }

  if(length(moderator) == 1){
  #### standard data
  if("data.frame" %in% class(data) == T ){
  data<-as.data.frame(data)
        INT <- lm(scale(data[,outcome]) ~
                          scale(data[,moderator])*scale(data[,predictor]))

  names(INT$coefficients)[2]<-paste("scale(",moderator,")", sep = "")
  names(INT$coefficients)[3]<-paste("scale(",predictor,")", sep = "")
  names(INT$coefficients)[4]<-paste("scale(",moderator,")","*",
                                       "scale(",predictor,")", sep = "")

  print(summary(INT))
  }
  ####IF multiple imputation
  if(class(data) == "imputationList"){
      result.list<-list()
      for(i in seq_along(data[[1]])){
        result.list[[paste("imp",i,sep="")]] <- lm(scale(data$imputations[[i]][,outcome])~
                                                     scale(data$imputations[[i]][,moderator]) *
                                                     scale(data$imputations[[i]][,predictor]))
      }

      result <- MIcombine(result.list)
      names(result$coefficients)[2]<-paste("scale(",moderator,")", sep = "")
      names(result$coefficients)[3]<-paste("scale(",predictor,")", sep = "")
      names(result$coefficients)[4]<-paste("scale(",moderator,")","*",
                                           "scale(",predictor,")", sep = "")
      INT<-result

      }

        sum.int<-(summary(INT))
  }

 #######################
 ###3 way interaction
  if(length(moderator) == 2) {

    if("data.frame" %in% class(data) == T ){
      data<-as.data.frame(data)
      INT <- lm(scale(data[,outcome]) ~
                  scale(data[,moderator[1]])*scale(data[,moderator[2]])*scale(data[,predictor]))

      names(INT$coefficients)[2]<-paste("scale(",moderator[1],")", sep = "")
      names(INT$coefficients)[3]<-paste("scale(",moderator[2],")", sep = "")
      names(INT$coefficients)[4]<-paste("scale(",predictor,")", sep = "")
      names(INT$coefficients)[5]<-paste("scale(",moderator[1],")","*",
                                        "scale(",moderator[2],")", sep = "")
      names(INT$coefficients)[6]<-paste("scale(",moderator[1],")","*",
                                        "scale(",predictor,")", sep = "")
      names(INT$coefficients)[7]<-paste("scale(",moderator[2],")","*",
                                        "scale(",predictor,")", sep = "")
      names(INT$coefficients)[8]<-paste("scale(",moderator[1],")","*",
                                        "scale(",moderator[2],")","*",
                                        "scale(",predictor,")", sep = "")
      print(summary(INT))
    }

    ####IF mi.imputation
    if(class(data) == "imputationList"){
      result.list<-list()
      for(i in seq_along(data[[1]])){
        result.list[[paste("imp",i,sep="")]] <- lm(scale(data$imputations[[i]][,outcome])~
                                                     scale(data$imputations[[i]][,moderator[1]]) * scale(data$imputations[[i]][,moderator[2]])*
                                                     scale(data$imputations[[i]][,predictor]))
      }

      INT <- MIcombine(result.list)
      names(INT$coefficients)[2]<-paste("scale(",moderator[1],")", sep = "")
      names(INT$coefficients)[3]<-paste("scale(",moderator[2],")", sep = "")
      names(INT$coefficients)[4]<-paste("scale(",predictor,")", sep = "")
      names(INT$coefficients)[5]<-paste("scale(",moderator[1],")","*",
                                        "scale(",moderator[2],")", sep = "")
      names(INT$coefficients)[6]<-paste("scale(",moderator[1],")","*",
                                        "scale(",predictor,")", sep = "")
      names(INT$coefficients)[7]<-paste("scale(",moderator[2],")","*",
                                        "scale(",predictor,")", sep = "")
      names(INT$coefficients)[8]<-paste("scale(",moderator[1],")","*",
                                        "scale(",moderator[2],")","*",
                                        "scale(",predictor,")", sep = "")

      summary(INT)

    }

  }




        ##create dataset for ggplot
        colnames<-c(outcome, predictor, moderator)
        if(class(data) == "data.frame"){
        int.data<-cbind(data[,outcome], data[,predictor],data[,moderator])
        int.data<-as.data.frame(int.data)
        colnames(int.data)<-c("outcome","predictor","moderator")
        }else{
          int.data<-cbind(data$imputations[[1]][,outcome], data$imputations[[1]][,predictor],data$imputations[[1]][,moderator])
          int.data<-as.data.frame(int.data)
          colnames(int.data)<-c("outcome","predictor","moderator")
        }
        ######plot
temp.plot<-ggplot(int.data, aes(x = scale(predictor), y = scale(outcome),
                colour = scale(moderator)))

        ##add points if set to true
                if(show.points ==T){
                        temp.plot<-temp.plot +
                                geom_jitter(aes(size = scale(moderator)),width = 0.025) +
                           scale_color_gradient(limits = c(-2,2),low = "black", high = colour, guide= "none")
                }

                #linetypes
                if(length(moderator == 1)){
                positive<- paste("+",SDs,"SD",sep="")
                negative<- paste("-",SDs,"SD",sep="")
                }
                if(length(moderator == 2)){
                  mod1high.mod2high<- paste(substr(moderator[1],start = 1, stop = 4)," high","; ",substr(moderator[2],start = 1, stop = 4)," high",sep="")
                  mod1low.mod2high <- paste(substr(moderator[1],start = 1, stop = 4)," low","; ",substr(moderator[2],start = 1, stop = 4)," high",sep="")
                  mod1high.mod2low <- paste(substr(moderator[1],start = 1, stop = 4)," high","; ",substr(moderator[2],start = 1, stop = 4)," low",sep="")
                  mod1low.mod2low  <- paste(substr(moderator[1],start = 1, stop = 4)," low","; ",substr(moderator[2],start = 1, stop = 4)," low",sep="")
                }


                if(y.lab == "auto"){
                        y.lab <- outcome
                }
                if(x.lab == "auto"){
                        x.lab <- predictor
                }
                if(title == "auto"){
                        if(length(moderator) == 1){

                title<- paste(predictor, "*", moderator, " predicting ",outcome, sep="")
                        }else{
                                title<- paste(predictor, "*", moderator[1],"*",moderator[2], " predicting ",outcome, sep="")
                        }
                }

                if(length(moderator) == 1) {
                if(legend.name == "auto"){
                legend.name<- moderator
                  }
                  }

                if(length(moderator) == 2) {
                if(legend.name == "auto"){
                  legend.name<- " "
                  }
                  }

        temp.plot <- temp.plot +

          scale_x_continuous(limits = x.lim) +
          scale_y_continuous(limits = y.lim) +
          labs(x = x.lab, y = y.lab)+
          scale_size_continuous(name=moderator, guide = "none") +
          ggtitle(title)+ theme_classic() +
          theme(plot.title = element_text(size = title.size, face = "bold"))


        ###add lines for 2 way interaction
        if(length(moderator) == 1){

        temp.plot<-temp.plot +
          geom_abline(aes(intercept=(as.numeric(INT$coefficients[1]) + as.numeric(INT$coefficients[2]*SDs)),
                                    slope=(as.numeric(INT$coefficients[3]) + as.numeric(INT$coefficients[4]*SDs)),
                                    linetype= positive), size = 1)+
          geom_abline(aes(intercept=as.numeric(INT$coefficients[1]),
                          slope=as.numeric(INT$coefficients[3]),
                          linetype= "Mean"), size = 1)+
          geom_abline(aes(intercept=(as.numeric(INT$coefficients[1]) - as.numeric(INT$coefficients[2]*SDs)),
                          slope=(as.numeric(INT$coefficients[3])-as.numeric(INT$coefficients[4]*SDs)),
                          linetype= negative),size = 1)+
          scale_linetype_manual(values = c("dashed","solid","dotted"),
                                breaks =c(positive, "Mean", negative),
                                name = legend.name)
        }
        ###add lines for 3 way interaction
        if(length(moderator) == 2){

          temp.plot<-temp.plot + scale_linetype_manual(legend.name,
                                                      values = c("mod1high.mod2high" = "solid", "mod1high.mod2low" = "twodash","mod1low.mod2high" = "dashed", "mod1low.mod2low" = "dotted"),
                                                      labels = c(mod1high.mod2high,mod1high.mod2low,mod1low.mod2high,mod1low.mod2low)) +

            #mod1high,mod2high
            geom_abline(aes(intercept=(as.numeric(INT$coefficients[1]) + as.numeric(INT$coefficients[2]*SDs) + as.numeric(INT$coefficients[3]*SDs) + as.numeric(INT$coefficients[5]*SDs*SDs)),
                            slope=(as.numeric(INT$coefficients[4]) + as.numeric(INT$coefficients[6]*SDs) + as.numeric(INT$coefficients[7]*SDs) + as.numeric(INT$coefficients[8]*SDs*SDs)),
                            linetype= "mod1high.mod2high"), size = 1) +
            ##doling this one
            #mod1low.mod2high
            geom_abline(aes(intercept=(as.numeric(INT$coefficients[1]) - as.numeric(INT$coefficients[2]*SDs) + as.numeric(INT$coefficients[3]*SDs) - as.numeric(INT$coefficients[5]*SDs*SDs)),
                            slope=(as.numeric(INT$coefficients[4]) - as.numeric(INT$coefficients[6]*SDs) + as.numeric(INT$coefficients[7]*SDs) - as.numeric(INT$coefficients[8]*SDs*SDs)),
                            linetype= "mod1low.mod2high"), size = 1) +

            #mod1high.mod2low
            geom_abline(aes(intercept=(as.numeric(INT$coefficients[1]) + as.numeric(INT$coefficients[2]*SDs) - as.numeric(INT$coefficients[3]*SDs) - as.numeric(INT$coefficients[5]*SDs*SDs)),
                            slope=(as.numeric(INT$coefficients[4]) + as.numeric(INT$coefficients[6]*SDs) - as.numeric(INT$coefficients[7]*SDs) - as.numeric(INT$coefficients[8]*SDs*SDs)),
                            linetype= "mod1high.mod2low"), size = 1) +

            #mod1low.mod2low
            geom_abline(aes(intercept=(as.numeric(INT$coefficients[1]) - as.numeric(INT$coefficients[2]*SDs) - as.numeric(INT$coefficients[3]*SDs) + as.numeric(INT$coefficients[5]*SDs*SDs)),
                            slope=(as.numeric(INT$coefficients[4]) - as.numeric(INT$coefficients[6]*SDs) - as.numeric(INT$coefficients[7]*SDs) + as.numeric(INT$coefficients[8]*SDs*SDs)),
                            linetype= "mod1low.mod2low" ), size = 1 )

        }

        if(save == T){
          save<- paste(outcome,"~",predictor,"x",moderator[1],".jpg", sep ="")
          if(path == "desktop"){
          username<-Sys.getenv("USERNAME")
          path <- paste("C:/Users/",username,"/Desktop/",sep = "")
          }
          ggsave(save, path = path,
                 height = 11, width = 13.5, units = "cm", dpi = 600)
          message("plot saved:")
          message(path)
        }

        return(temp.plot)
}
################################################################################
