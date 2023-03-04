#' Drawing a hist picture with Printing style
#' Any BUG You found,please send email to 1270950448@qq.com(DU Yunhan,from Southwestern University of Finance & Economics)
#' @param hist_data the original data(panel or time series data) (dataframe)
#' @param individual the individual variable of different region or something else (factor)
#' @param type Classification variables to indicate the type you care about of one of the individuals,like NewYork's  15-64/0-14/65+ age people. (factor)
#' @param value the value of the individual's index,like NewYork's birthrate.(numeric)
#' @param axisname.x name of x axis (character)
#' @param axisname.y name of y axis (character)
#' @param type.name name of type,like"girl_of_age15-64" (character)
#' @param individual.name name of individual,like "NewYork" (character)
#' @param legend.x distance of legend(horizontal) (numeric)
#' @param legend.y distance of legend(vertical) (numeric)
#' @param legend.x.distance distance of legend's graphic and legend's font(horizontal) (numeric)
#' @param x.size size of x axis's font (numeric)
#' @param y.size size of y axis's font (numeric)
#' @param lab.size size of the font of title of both x axis and y axis (numeric)
#' @export
#' @examples
#' library(chip)
#' data("hist_test_data",package = "chip")  #load integration sample data
#' x.size <- 2 
#' y.size <- 2 
#' lab.size <- 2 
#' legend.x <- 30 
#' legend.y <- 0.3 
#' legend.x.distance <- 0.1 
#' axisname.x <- c("region") 
#' axisname.y <- c("ratio") 
#' type.name = c("0-14","15-64","65+","immortality") 
#' individual.name=c("A","B","C","D","E") 
#' histeasy(hist_test_data,hist_test_data$region,hist_test_data$age,hist_test_data$ratio)
histeasy <- function(data,individual,type,value){
  if (is.element('tidyverse', installed.packages()[,1]) == FALSE) {
    install.packages("tidyverse",type="binary")
  } else if (is.element('tidyverse', installed.packages()[,1]) == TRUE){
    library(tidyverse)
  }
  hist_data <<- data.frame(matrix(NA,nrow(data),ncol(data)))
  hist_data[,1] <<- as.factor(individual)
  hist_data[,2] <<- as.factor(type)
  hist_data$X1 <<- ordered(hist_data$X1, levels = individual.name)
  hist_data$X2 <<- ordered(hist_data$X2, levels = type.name)
  hist_data[,3] <<- as.numeric(value)
  hist_wide_data <<- spread(hist_data,X2,X3)
  hist_wide_data <<- hist_wide_data %>%
    select(-X1)
  dev.new()
  par(mai=c(1,1,1,1.5))
  barplot(t(as.matrix(hist_wide_data)),beside=TRUE,bty="n",xlab = axisname.x,ylab = axisname.y,args.legend = list(x = legend.x,y = legend.y,cex=1.7,bty="n",x.intersp = legend.x.distance),cex = x.size,cex.axis = y.size,cex.lab = lab.size,legend.text = type.name,names.arg = individual.name)
  dev.new()
  par(mai=c(1,1,1,1.5))
  barplot(t(as.matrix(hist_wide_data)),beside=TRUE,bty="n",xlab = axisname.x,ylab = axisname.y,args.legend = list(x = legend.x,y = legend.y,cex=1.7,bty="n",x.intersp = legend.x.distance),cex = x.size,cex.axis = y.size,cex.lab = lab.size,legend.text = type.name,names.arg = individual.name)
  rm(hist_data,envir = .GlobalEnv)
  rm(hist_wide_data,envir = .GlobalEnv)
  rm(axisname.x,envir = .GlobalEnv)
  rm(axisname.y,envir = .GlobalEnv)
  rm(individual.name,envir = .GlobalEnv)
  rm(lab.size,envir = .GlobalEnv)
  rm(legend.x,envir = .GlobalEnv)
  rm(legend.x.distance,envir = .GlobalEnv)
  rm(legend.y,envir = .GlobalEnv)
  rm(type.name,envir = .GlobalEnv)
  rm(x.size,envir = .GlobalEnv)
  rm(y.size,envir = .GlobalEnv)
}

