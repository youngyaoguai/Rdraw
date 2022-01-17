#' Drawing a line picture with Printing style
#' Any BUG You found,please send email to 1270950448@qq.com(DU Yunhan,from Southwestern University of Finance & Economics)
#' @param input_data the original data(panel or time series data) (dataframe)
#' @param individual the individual variable of different region or something else (factor)
#' @param value value of target variable,like GDP¡¢CPI¡¢income.
#' @param time sequence,like year.
#' @param start start value of time
#' @param end end value of time
#' @param interval interval of time
#' @param axis.text.x.size size of x axis's text
#' @param axis.text.y.size size of y axis's text
#' @param legend.title.size size of title of legend
#' @param legend.text.size size of text of legend
#' @param axis.title.x.size size of title of x axis
#' @param axis.title.y.size size of title of y axis
#' @param axis.line.x.size thickness of x axis line
#' @param axis.line.y.size thickness of y axis line
#' @param geom.point.size size of point on line
#' @param ylab.name title of y axis
#' @param xlab.name title of x axis
#' @param individual.name name of individual
#' @param legend.name title of legend
#' @export
#' @examples
#' library(chip)
#' data("line_test_data",package = "chip")  #load integration sample data
#' start <- 2009
#' end <- 2019
#' interval <- 2
#' axis.text.x.size <- 30
#' axis.text.y.size <- 30
#' legend.title.size <- 30
#' legend.text.size <- 30
#' axis.title.x.size <- 30
#' axis.title.y.size <- 30
#' axis.line.x.size <- 0.5
#' axis.line.y.size <- 0.5
#' geom.point.size <- 7
#' ylab.name <- c("index")
#' xlab.name <- c("year")
#' individual.name <- c("A","B","C","D","E")
#' legend.name <- c("region")
#' linepoint(line_test_data,line_test_data$region,line_test_data$index,line_test_data$year)
linepoint <- function(input_data,individual,value,time){
  if (is.element('tidyverse', installed.packages()[,1]) == FALSE) {
    install.packages("tidyverse",type="binary")
  } else if (is.element('tidyverse', installed.packages()[,1]) == TRUE){
    library(tidyverse)
  }
  if (is.element('reshape', installed.packages()[,1]) == FALSE) {
    install.packages("reshape",type="binary")
  } else if (is.element('reshape', installed.packages()[,1]) == TRUE){
    library(reshape)
  }
  line_data <<- data.frame(matrix(NA,nrow(input_data),ncol(input_data)))
  line_data[,1] <<- as.factor(individual)
  line_data[,2] <<- as.numeric(value)  
  line_data[,3] <<- as.numeric(time)
  line_data$X1 <<- ordered(line_data$X1, levels = individual.name)
  line_data <<- line_data %>%
    rename(c(X1=legend.name))
  attach(line_data)
  gg <- ggplot(line_data, aes(x = X3, y = X2, group = .data[[legend.name]],linetype = .data[[legend.name]], shape = .data[[legend.name]])) + 
    geom_line() + 
    geom_point(size = geom.point.size) + 
    scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12))+ 
    theme(panel.background = element_rect(fill = NA)) +
    theme_set(theme_bw()) +
    theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
    ylab(ylab.name) +
    xlab(xlab.name) + 
    theme(axis.title.x = element_text(family = "A", size = axis.title.x.size),
          axis.title.y = element_text(family = "A", size = axis.title.y.size)) +
    theme(axis.line.x = element_line(color="black", size = axis.line.x.size), 
          axis.line.y = element_line(color="black", size = axis.line.y.size),
          panel.border = element_blank()) +
    theme(axis.text.x=element_text(vjust=1,size=axis.text.x.size))+
    theme(axis.text.y=element_text(vjust=1,size=axis.text.y.size))+
    theme(legend.title = element_text(size = legend.title.size,family = "A"),legend.text = element_text(size = legend.text.size,family = "A")) +
    scale_x_continuous(breaks=seq(start,end, interval))
  rm(line_data,envir=.GlobalEnv)
  rm(axis.line.x.size,envir=.GlobalEnv)
  rm(axis.line.y.size,envir=.GlobalEnv)
  rm(axis.text.x.size,envir=.GlobalEnv)
  rm(axis.text.y.size,envir=.GlobalEnv)
  rm(axis.title.x.size,envir=.GlobalEnv)
  rm(axis.title.y.size,envir=.GlobalEnv)
  rm(end,envir=.GlobalEnv)
  rm(geom.point.size,envir=.GlobalEnv)
  rm(individual.name,envir=.GlobalEnv)
  rm(interval,envir=.GlobalEnv)
  rm(legend.name,envir=.GlobalEnv)
  rm(legend.text.size,envir=.GlobalEnv)
  rm(legend.title.size,envir=.GlobalEnv)
  rm(start,envir=.GlobalEnv)
  rm(xlab.name,envir=.GlobalEnv)
  rm(ylab.name,envir=.GlobalEnv)
  gg
  dev.new()
  dev.new()
  gg
}