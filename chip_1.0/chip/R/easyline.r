#' Drawing a line picture with Printing style
#' Any BUG You found,please send email to 1270950448@qq.com(DU Yunhan,from Southwestern University of Finance & Economics)
#' @param easyline the original data(panel or time series data) (dataframe)
#' @param value the target value to show
#' @param time the sequence,like:1,2,3,4;or 1998,1999,2000.....
#' @param ylab.name title of y axis
#' @param xlab.name title of x axis
#' @param axis.title.x.size size of title of x axis
#' @param axis.title.y.size size of title of y axis
#' @param axis.line.x.thickness thickness of x axis
#' @param axis.line.y.thickness thickness of y axis
#' @param axis.text.x.size size of x axis's text
#' @param axis.text.y.size size of y axis's text
#' @param x.start start value of x axis's value
#' @param x.end end value of x axis's value
#' @param x.interval interval of x axis's value
#' @param y.start start value of y axis's value
#' @param y.end end value of y axis's value
#' @param y.interval interval of y axis's value
#' @param Fon the Font of Picture
#' @export
#' @examples
#' library(chip)
#' data("easyline_test_data",package = "chip")  #load integration sample data
#' ylab.name <- "population"
#' xlab.name <- "age"
#' axis.title.x.size <- 25
#' axis.title.y.size <- 25
#' axis.line.x.thickness <- 0.5
#' axis.line.y.thickness <- 0.5
#' axis.text.x.size <- 20
#' axis.text.y.size <- 20
#' x.start <- 0
#' x.end <- 100
#' x.interval <- 10
#' y.start <- 0
#' y.end <- 21571
#' y.interval <- 5000
#' Fon <- 'sans'
#' easyline(easyline_test_data,easyline_test_data$population,easyline_test_data$age)
easyline <- function(input_data,value,time){
 if (is.element('tidyverse', installed.packages()[,1]) == FALSE) {
    install.packages("tidyverse",type="binary")
  } else if (is.element('tidyverse', installed.packages()[,1]) == TRUE){
    library(tidyverse)
  }
  if (is.element('showtext', installed.packages()[,1]) == FALSE) {
    install.packages("showtext",type="binary")
  } else if (is.element('showtext', installed.packages()[,1]) == TRUE){
    library(showtext)
  }
 line_data <<- data.frame(matrix(NA,nrow(input_data),ncol(input_data)))
 line_data[,2] <<- as.numeric(value)  
 line_data[,3] <<- as.numeric(time)
  attach(line_data)
  gg <- ggplot(line_data, aes(x = X3, y = X2)) + 
  geom_line() + 
    theme(panel.background = element_rect(fill = NA)) +
  theme_set(theme_bw()) +
   theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  ylab(ylab.name) +
  xlab(xlab.name) + 
  theme(axis.title.x = element_text(family = Fon, size = axis.title.x.size),
          axis.title.y = element_text(family = Fon, size = axis.title.y.size)) +
      theme(axis.line.x = element_line(color="black", size = axis.line.x.thickness), 
             axis.line.y = element_line(color="black", size = axis.line.y.thickness),
           panel.border = element_blank()) +
      theme(axis.text.x=element_text(vjust=1,size=axis.text.x.size))+
      theme(axis.text.y=element_text(vjust=1,size=axis.text.y.size))+
      scale_x_continuous(breaks=seq(x.start,x.end,x.interval)) +
      scale_y_continuous(breaks=seq(y.start,y.end,y.interval))
  rm(line_data,envir = .GlobalEnv)
  rm(axis.line.x.thickness,envir = .GlobalEnv)
  rm(axis.line.y.thickness,envir = .GlobalEnv)
  rm(axis.text.x.size,envir = .GlobalEnv)
  rm(axis.text.y.size,envir = .GlobalEnv)
  rm(axis.title.x.size,envir = .GlobalEnv)
  rm(axis.title.y.size,envir = .GlobalEnv)
  rm(x.end,envir = .GlobalEnv)
  rm(x.interval,envir = .GlobalEnv)
  rm(x.start,envir = .GlobalEnv)
  rm(xlab.name,envir = .GlobalEnv)
  rm(y.start,envir = .GlobalEnv)
  rm(y.end,envir = .GlobalEnv)
  rm(y.interval,envir = .GlobalEnv)
  rm(ylab.name,envir = .GlobalEnv)
    gg
    dev.new()
    dev.new()
    gg
 }