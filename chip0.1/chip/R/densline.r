#' Drawing a Kernel density picture with Printing style
#' Any BUG You found,please send email to 1270950448@qq.com(DU Yunhan,from Southwestern University of Finance & Economics)
#' @param input_data the original data(panel or time series data) (dataframe)
#' @param xscope scope of x axis's value
#' @param yscope scope of y axis's value
#' @param legend.name names of every Kernal density line(input from the first column to the last column)
#' @export
#' @examples
#' library(chip)
#' data("densline_test_data",package = "chip")  #load integration sample data
#' yscope <- c(0.0004,0.00155) 
#' xscope <- c(0,1000)  
#' legend.name <- c("line1","line2","line3","line4","line5")
#' densline(densline_test_data)
#' BUT!!!!because you don't know the accurate scope,I suggest you set up the scope parameter like this:
#' yscope <- c()
#' xscope <- c()
#' legend.name <- c("line1","line2","line3","line4","line5")
#' densline(test_data)
#' After you get a drawing line,you can know about the scope,then you set up the scope parameter by your idea,and draw it again to adjust.
#' yscope <- c(0.0004,0.00155) 
#' xscope <- c(0,1000)  
#' legend.name <- c("line1","line2","line3","line4","line5")
#' densline(densline_test_data)
densline <- function(input_data){
  if (is.element('tidyverse', installed.packages()[,1]) == FALSE) {
    install.packages("tidyverse",type="binary")
  } else if (is.element('tidyverse', installed.packages()[,1]) == TRUE){
    library(tidyverse)
  }
  densline_data <<- data.frame(matrix(NA,nrow(input_data),ncol(input_data)))
  densline_data[1:ncol(densline_data)] <<- as.numeric(unlist(input_data[1:ncol(input_data)]))
  line.type <<- 1:ncol(densline_data)
  line.wide <<- 1:ncol(densline_data)
  plot(density(densline_data[[1]]),lty = 1,lwd = 1,ylim=yscope,xlim=xscope,xlab = " ",ylab = " ",main=" ") 
  for (i in 2:ncol(densline_data)){
    lines(density(densline_data[[i]]), lty = i,lwd = i)
  }
  legend("topright",legend = legend.name,lty = line.type,lwd = line.wide)
  dev.new()
  dev.new()
  plot(density(densline_data[[1]]),lty = 1,lwd = 1,ylim=yscope,xlim=xscope,xlab = " ",ylab = " ",main=" ") 
  for (i in 2:ncol(densline_data)){
    lines(density(densline_data[[i]]), lty = i,lwd = i)
  }
  legend("topright",legend = legend.name,lty = line.type,lwd = line.wide)
  rm(densline_data,envir = .GlobalEnv)
  rm(line.type,envir = .GlobalEnv)
  rm(line.wide,envir = .GlobalEnv)
  rm(legend.name,envir = .GlobalEnv)
  rm(yscope,envir = .GlobalEnv)
  rm(xscope,envir = .GlobalEnv)
}