#' Drawing a box line picture with Printing style
#' Any BUG You found,please send email to 1270950448@qq.com(DU Yunhan,from Southwestern University of Finance & Economics)
#' @param input_data the original data(panel or time series data) (dataframe)
#' @param individual the classification of value you want to show distribution,like year,region.
#' @param ylab.name y axis title
#' @param xlab.name x axis title
#' @param axis.title.x.size size of x axis title
#' @param axis.title.y.size size of y axis title
#' @param axis.text.x.size size of x axis text
#' @param axis.text.y.size size of y axis text
#' @export
#' @examples
#' library(chip)
#' data("box_test_data",package = "chip")  #load integration sample data
#' ylab.name <- c("index")
#' xlab.name <- c("year")
#' axis.title.x.size <- 15
#' axis.title.y.size <- 15
#' axis.text.x.size <- 15
#' axis.text.y.size <- 15
#' boxeasy(box_test_data,box_test_data$year,box_test_data$index)
boxeasy <- function(input_data,individual,value){
  if (is.element('tidyverse', installed.packages()[,1]) == FALSE) {
    install.packages("tidyverse")
  } else if (is.element('tidyverse', installed.packages()[,1]) == TRUE){
    library(tidyverse)
  }
  box_data <<- data.frame(matrix(NA,nrow(input_data),ncol(input_data)))
  box_data[,1] <<- as.factor(individual)
  box_data[,2] <<- as.numeric(value)
  gg <- ggplot(box_data,aes(X1,X2)) + 
    geom_boxplot() +
    theme(panel.background = element_rect(fill = NA)) +
    theme_set(theme_bw()) +
    theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
    ylab(ylab.name) +
    xlab(xlab.name) + 
    theme(axis.title.x = element_text(family = "A", size = axis.title.x.size),
          axis.title.y = element_text(family = "A", size = axis.title.y.size)) +
    theme(axis.text.x=element_text(vjust=1,size=axis.text.x.size))+
    theme(axis.text.y=element_text(vjust=1,size=axis.text.y.size))+
    theme(axis.line.x = element_line(color="black", size = 0.5), 
          axis.line.y = element_line(color="black", size = 0.5),
          panel.border = element_blank()) +
    theme(legend.position = 'none') 
  rm(box_data,envir = .GlobalEnv)
  rm(axis.text.x.size,envir = .GlobalEnv)
  rm(axis.text.y.size,envir = .GlobalEnv)
  rm(axis.title.x.size,envir = .GlobalEnv)
  rm(axis.title.y.size,envir = .GlobalEnv)
  rm(xlab.name,envir = .GlobalEnv)
  rm(ylab.name,envir = .GlobalEnv)
  gg
  dev.new()
  dev.new()
  gg
}