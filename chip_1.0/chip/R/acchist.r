#' Drawing a accummulate hist picture with Printing style
#' Any BUG You found,please send email to 1270950448@qq.com(DU Yunhan,from Southwestern University of Finance & Economics)
#' @param achist_data the original data(panel or time series data) (dataframe)
#' @param individual the individual variable of different region or something else (factor)
#' @param individual.name the names of the individual of different region or something else (charactor)
#' @param value the value of the individual's index,like NewYork's birthrate.(numeric)
#' @param fill Classification variables to indicate the type you care about of one of the individuals,like NewYork's 15-64/0-14/65+ age people. (factor)
#' @param fill.name the names of  Classification variables to indicate the type you care about of one of the individuals,like NewYork's 15-64/0-14/65+ age people. (charactor)
#' @param bar_width the wide of the bar.(numeric)
#' @param axis.title.size the size of both x and y axis's title.(numeric)
#' @param axis.x.title the name of x axis.(charactor)
#' @param axis.y.title the name of y axis.(charactor)
#' @param axis.text.x.size the size of x axis's text.(numeric)
#' @param axis.text.y.size the size of y axis's text.(numeric)
#' @param legend.text.size the size of legend's text.(numeric)
#' @param legend.title.size the size of legend's title.(numeric)
#' @param Fon the Font of Picture
#' @export
#' @examples
#' library(chip)
#' data("acchist_test_data",package = "chip")  #load integration sample data
#' axis.x.title <- c("region")
#' axis.y.title <- c("ratio")
#' individual.name <- c("B","C","D","E","F")
#' fill.name <- c("academic","vocational","college","high","further","primary")
#' bar_width <- 0.3
#' axis.title.size <- 30
#' axis.text.x.size <- 30
#' axis.text.y.size <- 30
#' legend.text.size <- 30
#' legend.title.size <- 30
#' Fon <- 'sans'
#' acchist(acchist_test_data,acchist_test_data$region,acchist_test_data$edu,acchist_test_data$ratio)
acchist <- function(achist_data,individual,fill,value){
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
  acchist_data <<- data.frame(matrix(NA,nrow(achist_data),ncol(achist_data)))
  acchist_data[,1] <<- as.factor(individual)
  acchist_data[,2] <<- as.factor(fill)
  acchist_data$X1 <<- ordered(acchist_data$X1, levels = individual.name)
  acchist_data$X2 <<- ordered(acchist_data$X2, levels = fill.name)
  acchist_data[,3] <<- as.numeric(value)
  acchist_plot <- ggplot(acchist_data,aes(x = X1,y = X3, fill = X2)) +
    geom_bar(stat = "identity", colour = "black",width = bar_width)+
    guides(fill = guide_legend(reverse = F)) + 
    theme_bw() +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
    theme(panel.border = element_blank()) +
    theme(axis.line = element_line(colour = "black")) +
    xlab(axis.x.title) +
    ylab(axis.y.title) +
    theme(axis.title = element_text(size=axis.title.size), # 调整横纵坐标轴标题字号
          axis.text.x = element_text(size=axis.text.x.size),
          axis.text.y = element_text(size=axis.text.y.size))+  # 调整横坐标文字字号
    scale_fill_brewer(palette = "Greys") +
    theme(legend.text = element_text(family = Fon,size = legend.text.size),legend.title = element_text(family = Fon,size = legend.title.size))
  rm(acchist_data,envir = .GlobalEnv)
  rm(axis.text.x.size,envir = .GlobalEnv)
  rm(axis.text.y.size,envir = .GlobalEnv)
  rm(axis.title.size,envir = .GlobalEnv)
  rm(axis.x.title,envir = .GlobalEnv)
  rm(axis.y.title,envir = .GlobalEnv)
  rm(bar_width,envir = .GlobalEnv)
  rm(fill.name,envir = .GlobalEnv)
  rm(individual.name,envir = .GlobalEnv)
  rm(legend.text.size,envir = .GlobalEnv)
  rm(legend.title.size,envir = .GlobalEnv)
  acchist_plot
  dev.new()
  dev.new()
  acchist_plot
}



