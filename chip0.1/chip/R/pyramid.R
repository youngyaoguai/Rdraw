#' Drawing a pyramid picture with Printing style
#' Any BUG You found,please send email to 1270950448@qq.com(DU Yunhan,from Southwestern University of Finance & Economics)
#' @param data the original data(panel or time series data) (dataframe)
#' @param left variable which you want to put in leftside
#' @param right variable which you want to put in rightside
#' @param groupID squence of group
#' @param groupName name of each group
#' @param v_max the max value of all variable
#' @param middle.size size of text in middle
#' @param left.axis.title.x.size size of left of x axis title
#' @param left.axis.text.x.size size of left of x axis text
#' @param right.axis.title.x.size size of right of x axis title
#' @param right.axis.text.x.size size of right of x axis text
#' @export
#' @examples
#' data("pyramid_test_data",package = "chip")  #load integration sample data
#' v_max <- 4400
#' middle.size <- 6.6
#' left.axis.title.x.size <- 30
#' left.axis.text.x.size <- 30
#' right.axis.title.x.size <- 30
#' right.axis.text.x.size <- 30
#' pyramid(pyramid_test_data,pyramid_test_data$male,pyramid_test_data$female,pyramid_test_data$group,pyramid_test_data$group_name)
pyramid <- function(data,left,right,groupID,groupName)
{
  if (is.element('tidyverse', installed.packages()[,1]) == FALSE) {
    install.packages("tidyverse",type="binary")
  } else if (is.element('tidyverse', installed.packages()[,1]) == TRUE){
    library(tidyverse)
  }
  if (is.element('grid', installed.packages()[,1]) == FALSE) {
    install.packages("grid",type="binary")
  } else if (is.element('grid', installed.packages()[,1]) == TRUE){
    library(grid)
  }
  test_data <<- data.frame(matrix(NA,nrow(data),ncol(data)))
  test_data[,1] <<- as.numeric(left)
  test_data[,2] <<- as.numeric(right)  
  test_data[,3] <<- as.numeric(groupID)
  test_data[,4] <<- as.character(groupName)
  test_data$X1 <<- test_data$X1/(-1)
  vplayout <- function(x, y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  text_x <<- rep(2, nrow(data))
  text_y <<- 1:nrow(data)
  text_lab <<- groupName
  title_x <<- 2
  title_y <<- 2
  title_lab <<- ' '
  dig_temp <<- nchar(as.character(v_max))
  lim_1 <<- c(-v_max, 0)
  lim_2 <<- c(0, v_max)
  by <- round(v_max/(5 * 10^(dig_temp - 2))) * 10^(dig_temp - 2)
  bre_1 <<- seq(from = 0, to = -v_max, by = -by)
  bre_2 <<- seq(from = 0, to = v_max, by = by)
  lab_1 <<- seq(from = 0, to = v_max, by = by)
  lab_2 <<- seq(from = 0, to = v_max, by = by)
  mg_1 <<- unit(c(0, 0.0, 0.3, 0.5), "lines")  # 上右下左
  mg_2 <<- unit(c(0, 0.5, 0.3, 0.0), "lines")
  mg_3 <<- unit(c(0, 0.0, 2.3, 0.0), "lines")
  mg_l <<- margin(0, 0, 0, 0, 'lines')
  p_1 <- ggplot(test_data) +
    geom_bar(aes(X3, X1),fill = 'grey',colour = "black",stat="identity", position="dodge") +
    scale_y_continuous(limits = lim_1, breaks = bre_1, labels = lab_1) +
    scale_x_continuous(limits = c(0, (nrow(test_data) + 1)), breaks = 1:nrow(test_data), labels = NULL, expand = expand_scale(), position = 'top') +
    theme(axis.title.x = element_text(family = "A", size = left.axis.title.x.size),
          axis.title.y = element_text(family = "A", size = 30)) +
    theme(axis.text.x=element_text(vjust=1,size=left.axis.text.x.size))+
    theme(axis.text.y=element_text(vjust=1,size=30))+
    theme(plot.margin = mg_1, axis.text = element_text(margin = mg_l),panel.background = element_blank()) +
    xlab(NULL) +
    ylab("male") +
    coord_flip() + 
    guides(fill = FALSE)
  
  p_2 <- ggplot(test_data) +
    geom_bar(aes(X3, X2), fill = "grey", colour = "black", stat="identity", position="dodge") +
    scale_y_continuous(limits = lim_2, breaks = bre_2, labels = lab_2) +
    scale_x_continuous(limits = c(0, (nrow(test_data) + 1)), breaks = 1:nrow(test_data), labels = NULL, expand = expand_scale()) +
    theme(axis.title.x = element_text(family = "A", size = right.axis.title.x.size),
          axis.title.y = element_text(family = "A", size = 30)) +
    theme(axis.text.x=element_text(vjust=1,size=right.axis.text.x.size))+
    theme(axis.text.y=element_text(vjust=1,size=30))+
    theme(plot.margin = mg_2, axis.text = element_text(margin = mg_l),panel.background = element_blank()) +
    xlab(NULL) +
    ylab("female") +
    coord_flip() +
    guides(fill = FALSE)
  
  p_3 <- ggplot() + 
    geom_text(aes(x = text_x, y= text_y, label = text_lab), size = middle.size) +
    scale_x_continuous(limits = c(0, 4), breaks = NULL, expand = expand_scale()) +
    scale_y_continuous(limits = c(0, (nrow(test_data) + 1)), breaks = NULL, expand = expand_scale()) +
    labs(x = NULL, y = NULL) + 
    theme(plot.margin = mg_3,
          axis.text = element_text(margin = mg_l),
          panel.grid.major =element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  
  p_4 <- ggplot() + 
    geom_text(aes(x = title_x, y= title_y, label = title_lab), size = 6) +
    scale_x_continuous(limits = c(0, 4), breaks = NULL, expand = expand_scale()) +
    scale_y_continuous(limits = c(0, 4), breaks = NULL, expand = expand_scale()) +
    labs(x = NULL, y = NULL) + 
    theme(plot.margin = mg_l,
          axis.text = element_text(margin = mg_l),
          panel.grid.major =element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  grid.newpage()  ##新建页面
  pushViewport(viewport(layout = grid.layout(12, 11))) 
  print(p_1, vp = vplayout(2:12, 1:5))
  print(p_3, vp = vplayout(2:12, 6))
  print(p_2, vp = vplayout(2:12, 7:11))
  print(p_4, vp = vplayout(1, 1:11))
  dev.new()
  dev.new()
  grid.newpage()  ##新建页面
  pushViewport(viewport(layout = grid.layout(12, 11))) 
  print(p_1, vp = vplayout(2:12, 1:5))
  print(p_3, vp = vplayout(2:12, 6))
  print(p_2, vp = vplayout(2:12, 7:11))
  print(p_4, vp = vplayout(1, 1:11))
  rm(test_data,envir = .GlobalEnv)
  rm(bre_1,envir = .GlobalEnv)
  rm(bre_2,envir = .GlobalEnv)
  rm(dig_temp,envir = .GlobalEnv)
  rm(lab_1,envir = .GlobalEnv)
  rm(lab_2,envir = .GlobalEnv)
  rm(left.axis.text.x.size,envir = .GlobalEnv)
  rm(left.axis.title.x.size,envir = .GlobalEnv)
  rm(lim_1,envir = .GlobalEnv)
  rm(lim_2,envir = .GlobalEnv)
  rm(mg_1,envir = .GlobalEnv)
  rm(mg_2,envir = .GlobalEnv)
  rm(mg_3,envir = .GlobalEnv)
  rm(mg_l,envir = .GlobalEnv)
  rm(middle.size,envir = .GlobalEnv)
  rm(right.axis.text.x.size,envir = .GlobalEnv)
  rm(right.axis.title.x.size,envir = .GlobalEnv)
  rm(text_lab,envir = .GlobalEnv)
  rm(text_x,envir = .GlobalEnv)
  rm(text_y,envir = .GlobalEnv)
  rm(title_lab,envir = .GlobalEnv)
  rm(title_x,envir = .GlobalEnv)
  rm(title_y,envir = .GlobalEnv)
  rm(v_max,envir = .GlobalEnv)
}



