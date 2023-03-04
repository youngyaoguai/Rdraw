#' Drawing a bonbon picture with Printing style
#' Any BUG You found,please send email to 1270950448@qq.com(DU Yunhan,from Southwestern University of Finance & Economics)
#' @param input_data the original data(panel or time series data) (dataframe)
#' @param individual the individual variable of different region or something else (factor)
#' @param value the target value of the individual,like NewYork's birthrate.(numeric)
#' @param outstanding the individual you want to mark,if don't have any,please type:   outstanding <- c(" ")
#' @param outstanding.label the label of individual you wish to mark,if don't have any,please type:   outstanding.label <- c(" ")
#' @param outstanding.label.size size of the label of individual you wish to mark
#' @param outstanding.size wide of the outstanding bar
#' @param not.outstanding.size wide of the bar you don't want to mark
#' @param outstanding.head.size size of Round head(just for outstanding bar)
#' @param not.outstanding.head.size size of Round head(just for the bar you don't want to mark)
#' @param axis.title.x.size size of the title of x axis 
#' @param axis.text.y.size size of the text of y axis
#' @param value.max max value of the sample you want to show
#' @param value.break the value you want to show in x axis
#' @param xlab.name the title of x axis
#' @param individual.name names of individual(drawing sequence is from bottom(first one) to top(last one))
#' @param Fon the Font of Picture
#' @export
#' @examples
#' library(chip)
#' data("bonbon_test_data",package = "chip")  #load integration sample data
#' outstanding <- c(" ") or like this:
#' outstanding <- c("wholesaleretail","Leasing and business services","realestate")
#' outstanding.label <- c("24.75%","28.51%","5.16%") #should be adjust with outstandingºÍoutstanding.label
#' outstanding.label.size <- 7
#' outstanding.size <- 4
#' not.outstanding.size <- 2.3
#' outstanding.head.size <- 7
#' not.outstanding.head.size <- 5
#' axis.title.x.size <- 30
#' axis.text.y.size <- 30
#' value.max <- 100
#' value.break <- c(0,value.max%/%3,78,100)
#' xlab.name <- c("ratio of service industry(%)")
#' individual.name <- c("Leasing and business services","wholesaleretail","scientificresearch","Accommodation and Catering","Informationtransmission","culture","socialwork","publicfacilities","residentsservice","financial","education","transportation","publicmanagement","realestate")
#' Fon <- 'sans'
#' bonbon(bonbon_test_data,bonbon_test_data$industry,bonbon_test_data$index)
bonbon <- function(input_data,individual,value){
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
  bonbon_data <<- data.frame(matrix(NA,nrow(input_data),ncol(input_data)))
  bonbon_data$case <<- individual
  bonbon_data[,1] <<- as.factor(individual)
  bonbon_data[,2] <<- as.numeric(value)  
  bonbon_data$X1 <<- bonbon_data$X1 %>%
    ordered(levels = individual.name)
  bonbon_data <<- bonbon_data %>%
    arrange(X1)
  bonbon_data$order <<- 1:nrow(bonbon_data)
  findorder.outstanding <<- rep(NA,times = length(outstanding))
  findvalue.outstanding <<- rep(NA,times = length(outstanding))
  for(j in 1:length(outstanding)){
    
    for(i in 1:length(bonbon_data$case)){
      
      if(outstanding[j] == bonbon_data$case[i]){
        
        findorder.outstanding[j] <<- i
        findvalue.outstanding[j] <<- bonbon_data$X2[i]
        
      }
      
    }
  }
  x <<- findorder.outstanding
  y <<- (findvalue.outstanding)*1.25
  
  p <- ggplot(bonbon_data, aes(x=X1, y=X2)) +
    geom_segment(aes(x=X1, xend=X1, y=0, yend=X2), color=ifelse(bonbon_data$X1 %in% outstanding, "orange", "grey"), size=ifelse(bonbon_data$X1 %in% outstanding,outstanding.size, not.outstanding.size)) +
    geom_point(color=ifelse(bonbon_data$X1 %in% outstanding, "orange", "grey"), size=ifelse(bonbon_data$X1 %in% outstanding, outstanding.head.size,not.outstanding.head.size) ) +
    theme_light() +
    coord_flip() +
    theme(
      legend.position="none",
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    theme(axis.title.x = element_text(family = Fon, size = axis.title.x.size),
          axis.title.y = element_text(family = Fon, size = 30)) +
    theme(axis.text.x=element_text(vjust=1,size=30))+
    theme(axis.text.y=element_text(vjust=0.5,size=axis.text.y.size))+
    theme(axis.line.x.bottom = element_line(colour = "black")) +
    theme(panel.background = element_rect(fill = NA)) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
    xlab("") +
    ylab(xlab.name) +
    scale_y_continuous(limits = c(0,value.max),breaks = value.break) +
    annotate("text", x = x,y = y,label = outstanding.label, color="black", size=outstanding.label.size, angle=0,fontface="bold", hjust=0)
  rm(bonbon_data,envir = .GlobalEnv)
  rm(y,envir = .GlobalEnv)
  rm(xlab.name,envir = .GlobalEnv)
  rm(x,envir = .GlobalEnv)
  rm(value.max,envir = .GlobalEnv)
  rm(value.break,envir = .GlobalEnv)
  rm(outstanding.size,envir = .GlobalEnv)
  rm(outstanding.label.size,envir = .GlobalEnv)
  rm(outstanding.label,envir = .GlobalEnv)
  rm(outstanding.head.size,envir = .GlobalEnv)
  rm(outstanding,envir = .GlobalEnv)
  rm(not.outstanding.size,envir = .GlobalEnv)
  rm(not.outstanding.head.size,envir = .GlobalEnv)
  rm(individual.name,envir = .GlobalEnv)
  rm(findvalue.outstanding,envir = .GlobalEnv)
  rm(findorder.outstanding,envir = .GlobalEnv)
  rm(axis.title.x.size,envir = .GlobalEnv)
  rm(axis.text.y.size,envir = .GlobalEnv)
  p
  dev.new()
  dev.new()
  p
}