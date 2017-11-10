library(jsonlite)
library(ggplot2)

mycsv <- read.csv("estc_processed_080517.csv")

cleaner <- function(id) {
  return(gsub("^([A-Z])0*([1-9][0-9]*)$","\\1\\2",id))
}
 
#Createdatawithyears takes scored paragraphs as input and returns them with publication years
Createdatawithyears <- function(data){
  publication_year <- mycsv[match(sapply(data$ESTCID, cleaner), mycsv$id), 'publication_year']  
  return(cbind(data,publication_year))
}
#Giverelativescore_scv takes output of the Createdatawithyears and gives yearly averages from 1700-1799 as a csv file 
Giverelativescore_csv <- function(data,name){
  datarightyears <- subset(data,data$publication_year >=1700 & data$publication_year<1800)  
  paragraphsperyr <- integer(100)
  emotionsperyear <- integer(100)
  for(i in 1700:1799){
    rightparagrahphs <- subset(datarightyears, datarightyears$publication_year==i)
    paragraphsperyr[i-1699] <- length(rightparagrahphs$score)  
    emotionsfromtheyear <- rightparagrahphs$emotionality
    emotionsperyear[i-1699] <- sum(emotionsfromtheyear)
  }
  years <- seq(from=1700, to=1799,by=1)
  relativescore <- emotionsperyear/paragraphsperyr
  dataforcsv <- data.frame(relativescore,years)
  write.csv(x=dataforcsv,file=name)
}


#The following visualizers take csv-files (parameter data) of yearly average genre scores per-year.
#. Leters note the words from which the data
#is counted (first data corresponds to first letter etc.) genre is simply
#the genre being measured. Visualizer is done separatel for 1-4 words, R
#is a bit clumsy for making a visualizer that is flexible by the number of
#data sets.


Visualize<- function(data1,a,genre){
  years <- seq(from=1700,to=1799,by=1)
  combined_data <- data.frame(data1,Dates=years)          
  stacked <- with(combined_data,
                  data.frame(value = c(data1),
                             variable = factor(rep(c(a),
                                                   each = NROW(combined_data))),
                             Dates = rep(Dates, 2)))
  require(ggplot2)
  p <- ggplot(stacked, aes(Dates, value, colour = variable))
  p +
    #geom_line(size=1.2) +
    scale_x_continuous("Timeline") +
    scale_y_continuous(genre) + 
    geom_smooth(se=FALSE,size=2.0) +
    theme(text = element_text(size=18))
  
}
Visualize2<- function(data1,data2,a,b,genre){
  years <- seq(from=1700,to=1799,by=1)
  combined_data <- data.frame(data1,data2,Dates=years)          
  stacked <- with(combined_data,
                  data.frame(value = c(data1,data2),
                             variable = factor(rep(c(a,b),
                                                   each = NROW(combined_data))),
                             Dates = rep(Dates, 2)))
  require(ggplot2)
  p <- ggplot(stacked, aes(Dates, value, colour = variable))
  p +
    #geom_line(size=1.2) +
    scale_x_continuous("Timeline") +
    scale_y_continuous(genre) + 
    geom_smooth(se=FALSE,size=2.0) +
    theme(text = element_text(size=18))
  
}
Visualize3 <- function(data1,data2,data3,b,c,genre){
  years <- seq(from=1700,to=1799,by=1)
  combined_data <- data.frame(data1,data2,data3,Dates=years)          
  stacked <- with(combined_data,
                  data.frame(value = c(data1, data2,data3),
                             variable = factor(rep(c(a,b,c),
                                                   each = NROW(combined_data))),
                             Dates = rep(Dates, 2)))
  require(ggplot2)
  p <- ggplot(stacked, aes(Dates, value, colour = variable))
  p +
    #geom_line(size=1.2) +
    scale_x_continuous("Timeline") +
    scale_y_continuous(genre) + 
    geom_smooth(se=FALSE,size=2.0) +
    theme(text = element_text(size=18))
  
}


Visualize4 <- function(data1,data2,data3,data4,a,b,c,d,genre){
  years <- seq(from=1700,to=1799,by=1)
  combined_data <- data.frame(data1,data2,data3,data4,Dates=years)          
  stacked <- with(combined_data,
                  data.frame(value = c(data1, data2,data3,data4),
                             variable = factor(rep(c(a,b,c,d),
                                                   each = NROW(combined_data))),
                             Dates = rep(Dates, 2)))
  require(ggplot2)
  p <- ggplot(stacked, aes(Dates, value, colour = variable))
  p +
    #geom_line(size=1.2) +
    scale_x_continuous("Timeline") +
    scale_y_continuous(genre) + 
    geom_smooth(se=FALSE,size=2.0) +
    theme(text = element_text(size=18))
  
}



  
  



  
