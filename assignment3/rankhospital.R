rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Read outcome data
  temp <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data<-data.frame(temp[,2],temp[,7],temp[,11],temp[,17],temp[,23])
  colnames(data) <- c("name","state", "heartattack", "heartfailure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!(state %in% data[, 2]))
  {
    stop("invalid state")
  } 
  
  index<-switch(outcome,                 
                "heart attack" = 3,
                "heart failure" = 4,  
                "pneumonia" = 5, 
                -1)
  if(index<0)
  {
    stop("invalid outcome")
  }  
  
  ## Return hospital name in that state with the given rank 30-day death rate
  data[,1] <- as.character(data[,1])
  data[,2] <- as.character(data[,2])
  data[,3] <- as.numeric(as.character(data[,3]))
  data[,4] <- as.numeric(as.character(data[,4]))
  data[,5] <- as.numeric(as.character(data[,5]))
  
  dataFiltered = data[data[,2]==state,]   
  colnames(dataFiltered) <- c("name","state", "heartattack", "heartfailure", "pneumonia")
  dataFiltered<- switch(index-2,
                        {                          
                          dataFiltered<-dataFiltered[complete.cases(dataFiltered$heartattack),]
                          dataFiltered[order(dataFiltered$heartattack, dataFiltered$name),]
                        }, 
                        {                          
                          dataFiltered<-dataFiltered[complete.cases(dataFiltered$heartfailure),]
                          dataFiltered[order(dataFiltered$heartfailure, dataFiltered$name),]
                        }, 
                        {                          
                          dataFiltered<-dataFiltered[complete.cases(dataFiltered$pneumonia),]
                          dataFiltered[order(dataFiltered$pneumonia, dataFiltered$name),]
                        })
  i<-NULL
  if(num>0)
  {
    i<-num
  }
  if(num=="best")
  {
    i<-1
  }
  if(num=="worst")
  {
    i<-nrow(dataFiltered)
  }
  dataFiltered[i,1]
}