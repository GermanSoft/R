best <- function(state, outcome) {
  ## Read outcome data
  temp <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data<-data.frame(temp[,2],temp[,7],temp[,11],temp[,17],temp[,23])
    
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
  
  ## Return hospital name in that state with lowest 30-day death rate
  data[,index] <- as.numeric(as.character(data[,index]))
  dataFiltered = data[data[,2]==state,]  
  min <- min(dataFiltered[,index], na.rm = TRUE)  
  res<-dataFiltered[dataFiltered[,index]==min,]
  res<-res[complete.cases(res),]submit
  res3<-as.character(res[1,1])
}