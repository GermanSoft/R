complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  result<-data.frame(id= integer(0), nobs= integer(0))
  Colnames <- names(result)
  
  filename<-sprintf("%s\\%03d.csv",directory,id)
  for(file in filename)
  {  
    data<-read.csv(file)
    dataFiltered<-data[complete.cases(data),]
    length<-nrow(dataFiltered)
    result <- rbind(result, c(data$ID[1],length))
    names(result) <- Colnames
  }
  result
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}