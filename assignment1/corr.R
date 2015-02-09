corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  result<-vector()
  
  filename<-sprintf("%s\\%03d.csv",directory,1:332)
  for(file in filename)
  {  
    data<-read.csv(file)
    dataFiltered<-data[complete.cases(data),]
    length<-nrow(dataFiltered)
    if(length >= threshold)
    {       
      c<-cor(dataFiltered$sulfate,dataFiltered$nitrate)
      result <- c(result, c)
    }
  }
  result
  
  ## Return a numeric vector of correlations
}