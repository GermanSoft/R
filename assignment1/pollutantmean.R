pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  filename<-sprintf("%s\\%03d.csv",directory,id)
  total<-data.frame()
  for(file in filename)
  {  
    data<-read.csv(file)
    total<-rbind(total,data)      
  }
  
  mean(total[,pollutant],na.rm = TRUE) 
  
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
}