setwd("C:/Mydata/Certificates/Data Scienties/2- R Programming/practiceFiles/")

pollutantmean <- function(directory  ,pollutant, id = 1:332) {
           allMonitorsPollutant <- data.frame()
           allMonitorsDataList <- lapply(paste(directory, sprintf("%03d.csv", id),sep ='\\') ,read.csv)
           for(aDataFrame in allMonitorsDataList){
                  allMonitorsPollutant <- rbind(allMonitorsPollutant,aDataFrame)
           }
           
          round(mean(allMonitorsPollutant[,pollutant], na.rm = TRUE),3)
}

