setwd("C:/Mydata/Certificates/Data Scienties/2- R Programming/practiceFiles/")

complete <- function(directory , id = 1:332) {
        
        
        allMonitorsPollutantCompleted <- data.frame();
        allMonitorsDataList <- lapply(paste(directory, sprintf("%03d.csv", id),sep ='\\') ,read.csv)
        for(aDataFrame in allMonitorsDataList){
                allMonitorsPollutantCompleted <- rbind(allMonitorsPollutantCompleted, c(id= aDataFrame$ID[1], nobs= nrow(na.omit(aDataFrame))))
        }
        names(allMonitorsPollutantCompleted) <- c('id','nobs')
        allMonitorsPollutantCompleted 
        
}