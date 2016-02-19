setwd("C:/Mydata/Certificates/Data Scienties/2- R Programming/practiceFiles/")
source("complete.R")
corr <- function(directory , threshold = 0) {
        
        
         dsCompletedAboveThr <-   which(complete(directory)$nobs>threshold)
         fileList <- list.files(directory, full.names=TRUE)
         corrVectors  <-  c()
         if( length(dsCompletedAboveThr) == 0){
                 class(corrVectors) <- 'numeric'
         }
         else{
                 for( i in dsCompletedAboveThr ){
                        dfSingleMonitor <- na.omit(read.csv(fileList[i]))
                        corrVectors[length(corrVectors)+1 ] <- cor(dfSingleMonitor[,2],dfSingleMonitor[,3] )
                 }
         }
        
        
        corrVectors
        
        
       ##  corrVectors <- c()
        ## dsCompletedCases <- complete(directory) 
        ## corrVectors <- apply(dsCompletedCases,function(singleDF){
        ##         if( singleDF$nobs > threshold ){
        ##                 rfile <- na.omit(read.csv(paste(directory, sprintf("%03d.csv", singleDF$ID),sep ='\\')))
        ##                cor(rfile[,2],rfile[,3])
        ##         }
                
        ## })
        ## corrVectors
        ##allMonitorsPollutantCompleted <- data.frame();
        ##corrVectors <- c()
        
        ##allMonitorsDataList <- lapply(paste(directory, sprintf("%03d.csv", 1:332),sep ='\\') ,read.csv)
        ##for(i in length(allMonitorsDataList)){
        ##        dsMonitor <- na.omit(allMonitorsDataList[[i]])
        ##        if ( nrow(dsMonitor) > threshold){
        ##                corrVectors[i] <- corr(dsMonitor[,2],dsMonitor[,3])  
        ##        }
        ##}
        ##corrVectors 
        
}