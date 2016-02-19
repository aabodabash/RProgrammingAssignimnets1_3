rankall <- function(outcome, num = "best") {
       
        
        ##lowercase of outcome and uppercase of state
        ## to avoid input caps mistakes
        outcome <- tolower(outcome)
        
        
        ## to hold outcome col index from CVS file
        outcomeRateColIndex <- 0
        
        ## Read outcome data
        allDF <-  read.csv("outcome-of-care-measures.csv",colClasses = "character") 
        
      
        ##Validating outcome "heart attack", "heart failure", or "pneumonia"
        ## asign outage coloutcome index
        switch(outcome,
               "heart attack" ={outcomeRateColIndex <- 11},
               "heart failure" ={outcomeRateColIndex <- 17},
               "pneumonia" ={outcomeRateColIndex <- 23},
               stop("invalid outcome"))
        
        allDF[,outcomeRateColIndex] <- suppressWarnings(as.numeric(allDF[,outcomeRateColIndex]))
        
        rankedHospitals <- data.frame("hospital" = character(0),"state" = character(0),stringsAsFactors=FALSE)
        
        allStatesList <- split(allDF,allDF$State)
       
        rateIndex <- if( tolower(num) == "best"){
                1    
        }else if (tolower(num) == "worst") {
               -1  
        }else{ num }
 
        
        for(stateDF in allStatesList){
                
                if(is.numeric(rateIndex) &&  rateIndex > nrow(stateDF) ){
                        rankedHospitals[nrow(rankedHospitals)+1,] <- c("NA",stateDF[1,7])   
                }else{
                        
                        stateOrdered <- stateDF[order(stateDF[, outcomeRateColIndex], stateDF[, 2], na.last = NA),] 
                        
                        thisRating <-  if(is.numeric(rateIndex) && rateIndex == -1){
                                nrow(stateOrdered)
                        }else{
                                rateIndex 
                        }
                         
                        ##return(stateOrdered)
                        if (!is.na(stateOrdered[thisRating,outcomeRateColIndex])){
                                rankedHospitals[nrow(rankedHospitals)+1,] <-  c(  stateOrdered[thisRating,2] , stateOrdered[thisRating,7])  
                        }
                        else
                        {
                                rankedHospitals[nrow(rankedHospitals)+1,] <-  c("NA",stateOrdered[1,7])          
                        }
                        
                }
                        
                         
        }
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
       ## names(rankedHospitals) <-  c("hospital","state")
        rankedHospitals
}