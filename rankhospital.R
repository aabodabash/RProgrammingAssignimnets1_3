rankhospital <- function(state, outcome, num = "best") {
      
        ##lowercase of outcome and uppercase of state
        ## to avoid input caps mistakes
        outcome <- tolower(outcome)
        state <- toupper(state)
        
        ## to hold outcome col index from CVS file
        outcomeRateColIndex <- 0
        
        ## Read outcome data
        allDF <-  read.csv("outcome-of-care-measures.csv",colClasses = "character") 
        
        
        ## validating state
        if(length(grep(state, allDF[,7])) == 0){
                stop("invalid state")
                ##stop("State not found, Kindly check the spelling..!")
        }
        
        ##Validating outcome "heart attack", "heart failure", or "pneumonia"
        ## asign outage coloutcome index
        switch(outcome,
               "heart attack" ={outcomeRateColIndex <- 11},
               "heart failure" ={outcomeRateColIndex <- 17},
               "pneumonia" ={outcomeRateColIndex <- 23},
               stop("invalid outcome"))
        
        ## creating a new DF of two cols (Hosbital Name, outcome) for only seletced state.
        slicedDF <-   allDF[allDF$State == state, c(2,outcomeRateColIndex)] 
        
        
        if ( is.numeric(num) && num > nrow(slicedDF)){
                return(NA)
        }
        
        
        ## convert outage rate col to numiric type
        slicedDF[,2] <- suppressWarnings(as.numeric(slicedDF[,2]))
        
        slicedDF <- slicedDF[order(slicedDF[, 2], slicedDF[, 1], na.last = NA),] 
        
        rateIndex <- if( tolower(num) == "best"){
                   1    
        }else if (tolower(num) == "worst") {
                nrow(slicedDF)  
        }else{ num }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        slicedDF[rateIndex,1]
     
}