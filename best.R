
## Reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specifed outcome
## in that state
best <- function(state, outcome) {
       
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
        
        ## convert outage rate col to numiric type
        slicedDF[,2] <- suppressWarnings(as.numeric(slicedDF[,2]))
        
        ## order by outcome rate ASC then Hosbital name ASC
        slicedDF <- slicedDF[order(slicedDF[, 2], slicedDF[, 1], na.last = NA),]
        
        ## return state name
        slicedDF[1,1]
      
}

## length(grep("AL", outcome[,7]))
## [1] 98
## unique(outcome[,7])
## [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI"
## [24] "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "PR" "RI" "SC" "SD" "TN" "TX" "UT"
## [47] "VT" "VI" "VA" "WA" "WV" "WI" "WY" "GU"