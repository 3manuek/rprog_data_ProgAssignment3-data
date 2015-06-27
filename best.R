best <- function(state, outcome = c("heart attack", "heart failure", "pneumonia"), cache=TRUE ) {
	## Read outcome data
    tempSet <- NULL 
    outcomeMapping <- list("heart attack" = 11 ,
    	 "heart failure" = 17,
    	 "pneumonia" = 23)

    if (is.null(outcomeMapping[[outcome]])) {
    	stop("Invalid outcome.")
    }

    mappedCol <- outcomeMapping[[outcome]]

	if(!exists("outcomeData") | cache==FALSE) {
		# Chaching data in the parent environment.
        outcomeData <<- read.csv("outcome-of-care-measures.csv",colClasses="character")
    }

    if(!exists("hospitalName") | cache==FALSE){
    	# Chaching data in the parent environment.
    	hospitalName <<- read.csv("hospital-data.csv", colClasses="character")
    }

    if(!exists("states") | cache==FALSE) {
    	states <<- levels(factor(hospitalName$State))
    }

    if ( !(state %in% states) ) {
    	stop("Invalid state.")
    }

    #rankHosp <- aggregate(tempSet[,3], by=list(tempSet$State), FUN=min)
    ## Check that state and outcome are valid

    tempSet <- outcomeData[outcomeData$State == state, c(2,7,mappedCol) ]
    tempSet <- tempSet[!is.na(as.numeric(tempSet[,3])), ]
    tempSet[,3] <- as.numeric(as.character(tempSet[,3])) 
    

	## Return hospital name in that state with lowest 30-day death
	## rate

	# Need to order per hospital or group by both cols
	head(tempSet[order(tempSet[,3], decreasing=FALSE,na.last = TRUE),1],1)
    
}