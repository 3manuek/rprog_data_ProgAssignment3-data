rankhospital <- function(state, outcome, num = "best", cache=TRUE ) {
	## Read outcome data
	library(dplyr)
	library(data.table)
	tempSet <- NULL 
    outcomeMapping <- list("heart attack" = 11 ,
    	 "heart failure" = 17,
    	 "pneumonia" = 23)
    # num coudl be best, worst or integer rank
    # if num > total records, return NA
    # 

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

    ## Check that state and outcome are valid

    if(!exists("states") | cache==FALSE) {
    	states <<- levels(factor(hospitalName$State))
    }

    if ( !(state %in% states) ) {
    	stop("Invalid state.")
    }

	## Return hospital name in that state with the given rank
	## 30-day death rate

    tempSet <- outcomeData[outcomeData$State == state, c(2,7,mappedCol) ]
    tempSet <- tempSet[!is.na(as.numeric(tempSet[,3])), ]
    tempSet[,3] <- as.numeric(as.character(tempSet[,3])) 

    names(tempSet)[3] <- "Rate"
    names(tempSet)[2] <- "Hospital.Name"
    setkey(setDT(tempSet), Rate, Hospital.Name)
    ranks <- tempSet %>% mutate(Rank = row_number(Rate)) 

    if ( as.character(num) == "best" ) {
    	num <- 1
    }

    if ( as.character(num) == "worst") {
    	num <- max(ranks$Rank)
    }

    num <- as.numeric(num)


    if ( max(ranks$Rank) >= num ) {
    	(head(filter(ranks, Rank == num),1))$Hospital.Name
    } else {
    	NA
    }
}