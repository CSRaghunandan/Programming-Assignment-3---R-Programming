rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClass = "character")
    
    # character vectors containing the valid state and outcome names
    validOutcome <- c("heart attack", "heart failure", "pneumonia")
    validStates <- unique(data[, "State"])
    
    # check that state and outcome are valid
    if(!(outcome %in% validOutcome))
    {
        stop("invalid outcome") 
    }
    
    hospitalName <- c(0)

    for(i in seq_along(validStates))
    {
        hospitalName[i] <- rankhospital(validStates[i], outcome, num)
    }
     data.frame(hospital = hospitalName, state = validStates, row.names = validStates)

    
#     sorted.df.list <- lapply(s, function(x) {
#         index <- order(sorting.variable.1, sorting.variable.2)
#         x[index, ]
#     } )
#     
    ## For each state, find the hospital of the given rank
}   
## Return a data frame with the hospital names and the
## (abbreviated) state name
