rankhospital <- function(state, outcome, num = "best") 
{
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
    if(!(state %in% validStates))
    {
        stop("Invalid state")
    }
    
    if(outcome == "heart attack")
        temp <- 11
    else if(outcome == "heart failure")
        temp <- 17
    else if(outcome == "pneumonia")
        temp <- 23
    
    stateData <- subset(data, data$State == state)
    x <- stateData[order(as.numeric(stateData[, temp]), stateData$Hospital.Name, na.last = NA), ]
        
    if(num == "best")
        return(x$Hospital.Name[1])
    else if(num == "worst")
        return (tail(x, 1)$Hospital.Name)
    else
        x$Hospital.Name[num]
    
}
## Return hospital name in that state with the given rank
## 30-day death rate
