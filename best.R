best <- function(state, outcome)
{
    # read outcome data
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
    y <- subset(stateData, as.numeric(stateData[, temp]) == min(as.numeric(stateData[, temp]), na.rm = TRUE))
    y <- sort(y$Hospital.Name)
    head(y, 1)
    # return hospital name in that state with the lowest 30 day
    # death rate.
}