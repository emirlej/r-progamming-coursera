# File paths
outcomeData <- file.path("data", "outcome-of-care-measures.csv")


find_wanted_column <- function(df, outcome) {
    # Find the wanted column based on outcome

    colNames <- names(df)

    for (i in seq_along(colNames)) {

        col <- colNames[i]

        # Removes dots (.) in a string. However, U.S becomes U S. 
        # Should fix this in th future
        tmp <- gsub("[.]+", " ", col)

        # First column with pattern is found
        if (grepl(pattern = outcome, x = tmp, ignore.case = TRUE)) return(col)
        }
    # If no return
    stop("Column not found...")
}

find_state_hospial <- function(df, state, num) {
    # Find the state hospital which is at num-th place in that state
    #
    # df : data.frame
    # state : string
    # num : int or string

    # Get only wanted state
    df.State <- df[df$State == state,]

    # Remove outcome NA values
    outcomeColumn <- tail(names(df), n = 1)
    df.State <- df.State[complete.cases(df.State[, c(outcomeColumn)]),]

    # If e.g. num = 'best'
    if (is.character(num)) {
        stopifnot(num == "best" || num == "worst")
        num <- ifelse(num == "best", 1 , nrow(df.State))
    } 
    else if (is.numeric(num)) { # if num is numeric
        # Cannot have lower than 1
        stopifnot(num >= 1)
    } 
    else { # Any other type
        stop("num is not correct type")
    }
      
    # Order the data frame
    orderBy <- order(df.State[, outcomeColumn], df.State[, "Hospital.Name"])
    df.State <- df.State[orderBy,]

    return(df.State[num, ])
}

rankall <- function(outcome, num = "best") {
    # Rank all hospitals, and return the num-th hospital in each state
    #
    # outcome : string
    # num : string or int

    # Outcome assertion
    if (is.na(match(outcome, c('heart attack', 'heart failure', 'pneumonia')))) stop("invalid outcome") 
    
    # Read to data frame
    dfOutcome <- read.csv(outcomeData, colClasses = "character")
    
    ## Check that state and outcome are valid
    allStates <- sort(unique(dfOutcome$State))

    ## Get the correct outcome column
    outcomeColumn <- find_wanted_column(dfOutcome, outcome)

    # Look at only these columns
    dfOutcome <- dfOutcome[, c("Hospital.Name", "State", outcomeColumn)]

    # Change this column to be numeric
    suppressWarnings(dfOutcome[, outcomeColumn] <- as.numeric(dfOutcome[, outcomeColumn]))
    
    # Add hospital rankings here
    rankMatrix <- matrix(ncol = 2, nrow = length(allStates))

    # Loop over the states
    for (i in seq_along(allStates)) {

        state <- allStates[i]

        # Find the num state hospital
        state_hospital <- find_state_hospial(dfOutcome, state, num)

        # Add to matrix
        rankMatrix[i, 1] <- state_hospital$Hospital.Name
        rankMatrix[i, 2] <- state
    }

    # Now, create a dataframe
    df <- as.data.frame(rankMatrix)

    # Change the col and row names
    names(df) <- c("hospital", "state")
    rownames(df) <- df$state

    return(df)
}

# Tests
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

# Quiz questions
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
