## Where the data is stored
dataDir <- file.path(getwd(), "data")
hospitalData <- file.path(dataDir, "hospital-data.csv")
careOutcomeData <- file.path(dataDir, "outcome-of-care-measures.csv")

# Read in the csv files to data frames
hospital.df <- read.csv(hospitalData)
careOutcome.df <- read.csv(careOutcomeData, colClasses = "character")

find_outcome_column <- function(df, outcome) {
    # This function searches through a dataframe and finds
    # column name which contains the outcome string.
    #
    # Input
    #   df : dataframe
    #   outcome : string
    # Return
    #   outcomeColName : string

    # All column names in dataframe
    colNames <- names(df)
    outcomeColName <- NULL

    # Loop through all columns and break when first match is found
    for (i in seq_along(colNames)) {

        # Remove non-chars in string and replace with white space
        tmp <- gsub("\\W+", " ", colNames[i])

        # Match colname with outcome
        if (grepl(outcome, tmp, ignore.case = TRUE)) {

            # Change back to correct col.name
            outcomeColName <- colNames[i]

            return(outcomeColName)
        }
    }

    # Raise error if not found
    if (is.null(outcomeColName)) stop("Invalid outcome")
    }

valid_state <- function(df.col, state) {
    # Checks whether the state is valid.
    return(!is.na(match(state, unique(df.col))))
}

rank_hospitals <- function() {

}

rankhospital <- function(state, outcome, num = "best") {
    ## Rank hospitals in the wanted state. 
    ##
    ## state: string, 
    ##      e.g. 'TX' or 'MD'
    ## outcome: string
    ##      e.g. 'heart failure' or 'heart attack'. See docs. in function
    ##      find_outcome_column for more info.
    ## num : int or string ('best' or 'worst')
    ##      The function will return the i-th best hospital.
    ## return : df-row

    ## Check that state and outcome are valid
    if (valid_state(careOutcome.df[, "State"], state)) {

        ## Find the wanted outcome column
        outcomeColName <- find_outcome_column(careOutcome.df, outcome)

        # Create a new df
        df <- careOutcome.df[careOutcome.df$State == state, c("Hospital.Name", outcomeColName)]        
        
        # Create a numeric column and remove NA's
        suppressWarnings(df[, outcomeColName] <- as.numeric(df[, outcomeColName]))
        df <- df[complete.cases(df), ]

        # If the num is greater than number of rows in dataframe.
        if (!is.character(num) && num > nrow(df)) {
            return(NA)
        } else {

            # Order the data first. First by mortality, then y
            df <- df[order(df[, outcomeColName], df[, "Hospital.Name"]),]

            # Change name. Should make a function for this taking outcomeColName as input
            names(df)[-1] <- "Rank"

            # By default it ranks from lowest to highest, therefore we need to use -df
            df$MyRank <- rank(df[, "Rank"], ties.method = "first")

            ## Return hospital name in that state with the given rank
            if (tolower(num) == "best") {
                return(df[1,])
            } else if (tolower(num) == "worst") {
                return(df[nrow(df), ])
            } else {
                return(df[num, ])
            }
        }

    } else {
        stop("invalid state")
    }
}

# Test runs
rankhospital("TX", "heart attack", "best")
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000) # Should return NA
