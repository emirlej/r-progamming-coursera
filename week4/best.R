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
    for ( i in seq_along(colNames) ) {
        
        # Remove non-chars in string and replace with white space
        tmp <- gsub("\\W+", " ", colNames[i])
        
        # Match colname with outcome
        if ( grepl(outcome, tmp, ignore.case = TRUE) ) {
            
            # Change back to correct col.name
            outcomeColName <- colNames[i]
            
            return(outcomeColName)
        }
    }
    
    # Raise error if not found
    if ( is.null(outcomeColName) ) stop("Invalid outcome")
}


best <- function(state, outcome) { 
    ## Read the entire outcome-of-care-measures.csv file and find the
    ## state hospital with best (i.e. lowest) 30-day heart-attak mortality.
    ##
    ## state : string
    ## outcome : numerical vector specifying which columns are important
    
    # The wanted column number
    colNum <- NULL
    
    # Check if state exists in df
    state.ok <- match(state, unique(careOutcome.df$State))
    
    if (!is.na(state.ok)) {
        
        # Get column names
        colNames <- names(careOutcome.df)
        
        # Find index of column with wanted outcome
        outcomeColName <- find_outcome_column(careOutcome.df, outcome)
        
        # Createa df with only the specified state
        state.df <- careOutcome.df[careOutcome.df$State == state, 
                                   c("Hospital.Name", outcomeColName)]
        
        # Change the outcome column to numeric
        # Need to suppress warning beacuase of NA coercion
        suppressWarnings(state.df[, c(outcomeColName)] <- 
                             as.numeric(state.df[, c(outcomeColName)]))
        
        # Find minimum value in wanted column
        rowMinHospital <- which.min(state.df[, c(outcomeColName)])
        
        # Return the hospital name
        return(state.df[rowMinHospital, 1])
        
    } else {
        # Print error msg
        stop("Invalid state")
    }    
}
