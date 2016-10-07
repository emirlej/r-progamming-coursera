complete <-
function(directory, id = 1:332, sep = ",") {
    ## Directory where the csv files are stored.
    ##
    ## id is an integer vector used to choose which
    ## csv files should be read.
    ##
    ## Calculate the number of complete observations,
    ## both sulfate and nitrate in each csv file.
    
    ## Read all files to vector
    csvfiles <- list.files(directory, pattern = ".csv$")
    
    ## Initialize empty dataframe
    df_nobs <- data.frame(id = integer(), nobs = integer())
    
    ## Loop through the file id's
    for (i in id) {
                
        ## CSV file to data frame
        df <- read.csv(file = file.path(directory, csvfiles[i]), sep = sep)
        
        ## Calculate the number of complete cases in column 2 (sulfate) and 3 (nitrate)
        ###non_na <- colSums(!is.na(df[, 2:3]))
        ###non_na <- sum(!is.na(df[, 2:3]))
        non_na <- df[complete.cases(df), ]
        
        ## Insert number of complete obs. in output
        df_nobs <- rbind(df_nobs, list(id = i, nobs = nrow(non_na)))
                        
    }
    
    return(df_nobs)
    
}
