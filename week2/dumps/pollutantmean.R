pollutantmean <-
function(directory, pollutant, id = 1:332, sep = ",") {
    ## 'directory' is a character vector of length 1 indicating the
    ## location of the CSV files.
    ##
    ## pollutant is a character vector of length 1 indicating the name
    ## of the pollutant for which we will calculate the mean; e
    ##
    ## id is a numeric vector specifying which files csv files should be
    ## parsed.
    
    ## Read all csvfiles to vector
    csvfiles <- list.files(path = directory, pattern = ".csv$")
    numpoints <- 0L
    totalsum <- 0L
    
    ## Loop over each file and calculate the mean in the specific column
    for (csvfile in csvfiles[id]) {
                
        ## Read file to df
        data <- read.csv(file.path(directory, csvfile), sep = sep)
        
        pollutant_data <- data[[pollutant]]
        pollutant_data <- pollutant_data[complete.cases(pollutant_data)]
        
        ## Calculate the mean in file for wanted column
        ##colmean <- mean(df[[pollutant]], na.rm = TRUE)
        
        ## Calculate the sum of the values
        #sums <- append(sums, sum(pollutant_data))        
        totalsum <- totalsum + sum(pollutant_data)
        numpoints <- numpoints + length(pollutant_data)
                            
        } 
    ## Calculate the mean of the means vector
    return(totalsum / numpoints)
    }
