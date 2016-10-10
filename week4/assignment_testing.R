## Libraries
library(ggplot2)

## Where the data is stored
dataDir <- file.path(getwd(), "data")
hospitalData <- file.path(dataDir, "hospital-data.csv")
careOutcomeData <- file.path(dataDir, "outcome-of-care-measures.csv")

# Read in the csv files to data frames
hospital.df <- read.csv(hospitalData) 
careOutcome.df <- read.csv(careOutcomeData, colClasses = "character")          

# Take a quick look at the data
head(careOutcome.df)

## This dataset has many columns, ncol = 46
dim(careOutcome.df)

## Check the column names
names(careOutcome.df)

## Make a simple histogram of the 30-day death rates
## Take a quick look
str(careOutcome.df[, 11])

## Make the data numeric
careOutcome.df[, 11] <- as.numeric(careOutcome.df[, 11])

## Now all non-numeric will be NA
str(careOutcome.df[, 11])

## Create a histogram plot
plt <- qplot(careOutcome.df[, 11], geom = "histogram")
## Adds a count legend and fills data with color
plt + geom_histogram(aes(fill = ..count..))

## Name of the column
names(careOutcome.df)[11]
