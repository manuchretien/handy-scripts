## Reorganise data in long format
## by Emmanuelle Chretien
## Nov. 17 2017

###################################

rm(list=ls())

# Set working directory
setwd("")

# load package
require(tidyr)

# load data
toto<-read.csv(file.choose())

# In this specific case, data was organized as a table where each line was a year
# each column was a month
# each cell was a mean value of water discharge 

# First, set year as factor 
toto$year<-as.factor(toto$year)

# gather data in long format using columns as new categoric variable
toto.long<-gather(toto,month,discharge,April, May, June, July, August, September)

# export data
write.table(toto.long,"filename.txt")

# reload data
toto.long<-read.csv(file.choose())
