setwd("C:/work/School/research/research-2013-2014/stat-courses/hopkins-r-programming/hw-03")


outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

print(head(outcome))

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[,11], breaks=30)

best <- function(state, outcome) 
  {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  }