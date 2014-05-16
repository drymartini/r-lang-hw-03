setwd("C:/work/School/research/research-2013-2014/stat-courses/hopkins-r-programming/hw-03")

# Initialization.

outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# print(head(outcome))

outcomes[, 11] <- as.numeric(outcomes[, 11])
outcomes[, 17] <- as.numeric(outcomes[, 17])
outcomes[, 23] <- as.numeric(outcomes[, 23])
hist(outcomes[,11], breaks=30)
stateList = levels(factor(outcomes$State))
# The number gives the column for the 30-day mortaility rate
outcomeIndexList = c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
outcomeList <- c("heart attack", "heart failure", "pneumonia")
hospNameCol <- 2
stateCol <- 7





IsValidState <- function(stateStr)
{
  return(stateStr %in% stateList)
}

IsValidOutcome <- function(outcomeStr)
{
  return(outcomeStr %in% outcomeList)
}



best <- function(state, outcome) 
{
  ## Check input.
  if(!IsValidState(state))
  {
    stop("invalid state")
  }
  if (!IsValidOutcome(outcome))
  {
    stop("invalid outcome")
  }
  # Get column index.
  outcomeCol <- outcomeIndexList[[outcome]]
  cols <- c(hospNameCol, stateCol, outcomeCol)
  # Get records for state.
  stateOutcomes <- subset(outcomes, State == state)
  
  done <- FALSE
  minVal <- 1.0e6
  nameList = vector()
  while(!done)
  {
    # Outcomes are in column 2 of stateOutcomes
    minRow <- which.min(stateOutcomes[,outcomeCol])
    curMinVal <- stateOutcomes[minRow,outcomeCol]
    if (curMinVal <= minVal )
    {
      nameList = c(nameList, stateOutcomes[[minRow,hospNameCol]])
      stateOutcomes = stateOutcomes[-minRow,]
      minVal = curMinVal
    }
    else
    {
      done = TRUE
    }
  }
  return(sort(nameList)[1])
}

IsValidRank <- function(x)
{
  return (x >= 1) & (x %% 1 == 0)
}


rankhospital <- function(state, outcome, num = "best") 
{
  # Check input.
  if(!IsValidState(state))
  {
    stop("invalid state")
  }
  if (!IsValidOutcome(outcome))
  {
    stop("invalid outcome")
  }
  if (!(num=="best" | num=="worst" | IsValidRank(num)))
  {
    stop("invalid rank specification")
  }
  # Get column index.
  outcomeCol <- outcomeIndexList[[outcome]]
  # Get records for state.
  stateOutcomes <- subset(outcomes, State == state)
  stateOutcomes <- stateOutcomes[,c(hospNameCol,outcomeCol)]
  stateOutcomes <- stateOutcomes[complete.cases(stateOutcomes),]
  ordered <- 
    stateOutcomes[order(stateOutcomes[,2],stateOutcomes[,1]),]
  numEntries = nrow(ordered)
  
  
  if (num=="best")
  {
    return(ordered[[1,1]])
  }
  else if (num=="worst")
  {
    return(ordered[[numEntries,1]])
  }
  else if (num <= numEntries)
  {
    return(ordered[[num,1]])
  }
  else
  {
    return(NA)
  }
}

test1 <- function()
{
  print(best("TX", "heart attack"))
  print(best("MD", "heart attack"))
  print(best("MD", "pneumonia"))
}


test2 <- function()
{
  print(rankhospital("TX", "heart failure", 4))
  print(rankhospital("MD", "heart attack", "worst"))
  print(rankhospital("MN", "heart attack", 5000))
}

# test1()
# test2()
  
  