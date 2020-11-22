rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        datafile <- file.path("data", "outcome-of-care-measures.csv")
        outcomeData <- read.csv(datafile, colClasses = "character")
        
        ## Check that state and outcome are valid
        validStates <- unique(outcomeData[, 7])
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        stopifnot("invalid state"= state %in% validStates)
        stopifnot("invalid outcome"= outcome %in% validOutcomes)
        
        ## Return hospital name in that state with the given rank of 30-day 
        ##   death rate for that particular outcome
        ##   Outcomes columns: heart attack-11, heart failure-17, pneumonia-23
        outcomesColumns <- data.frame(Outcomes = validOutcomes, DataColumn = 
                                              c(11, 17, 23))
        colOutcome <- outcomesColumns[outcomesColumns$Outcomes == outcome, 2]
        ## Subsetting hospitals belonging to the state and with actual values  
        ##   for the required outcome. Take out name and outcome evaluated
        candidateHosps <- subset(outcomeData, outcomeData[,7] == state & 
                                 outcomeData[,colOutcome] != "Not Available", 
                                 select = c(2, colOutcome))
        ## Conversion of DMRs to numeric values
        candidateHosps[, 2] <- as.numeric(candidateHosps[, 2])
        ## Ordering by DMR first and second alphabetically the names
        finalListHosps <- candidateHosps[order(candidateHosps[, 2], 
                                               candidateHosps[, 1]), ]
        if (num == "best") num <- 1
        if (num == "worst") {
                answer <- tail(finalListHosps[,1], 1)
        } else {
                if(num > nrow(finalListHosps)) {
                        answer <- "NA"
                } else {
                        answer <- finalListHosps[num, 1]
                }
        }
        return(answer)
}
