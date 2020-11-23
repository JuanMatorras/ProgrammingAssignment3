rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        datafile <- file.path("data", "outcome-of-care-measures.csv")
        outcomeData <- read.csv(datafile, colClasses = "character")
        
        ## Check that the outcome is valid
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        stopifnot("invalid outcome"= outcome %in% validOutcomes)
        
        ## Return a data frame  with the hospital names  and abbreviated state
        ## for the specified rank of death rate for that particular outcome

        ##   Outcomes columns: heart attack-11, heart failure-17, pneumonia-23
        outcomesColumns <- data.frame(Outcomes = validOutcomes, DataColumn = 
                                              c(11, 17, 23))
        colOutcome <- outcomesColumns[outcomesColumns$Outcomes == outcome, 2]
        ## Subsetting hospitals with actual value for required outcomes. Take 
        ##    out name, state and outcome under evaluation
        candidateHosps <- subset(outcomeData, outcomeData[,colOutcome] != 
                                "Not Available", select = c(2, 7, colOutcome))
        ## Conversion of DMRs to numeric values
        candidateHosps[, 3] <- as.numeric(candidateHosps[, 3])
        ## Ordering by DMR first and second alphabetically the names
        finalListHosps <- candidateHosps[order(candidateHosps[, 3], 
                                               candidateHosps[, 1]), ]
        ## List with valid states ordered alphabetically
        validStates <- sort(unique(outcomeData[, 7]))
        rankTable <- data.frame(hospital = character(), state = character())
        if (num == "best") num <- 1
        for (istate in validStates) {
                stateCandidates <- subset(finalListHosps, finalListHosps[, 2] == 
                                                  istate, select = c(1, 3))
   ##             stateCandidates <- finalListHosps[, 2 == istate]
                if (num == "worst") {
                        rankTable[istate, ] <- c(tail(stateCandidates[,1], 1), 
                                                 istate)
                                        } else {
                        if(num > nrow(stateCandidates)) {
                                rankTable[istate, ] <- c("NA", istate)
                        } else {
                                rankTable[istate, ] <- c(stateCandidates[num, 1], 
                                                         istate)
                        }
                }
        }
        return(rankTable)
}

