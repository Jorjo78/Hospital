rm(list = ls())

##-------4 week assignment (Plot 30-day mortality rates for heat attack)

setwd("/Users/giorgiocavallo/Desktop/GIORGIO/COURSERA/ProgrammingAssignment3/rprog-data-ProgAssignment3-data/")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

hist(as.numeric(outcome[, 11]))

#-------4 week assignment (Finding the best hospital in a state)

best <- function(state, outcome) {
     
       setwd("/Users/giorgiocavallo/Desktop/GIORGIO/COURSERA/ProgrammingAssignment3/rprog-data-ProgAssignment3-data/")
       df <-read.csv("outcome-of-care-measures.csv")
       
       ##check that state and outcome are valid
       df2 <- df[which(df$State == state), 7] 
       if (length(df2) == 0) {
                stop("invalid state")}
       if (outcome %in% c("heart attack","heart failure","pneumonia")){
       }
       else{
                stop("invalid outcome")
               }
       ## return hospital name in that state with lowest 30-day death rate
       df3 <-  df[which(df$State == state),]
  
       if (outcome == "heart attack") {
               df4 <- df3[which(df3[ ,11] != "Not Available"), ]
               df4[,11] <- as.numeric(df4[ ,11])
               df5 <- df4[which.min(df4$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
               df6 <- df5[order(df5$Hospital.Name), ]
               df6 <- df6[1,]
               return(df6)
               }
       if (outcome == "heart failure") {
               df4 <- df3[which(df3[ ,11] != "Not Available"), ]
               df4[,11] <- as.numeric(df4[,11])
               df5 <- df4[which.min(df4$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
               df6 <- df5[order(df5$Hospital.Name), ]
               df6 <- df6[1,]
               return(df6)
               }
       if (outcome == "pneumonia") {
               df4 <- df3[which(df3[ ,11] != "Not Available"), ]
               df4[,11] <- as.numeric(df4[,11])
               df5 <- df4[which.min(df4$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")] 
               df6 <- df5[order(df5$Hospital.Name), ]
               df6 <- df6[1,]
               return(df6)
               }
              }

best("MD", "pneumonia")


