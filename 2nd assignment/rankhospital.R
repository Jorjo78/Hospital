rm(list = ls())

rankhospital <- function(state, outcome, num = 4) {
        
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
        
        if (outcome == "heart failure") {
                df4 <- df3[which(df3[ ,11] != "Not Available"), ]
                df5 <- df4[, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
                df6 <- df5[as.numeric(df5$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                
                df7 <- df6[with(df6, order(df6$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, df6$Hospital.Name)),]
                View(df7)
                return(df6 <- df7[num, ])      
        }
        if (outcome == "heart attack") {
                df4 <- df3[which(df3[ ,11] != "Not Available"), ]
                df5 <- df4[, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
                df6 <- df5[order(as.numeric(df5$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
                return(df6 <- df6[num, ])
        }
        if (outcome == "pneumonia") {
                df4 <- df3[which(df3[ ,11] != "Not Available"), ]
                df5 <- df4[, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
                df6 <- df5[order(as.numeric(df5$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
                return(df6 <- df6[num, ]) 
        }
}



rankhospital("MD", "heart attack", )


####------
df6[(nrow(df5)-num):nrow(df5), ]        # alternatively: dplyr: sort?order?