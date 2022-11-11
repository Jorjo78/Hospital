rm(list = ls())

rankhospital <- function(state, outcome, num) {
        
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
                col <- 17}
        if (outcome  == "heart attack") {
                col <- 11}
        if (outcome == "pneumonia") {
                col <- 23}

        
        df4 <- df3[which(df3[ ,col] != "Not Available"), ]
        df4[,col] <- as.numeric(df4[,col])
        df5 <- df4[order(df4[,col],df4[,2]),]
        
        if (num == "best") {
           return(head(df5$Hospital.Name,1))}
        
        else if (num == "worst") {
           return(tail(df5$Hospital.Name,1))}
        
        else  {
                return(df5[num,2])}
}



rankhospital("MD", "heart attack", "worst")
