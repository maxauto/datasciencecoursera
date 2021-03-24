best <- function(state,outcome){
  
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  colnames(df)[11]<-"heart attack"
  colnames(df)[17]<-"heart failure"
  colnames(df)[23]<-"pneumonia"
  df[, 11] <- as.numeric(df[, 11])
  df[, 17] <- as.numeric(df[, 17])
  df[, 23] <- as.numeric(df[, 23])

  if(!(state %in% df$State)){
    stop("invalid state")
  }
  
  if(!(outcome %in% c(colnames(df)[11],colnames(df)[17],colnames(df)[23]))){
    stop("invalid outcome")
  }
  
  filter <- df[df$State==state, c(2,7,grep(outcome, colnames(df)))]
  colnames(filter)[3]<-"target"
  imin<-which.min(as.vector(filter$target))
  filter$Hospital.Name[imin]

}
