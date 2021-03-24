rankhospital <- function(state,outcome,  num = "best"){
  
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
  order_outcome <- filter[order(filter$target,filter$Hospital.Name,na.last = NA),]
  order_outcome$rank <- 1:nrow(order_outcome)
  
  if(num == "best"){
    num = 1
  }else if(num == "worst"){
    num = nrow(order_outcome)
  }else if (num > nrow(order_outcome)){
    return(NA)
  }
  
  filter_rank <- order_outcome[order_outcome$rank==num,]
  filter_rank$Hospital.Name
  
}


