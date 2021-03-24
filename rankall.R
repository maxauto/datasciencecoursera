rankall <- function(outcome, num = "best") {
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  colnames(df)[11]<-"heart attack"
  colnames(df)[17]<-"heart failure"
  colnames(df)[23]<-"pneumonia"
  df[, 11] <- as.numeric(df[, 11])
  df[, 17] <- as.numeric(df[, 17])
  df[, 23] <- as.numeric(df[, 23])
  
  if(!(outcome %in% c(colnames(df)[11],colnames(df)[17],colnames(df)[23]))){
    stop("invalid outcome")
  }
  filter <- df[c(2,7,grep(outcome, colnames(df)))]
  colnames(filter)[3]<-"target"
  m<-split(filter, filter$State)
  list_order <- lapply(split(filter, filter$State), function(x) x[order(x$target,x$Hospital.Name,na.last = NA), ])
  list_allrank <- lapply(list_order,function(x){x$rank <- 1:nrow(x); return(x)})
  name <- names(list_allrank)
  if(num == "best"){
    num = 1
    list_byrank <- lapply(list_allrank,function(x){x[x$rank==num,c(1,2)]})
  }else if(is_numeric(num)){
    list_byrank <- lapply(list_allrank,function(x){x[x$rank==num,c(1,2)]})
    
  }else if(num == "worst"){
    list_byrank <- lapply(list_allrank,function(x){x[x$rank==nrow(x),c(1,2)]})
  }
  
  i<-1
  s<-list()
  for(x in list_byrank){
    if(nrow(x)==0){
      s[[i]] <- rbind(data.frame(Hospital.Name = NA, State = names(list_byrank[i])),x)
      
    }else{
      s[i] <- list_byrank[i]
    }
    i<-i+1
  }
  names(s) <- name
  final <- do.call("rbind", s)
  names(final) <- c("hospital","state")
  final
  
  
}
