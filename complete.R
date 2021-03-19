complete <- function(directory, id=1:332){
  
  files <- list.files(path = directory)
  f <- list()
  show <-list()
  for (i in 1:length(files)) {
    f[[i]] <- read.csv(paste0(directory,"/",files[i]))
  }
  pollutant_df <- bind_rows(f)
  pollutant_df <- drop_na(pollutant_df,sulfate,nitrate)
  nobs_list<-list()
  
  i<-1
  for ( ids in id){
    filter_byID1 <- pollutant_df[pollutant_df$ID == ids,]
    nobs_list[[i]] <-  nrow(filter_byID1)
    i<-i+1
  }
  nobs<-unlist(nobs_list)
  df <- data.frame(id,nobs)
  df
}
