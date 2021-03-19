corr <- function(directory, threshold = 0){
  
  files <- list.files(path = directory)
  f <- list()
  for (i in 1:length(files)) {
    f[[i]] <- read.csv(paste0(directory,"/",files[i]))
  }
  
  pollutant_df <- bind_rows(f)
  pollutant_rmNa <- drop_na(pollutant_df,sulfate,nitrate)
  complete_case <- complete(directory)
  complete_bythr <- complete_case[complete_case$nobs > threshold,]
  j <- 1
  cr <-list()
  for( ids in complete_bythr$ID){
    each_id <- pollutant_rmNa[pollutant_rmNa$ID == ids,]
    cr[[j]] <- cor(each_id$sulfate,each_id$nitrate)
    j <- j+1
  }
  unlist(cr)
}

