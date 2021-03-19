pollutantmean <- function(directory,pollutant,id=1:332){
  
  files <- list.files(path = directory)
  f <- list()
  for (i in 1:length(files)) {
    f[[i]] <- read.csv(paste0(directory,"/",files[i]))
  }
  
  pollutant_df <- bind_rows(f)
  
  filter_byID <- pollutant_df[ pollutant_df$ID>=id[[1]] & pollutant_df$ID<=tail(id,n=1),]
  
  filter_bypollutant <- filter_byID[pollutant]
  names(filter_bypollutant) = "pollutant"
  mean(filter_bypollutant$pollutant,na.rm = TRUE)
  
  
}




