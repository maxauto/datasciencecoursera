complete <- function(directory, id=1:322){
  
  files <- list.files(path = directory)
  f <- list()
  show <-list()
  for (i in 1:length(files)) {
    f[[i]] <- read.csv(paste0(directory,"/",files[i]))
  }
  pollutant_df <- bind_rows(f)
  s <- list()
  i<-1
  for ( ids in id){
    filter_byID1 <- pollutant_df[pollutant_df$ID == ids,]
    s[[i]] <- filter_byID1
    i<-i+1
  }
  show <- bind_rows(s)
  show_rmNA <- drop_na(show,sulfate,nitrate)
  group_byID <- group_by(show_rmNA,ID)
  final_show <- summarise(group_byID,nobs = n())
  final_show
}
