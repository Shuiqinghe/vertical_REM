rm=list(ls())

library(readr)
library(dplyr)

contact_effort <- read.csv("contact_effort.csv")
profile <- read.csv("profile_estimate_MLE.csv")

# first thing first, merge based on profile data
colnames(contact_effort)[3]<-"camera_type"
colnames(profile)[4]<-"cameraheight"
colnames(profile)[2]<-"camera_type"

# in contact_effort dataset, since the effort calculating was pretty strict cuz numerous wrong 
# starting time, for those which exist in profile but not contact_effort, they need to be deleted
  
merge <- left_join(profile,unique(contact_effort),by=c("scientificName","camera_type","cameraheight"))
merge <- merge[,-1]
merge <- na.omit(merge) # some might have NA for valid entering point but not valid recording time, so no valid effort days 

# let's add speed, activity level and their se
sdata<-read.csv("./speed.csv")
aldata<-read.csv("./activity_level.csv")
saldata <- left_join(sdata, aldata, by = "scientificName")
saldata <- saldata[, c(2:8)]
colnames(saldata)[4] <- "groupsize_speed"
colnames(saldata)[7] <- "groupsize_al"

# merge with profiles
merge <- left_join(merge, saldata, by = "scientificName")
merge$profile_km <- merge$profile/1000
merge$profile_se_km<-merge$profile_se/1000
merge$hspeed_kmd<-merge$hmean_speed*86.4
merge$hspeed_se_kmd<-merge$hmean_speed_se*86.4

# merge with group size
gdata<-read.csv("./ref_info_data/group_size.csv")
merge <- left_join(merge, gdata, by = "scientificName")
colnames(merge)[19] <- "al_se"

write_csv(merge,"./for bootstrap/boostrap_all.csv")
