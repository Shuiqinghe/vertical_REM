library(dplyr)
library(readr)

# import data (original exported reference information)
observation <- read.csv("observations.csv", sep="," , header = TRUE)
deployment_camera <- read.csv("deplyment_cameratype.csv")
deployment <- read.csv("deployments.csv", sep="," , header = TRUE)
cameraheight <- read.csv("cameraheight.csv", sep="," , header = TRUE)

# merge locations 
merged <- left_join(x = observation, y = deployment, by = "deploymentID")

# clean up deployment data
deployment_camera[deployment_camera==""] <- NA
deployment_camera<-na.omit(deployment_camera)

# merge camera type of the deployments
colnames(deployment_camera)[1]<-"deploymentID"
#merged1 <- left_join(x=merged, y = deployment_camera, by="deploymentID")
# seems deployment_camera has more deployments than merged, which is reasonable cuz the first one
# is the extracted one, and deployment_cameratype csv is for the whole dataset. to avoid potential mistakes
nrow(as.data.frame(unique(deployment_camera$deploymentID))) # 1060 unique deployments
nrow(as.data.frame(unique(merged$deploymentID))) # 933 unique deployments

# let's delete the extra deployments in deployment data
merged_test<-as.data.frame(unique(merged[,2]))
colnames(merged_test)[1]<-"deploymentID"
merged_test$deploymentID_copy<-merged_test$deploymentID
deployment_camera_test<-deployment_camera
test<-left_join(x=deployment_camera_test,y=merged_test,by="deploymentID")
test<-na.omit(test)
nrow(unique(as.data.frame(test$deploymentID))) # now match: 933
deployment_camera<-test[,c(1:3)]
merged1<-left_join(x=merged, y=unique(deployment_camera),by="deploymentID")
nrow(merged1) # 85150

# add camera height 
colnames(cameraheight)[1]<-"locationName"
merged2 <- left_join(x = merged1, y = cameraheight, by = "locationName")

# Set original database
merged2 <- merged2[,c("scientificName","locationName","height","start","end","model")]
merged2<-merged2[!merged2$scientificName%in%c("","Homo sapiens"),]
merged2$start = substr(x = merged2$start, start = 1,stop = 10)
merged2$end = substr(x = merged2$end, start = 1,stop = 10)

# Delete data with errors
merged2 <- merged2[!grepl("1970-", merged2$start),]
merged2 <- merged2[!grepl("2099-", merged2$start),]

# change format for calculating difference
#merged2$start = replace(merged2$start,from =":",to="-")
startdate <- as.Date(merged2$start)
enddate <- as.Date(merged2$end)

# effortdays > deployment
merged2$effortdays_deployment<-difftime(enddate,startdate,units = "days")
#delete errors with effort days more than 1 year
merged2 <- merged2[merged2$effortdays_deployment <= 365,]
#If the effort day is 0, change it to 1
merged2$effortdays_deployment[merged2$effortdays_deployment == 0] <- 1

# delete the camera types we don't need
table(merged2$model)
merged2<-merged2[merged2$model%in%c(" BTC5HDPX "," HC500 HYPERFIRE ","HC600 HYPERFIRE "," HYPERFIRE 2 COVERT "),]

# effort > per deployment > per location
effort_location<-unique(merged2[,c("locationName","start","end","effortdays_deployment")]) # this is to prevent some deployment has same effort days
effort_location<-effort_location%>%group_by(locationName)%>%mutate(effort_location=sum(effortdays_deployment))
merged2<-left_join(merged2,effort_location,by=c("locationName","start","end","effortdays_deployment"))
# sum(unique(effort_location[,c(1,5)])$effort_location) # 28106 in total

# effort > per location per model
effort_location_model<-unique(merged2[,c("locationName","start","end","model","effortdays_deployment")]) # this is to prevent some deployment has same effort days
effort_location_model<-effort_location_model%>%group_by(locationName,model)%>%mutate(effort_location_model=sum(effortdays_deployment))
merged2<-left_join(merged2,effort_location_model,by=c("locationName","start","end","model","effortdays_deployment"))
# sum(unique(effort_location_model[,c(1,4,6)])$effort_location_model) # 28106 in total, match

# contacts part, contact per species and contacts per species per location, and contact per species per location per model
merged2<-merged2%>%group_by(scientificName)%>%mutate(contact_species=length(scientificName))
merged2<-merged2%>%group_by(scientificName,locationName)%>%mutate(contact_species_location=length(scientificName))
merged2<-merged2%>%group_by(scientificName,locationName,model)%>%mutate(contact_species_location_model=length(scientificName))

# delete effort_deployment, not needed anymore, and unique rows
merged2$model[merged2$model==" BTC5HDPX "]<-"Browning"
merged2$model[merged2$model==" HC500 HYPERFIRE "]<-"Hiperfire1"
merged2$model[merged2$model==" HYPERFIRE 2 COVERT "]<-"Hiperfire2" # change the names btw

# according to height

merged2<-unique(merged2)

# add species trap rate in general for all locations
merged2$sum_effort_all<-sum(unique(merged2[,c(2,8)])$effort_location)
merged2$sum_effort_all<-as.numeric(merged2$sum_effort_all)
merged2<-merged2%>%group_by(scientificName)%>%mutate(species_traprate=contact_species/sum_effort_all)

merged2<-merged2%>%group_by(height)%>%mutate(effort_height=sum(unique(effort_location)))
merged2<-merged2%>%group_by(height,model)%>%mutate(effort_height_model=sum(unique(effort_location_model)))
merged2<-merged2%>%group_by(height,scientificName)%>%mutate(contact_species_height=sum(unique(contact_species_location)))
merged2<-merged2%>%group_by(height,scientificName,model)%>%mutate(contact_species_height_model=sum(unique(contact_species_location_model)))

merged2<-unique(merged2[,-c(2,4,5,7:9,11,12)])
colnames(merged2)<-c("scientificName", "cameraheight", "cameratype", "contact_species", "sum_effortday_all", "species_traprate", "effort_cameraheight", "effort_cameraheight_cameratype", "contact_species_cameraheight", "contact_species_camereaheight_cameratype")

# select the species  
group<-c("Axis axis", "Axis porcinus","Canis aureus","Elephas maximus","Hystrix indica","Macaca mulatta","Muntiacus muntjak","Panthera tigris","Rhinoceros unicornis","Rusa unicolor","Semnopithecus schistaceus","Sus scrofa")
merged2 <- merged2[merged2$scientificName%in%group,]
merged2 <- unique(merged2)

write_csv(merged2,"contact_effort.csv")
