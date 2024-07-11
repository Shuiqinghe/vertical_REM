rm(list=ls())

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Macroecology project/PhD projects/Shuiqing He/REM_vertical/data")

obs_data <- read.csv("./ref_info_data/20240618-bardiya-observationpositions.csv", sep="," , header = TRUE)
assets_data <- read.csv("./ref_info_data/20240618-bardiya-assets.csv", sep="," , header = TRUE)
obs_origin <- read.csv("./ref_info_data/observations.csv", sep="," , header = TRUE)
media <- read.csv("./back_up/ref_info_data/media.csv", sep="," , header = TRUE)
cam_height <- read.csv("./back_up/ref_info_data/cameraheight.csv", sep="," , header = TRUE)

# data merging
colnames(assets_data)[12] <- "asset"
colnames(obs_origin)[3] <- "sequence_id"
merged <- left_join(x = obs_data, y = assets_data, by = "asset")
merged <- merged[,c(5,6,8,10,12:16)]
obs_origin <- obs_origin[, c(3,9)]
data <- left_join(x = merged, y = obs_origin, by = "sequence_id")
media <- media[,c(5,7)]
colnames(media)[2] <- "filename"
data <- merge(data, media, by = "filename")

# change the format of timestamp first 
data$timestamp = Replace(data=data$timestamp,from ="T",to=" ")
data$timestamp = substr(x = data$timestamp, start = 1,stop = 19) # to get rid of the + hours 

# getting rid of the wrong time records
data <- data[!grepl("1970:", data$timestamp),]
data <- data[!grepl("2099:", data$timestamp),]
data <- data[!grepl("2019:01:01 00:", data$timestamp),]
data <- subset(data, timestamp!="")

# changing into time format
data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%S")

# change the name of the model
data$model[data$model == "\"BTC5HDPX\""]<-"Browning"
data$model[data$model == "\"HYPERFIRE 2 COVERT\""]<-"Hiperfire2"
data$model[data$model == "\"HC500 HYPERFIRE\""]<-"Hiperfire1"

# define tangent based on different camerae types
HF1<-tan(21/180*pi)
HF2<-tan(19/180*pi)
Brown<-tan(27.5/180*pi)
data$fov[data$model=="Browning"]<-Brown
data$fov[data$model=="Hiperfire2"]<-HF2
data$fov[data$model=="Hiperfire1"]<-HF1

# merge height
colnames(data)[9] <- "location"
data <- left_join(data, cam_height, by="location")
group<-c("Axis axis", "Axis porcinus","Canis aureus","Elephas maximus","Hystrix indica","Macaca mulatta","Muntiacus muntjak","Panthera tigris","Rhinoceros unicornis","Rusa unicolor","Semnopithecus schistaceus","Sus scrofa")
data <- data[data$scientificName%in%group,]

# from pixel to meters (coordinates)
data$x_meters<-(data$x/640*(data$exifimagewidth)-data$exifimagewidth/2)*(data$height*(data$fov)*2/data$exifimagewidth)
data$y_meters<-((data$exifimageheight-data$y/640*data$exifimagewidth)-data$exifimageheight/2)*(data$height*(data$fov)*2/data$exifimagewidth)

# get rid of the NAs from other model of cameras
data <- na.omit(data)

# now speed estimate

# funtion for harmonic mean
# library(devtools)
# install_github("MarcusRowcliffe/sbd", repos = NULL, type = "source")

library(sbd)
hmean <- function(x){
  mn <- 1/mean(1/x) # definition of harmonic mean
  se <- mn^2 * sqrt(var(1/x)/length(x))
  c(mean=mn, se=se)
}

# get rid of the duplicated rows 
data <- unique(data)

# get rid of the single point
data <- data %>% group_by(sequence_id)%>%mutate(sequence_points=length(sequence_id))
# data_coordinates <- data%>%group_by(sequence_id)%>%mutate(x_meters = x_meters[1], y_meters = y_meters[1])
# data_coordinates <- data_coordinates[,c("model","scientificName","height","x_meters","y_meters")]
# data_coordinates <- unique(data_coordinates)
# write.csv(data_coordinates, "./coordinates_meter_all.csv") # for profile calculation
data <- data[!data$sequence_points==1,] 

# keep the last four values of the filname to specify the order of the picture within sequences
data$filename <- str_sub(data$filename, 20, 23)

# keep the information of H M S only and transform the format
data$timestamp <- str_sub(data$timestamp, 12, 19)

# get start and end time per sequence and calculate the time difference
data <- data%>%group_by(sequence_id)%>%mutate(max=max(timestamp)) 
data <- data%>%group_by(sequence_id)%>%mutate(min=min(timestamp)) 
data$max <- strptime(data$max, "%H:%M:%S") # change format to calculate the time difference in seconds later
data$min <- strptime(data$min, "%H:%M:%S")
data <- data%>%group_by(sequence_id)%>%mutate(timediff=difftime(max, min, units = "secs"))
data$timediff[data$timediff==0] <- 1 # set those 0 difference in time to 1 second

# calculate the distance between two points and sum them up within one sequence 
data <- data %>%
  group_by(sequence_id) %>%
  mutate(distance_eachtwopoints = sqrt((x_meters - lag(x_meters))^2 + (y_meters - lag(y_meters))^2))

data$distance_eachtwopoints <- ifelse(is.na(data$distance_eachtwopoints), 0, data$distance_eachtwopoints)
# replace NA with 0 for the sumup later

# distance sum up
data <- data%>%group_by(sequence_id)%>%mutate(distance_persequence=sum(distance_eachtwopoints))

# finally the original speed per sequence
data$distance_persequence <- as.numeric(data$distance_persequence)
data$timediff <- as.numeric(data$timediff, units = "secs")
data$speed <- data$distance_persequence/data$timediff

data <- subset(data, speed>0.01 & speed<10)

# clean up dataset
table(data$scientificName)
data <- data[,c("scientificName","sequence_id","speed")]
data <- unique(data)
table(data$scientificName) # jackal actually not enough but, use it for now

# final calculation with harmonic mean
data <- data%>%group_by(scientificName)%>%mutate(hmean_speed=hmean(speed)[1])
data <- data%>%group_by(scientificName)%>%mutate(hmean_speed_se=hmean(speed)[2])

speed <- unique(data[,c(1,4,5)])

# merge with group size
group_size <- as.data.frame(table(data$scientificName))
colnames(group_size) <- c("scientificName", "group_size")
speed <- left_join(speed, group_size, by = "scientificName")
  
write.csv(speed,"./speed.csv")

