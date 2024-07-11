library(stringr)
library(do)
library(dplyr)
library(stringr)
library(readr)
library(activity)

observation_for_species <- read.csv("observations.csv", sep="," , header = TRUE)
observation_for_pics <- read.csv("20240618-bardiya-assets.csv", sep="," , header = TRUE)
media_for_pictimes <- read.csv("media.csv", sep="," , header = TRUE)

# data merging
media_for_pictimes <- media_for_pictimes[,c(5,7)]
colnames(media_for_pictimes)[2] <- "filename"
data <- left_join(observation_for_pics, media_for_pictimes, by = "filename")
colnames(data)[2] <- "sequenceID"
data <- left_join(data, observation_for_species, by = "sequenceID")
data <- data[,c("sequenceID", "timestamp.x", "scientificName")]
colnames(data) <- c("sequenceID", "timestamp", "scientificName")

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

# select species
group<-c("Axis axis", "Axis porcinus","Canis aureus","Elephas maximus","Hystrix indica","Macaca mulatta","Muntiacus muntjak","Panthera tigris","Rhinoceros unicornis","Rusa unicolor","Semnopithecus schistaceus","Sus scrofa","Viverricula indica","Paradoxurus hermaphroditus","Viverra zibetha","Vulpes bengalensis","Herpestes javanicus","Tetracerus quadricornis","Rucervus duvaucelii","Prionailurus bengalensis","Mellivora capensis","Herpestes edwardsi","Felis chaus","Ursus thibetanus","Herpestes smithii","Caprolagus hispidus","Neofelis nebulosa","Arctictis binturong","Manis pentadactyla","Lepus nigricollis","Moschus leucogaster","Martes flavigula")
data <- subset(data, scientificName%in%group)

# check group size
data <- unique(data)
table(data$scientificName)

# get rid of species with too low sample size (<50)
data <- data[!data$scientificName%in%c("Arctictis binturong","Caprolagus hispidus","Felis chaus","Lepus nigricollis","Manis pentadactyla","Martes flavigula","Martes flavigula","Moschus leucogaster","Neofelis nebulosa","Viverra zibetha","Vulpes bengalensis"),]

# upgrade group species
group <- as.vector(unique(data$scientificName))

# loop for species in group
# List of species names
species_names <- group  # adjust as per your dataset structure

# Initialize an empty list to store results
results_list <- list()

# Loop through each species
for (species in group) {
  # Subset data for the current species
  species_data <- subset(data, scientificName == species)
  
  # Calculate solar times
  species_solar <- solartime(species_data$timestamp, 28.5415, 81.2767, 5.75)
  
  # Convert to data frame and process
  species_solar_df <- as.data.frame(species_solar$solar)
  colnames(species_solar_df) <- "time"
  
  # Remove NA values
  species_solar_df <- na.omit(species_solar_df)
  
  # Fit activity model
  species_activity_level <- fitact(species_solar_df$time, sample = "data", reps = 10)
  
  # Store results in a list with species name as key
  results_list[[species]] <- list(
    solar_time_data = species_solar_df,
    activity_level_model = species_activity_level
  )
}

# Example of accessing results for a specific species (e.g., "Axis axis")
Axis_axis_results <- results_list[["Axis axis"]]
Axis_porcinus_results <- results_list[["Axis porcinus"]]
Canis_aureus_results <- results_list[["Canis aureus"]]
Elephas_maximus_results <- results_list[["Elephas maximus"]]
Hystrix_indica_results <- results_list[["Hystrix indica"]]
Macaca_mulatta_results <- results_list[["Macaca mulatta"]]
Muntiacus_muntjak_results <- results_list[["Muntiacus muntjak"]]
Panthera_tigris_results <- results_list[["Panthera tigris"]]
Rhinoceros_unicornis_results <- results_list[["Rhinoceros unicornis"]]
Rusa_unicolor_results <- results_list[["Rusa unicolor"]]
Semnopithecus_schistaceus_results <- results_list[["Semnopithecus schistaceus"]]
Sus_scrofa_results <- results_list[["Sus scrofa"]]
# some extra added species 
Viverricula_indica_results <- results_list[["Viverricula indica"]]
Paradoxurus_hermaphroditus_results <- results_list[["Paradoxurus hermaphroditus"]]
Herpestes_javanicus_results <- results_list[["Herpestes javanicus"]]
Tetracerus_quadricornis_results <- results_list[["Tetracerus quadricornis"]]
Rucervus_duvaucelii_results <- results_list[["Rucervus duvaucelii"]]
Prionailurus_bengalensis_results <- results_list[["Prionailurus bengalensis"]]
Mellivora_capensis_results <- results_list[["Mellivora capensis"]]
Herpestes_edwardsi_results <- results_list[["Herpestes edwardsi"]]
Ursus_thibetanus_results <- results_list[["Ursus thibetanus"]]
Herpestes_smithii_results <- results_list[["Herpestes smithii"]]

# result manually stored in activity_level.csv
