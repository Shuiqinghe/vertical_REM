# Function gives point estimate for density with 360 degree detection model
rem360 <- function(data, params){
  dd <- data$y * params$g/ (data$E * params$v * params$a * params$w)
  sum(dd * data$E) / sum(data$E)
}

# Function estimates density including bootstrapped errors
rem360_boot <- function(data, params, ses, reps=999){
  
  sz <- lapply(params, length)
  if(!all(sz == nrow(data) | sz == 1))                         
    stop("parameters must have length 1 or same as nrow(data)")
  if(!all(c("v", "a", "w","g") %in% names(params)) |
     !all(c("v", "a", "w","g") %in% names(ses)))
    stop("params and ses must contain elements named v, a, g, and w")
  
  # function returns one density estimate sampling locations and 
  # log-normal parameter distributions
  sample_rem360 <- function(){
    n <- nrow(data)
    i <- sample(1:n, n, replace=TRUE)
    par <- sapply(names(params), function(p){
      j <- if(sz[[p]] == 1) 1 else i
      exp(rnorm(sz[[p]], 
                log(params[[p]][j]), 
                sqrt(log(1+(ses[[p]][j]/params[[p]][j])^2))
      ))
    }, simplify = FALSE)
    rem360(data[i,], par)
  }
  
  # generate bootstrap samples and return summary results
  samp <- replicate(reps, sample_rem360())
  c(estimate = rem360(data, params),
    mean_boot = mean(samp),
    se_boot = sd(samp),
    cv_boot = sd(samp) / mean(samp),
    lcl_boot = quantile(samp, 0.025),
    ucl_boot = quantile(samp, 0.975))
}

# Example with dummy data assuming constant speed and activity level per location (height)

n <- 50 # number of camera locations

# location-specific contact and effort data
data <- data.frame(y = rnbinom(n, mu=1, size=0.5), # contacts per location
                   E = runif(n, 30, 40) # location effort
)
# auxilary parameter estimates - can be location-specific or not
params <- list(v = 7, # speed while active
               a = 0.4, # activity level
               w = runif(n, 0.003, 0.005),
               g = 1) # average profile width
# parameter standard errors (arbitrarily assumed 10% CV)
ses <- list(v = params$v * 0.1,
            a = params$a * 0.1,
            w = params$w * 0.1,
            g = params$g * 0.1)

rem360_boot(data, params, ses)

# apply the real data, instead of the example which shows species per location (height)
#rem_boot_data<-read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Macroecology project/PhD projects/Shuiqing He/REM_vertical/data/for bootstrap/boostrap_all.csv")
rem_boot_data<-read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Macroecology project/PhD projects/Shuiqing He/REM_vertical/data/for bootstrap/boostrap_all.csv")
#rem_boot_data <- rem_boot_data[,c(1,10,12,15,16,19:24)]
rem_boot_data <- rem_boot_data[,c(2,12,14,18,19,21:26)]

# List of species names
species_list <- unique(rem_boot_data$scientificName)

# Create empty lists to store results for each species (density + error)
results_list <- list()
error_list <- list()

# Loop through each species
for (species_name in species_list) {
  # Filter data for the current species
  species_data <- rem_boot_data[rem_boot_data$scientificName == species_name, ]
  
  # Extract relevant data, parameters, and standard errors
  data <- species_data[, c("effort_cameraheight_cameratype", "contact_species_camereaheight_cameratype")]
  colnames(data) <- c("E", "y")
  
  params <- species_data[, c("group_size", "hspeed_kmd", "activity_level", "profile_km")]
  colnames(params) <- c("g", "v", "a", "w")
  
  ses <- species_data[, c("al_se", "hspeed_se_kmd", "group_size_se", "profile_se_km")]
  colnames(ses) <- c("a", "v", "g", "w")
  
  # Run rem360_boot for the current species
  species_result <- rem360_boot(data, params, ses)
  
  # Store the results in the list with the species name as a key
  species_result<-as.data.frame(species_result)
  results_list[species_name] <- species_result$species_result[1]
  error_list[species_name] <- species_result$species_result[3]
}

  results <- as.data.frame(cbind(results_list,error_list))

library(tibble)
results <- rownames_to_column(results, var = "scientificName")
colnames(results) <- c("scientificName","vREM","vREM_se")

# write_csv(results, "~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Macroecology project/PhD projects/Shuiqing He/REM_vertical/data/REM_estimate_result.csv")
