# import data
dat <- read.csv("coordinates_meter.csv")
bmdat <- read.csv("body_mass.csv")

# read packages 
library(bbmle)
library(dplyr)
library(boot)

# merge two dataset
data <- left_join(dat, bmdat, by = "scientificName")

# define function for bivariate normal distribtion (oval)
dbhnorm <- function(xy, logmajor_axis, axis_ratio, log=FALSE){
  major_axis <- exp(logmajor_axis)
  minor_axis <- major_axis * axis_ratio
  res <- dnorm(abs(xy[,1]), sd=major_axis) *
    dnorm(abs(xy[,2]), sd=minor_axis) * 4
  if(log) res <- log(res)
  res
}

# set parameters and variables for Maximum likelihood function
start_par <- list(logmajor_axis = 0, axis_ratio = 0.5)
xy <- abs(data[,c("x_meters", "y_meters")])

# power-transform body mass with an exponent of 2/3
data$body_mass_expo<-data$body_mass^(2/3)

# Maximum likelihood function
mod <- mle2(xy ~ dbhnorm(logmajor_axis=logmajor_axis, axis_ratio=axis_ratio),
             data = data,
             start = start_par,
             parameters = list(logmajor_axis ~ height  + body_mass_expo + model,
                               axis_ratio ~ model))

#AIC(mod)
summary(mod)

# calculate major and minor axes according to summary result
data$major_axis_mod[data$model=="Browning"]<-exp((-0.61560626)+0.16182455*data$height[data$model=="Browning"]+0.00204013*data$body_mass_expo[data$model=="Browning"])
data$major_axis_mod[data$model=="Hiperfire1"]<-exp((-0.61560626-0.35438590)+0.16182455*data$height[data$model=="Hiperfire1"]+0.00204013*data$body_mass_expo[data$model=="Hiperfire1"])
data$major_axis_mod[data$model=="Hiperfire2"]<-exp((-0.61560626-0.26037384)+0.16182455*data$height[data$model=="Hiperfire2"]+0.00204013*data$body_mass_expo[data$model=="Hiperfire2"])
data$axis_ratio[data$model=="Browning"]<-0.36795805
data$axis_ratio[data$model=="Hiperfire1"]<-0.36795805+0.22343680
data$axis_ratio[data$model=="Hiperfire2"]<-0.36795805+0.00255791
data$minor_axis<-(data$major_axis_mod)*(data$axis_ratio) 

# clean dataset
data<-unique(data[,c(2:4,9,11)])

# calculate profile

# Create vectors for 'a' and 'b' 
a <- data$major_axis_mod
b <- data$minor_axis

# build vectors to store the results and errors
integral_results <- numeric(length(a))
integral_error <- numeric(length(a))

# define the integral function
integrand <- function(x, a, b) {
  2 * sqrt(a^2 * tan(x)^2 + b^2) * cos(x)
}

# perform the integration for each pair of 'a' and 'b' values, range is within 0 to pi/2 for the symmetry of oval
for (i in 1:length(a)) {
  result <- integrate(integrand, lower = 0, upper = pi/2, a = a[i], b = b[i])
  integral_results[i] <- result$value
  integral_error[i] <- result$abs.error
}

# add result to data
data$profile<-2*integral_results/pi

# calculate standard error using bootstrap

# set the random seed for reproducibility
set.seed(123)

# define the integral function
calculate_integral <- function(data, params) {
  bootstrap_sample <- data[params, ]
  integral_results <- bootstrap_sample$profile
  return(integral_results)
}

# perform bootstrap resampling 
bootstrap_results <- boot(data = data, statistic = calculate_integral, R = 1000)

# calculate se for each integral result and add it to dataset
se_integral_results <- apply(bootstrap_results$t, 2, sd)
data$profile_se<-se_integral_results
