#### Load Libraries and Data ####
library(dplyr)

# Load model-averaged real estimates (contains both survival (Phi) and detection (p) estimates)
all_estimates <- read.csv("est.mod.avg.001.comb.Jan2025.csv")

# Load variance-covariance matrix
var_cov_matrix_real <- as.matrix(read.csv("Model_Avg_Real_VCV.csv")) # this step takes a bit longer

#### Check Data Structure ####
str(all_estimates)
str(var_cov_matrix_real)
dim(var_cov_matrix_real)

#### Extract Survival (Phi) Estimates ####
# Filter for survival probability (Phi) estimates
phi_estimates <- all_estimates %>% filter(parameter == "Phi")
nrow(phi_estimates)  # length(unique(phi_estimates$par.index)) = 8415

# Identify row indices for Phi estimates in the variance-covariance matrix
phi_indices <- which(all_estimates$parameter == "Phi")
length(phi_indices)  # Should match nrow(phi_estimates)

# Extract the variance-covariance submatrix for Phi
var_cov_matrix_phi <- var_cov_matrix_real[phi_indices, phi_indices]
dim(var_cov_matrix_phi)  # Should be square

#### Remove Duplicates and Split Data by Season ####
# Find the indices of the unique season-specific phi-estimates, and remove the estimates in the first year for the w-s season
phi_indices_ws <- which(!duplicated(phi_estimates[
  phi_estimates$season == "W-S" & phi_estimates$Year!=min(phi_estimates$Year), 
  c('estimate', 'se', 'lcl', 'ucl', 'region', 'Year', 'season', 'parameter', 'a04')]))

phi_indices_ws <- which(!duplicated(phi_estimates[,c('estimate', 'se', 'lcl', 'ucl', 'region', 'Year', 'season', 'parameter', 'a04')]) & phi_estimates$season=='W-S' & phi_estimates$Year!=min(phi_estimates$Year))

phi_indices_sw <- which(!duplicated(phi_estimates[,c('estimate', 'se', 'lcl', 'ucl', 'region', 'Year', 'season', 'parameter', 'a04')]) & phi_estimates$season=='S-W')

phi_estimates[phi_indices_ws, ]
phi_estimates[phi_indices_sw, ]

# check that they are correctly aligned:
phi_estimates[phi_indices_ws, c('group','Year','a04')] == phi_estimates[phi_indices_sw, c('group','Year','a04')] 

#### Pair W-S and S-W Estimates and Compute Annual Survival ####
# Pair W-S and S-W estimates by region, year, and age class (in case they were not correctly aligned)
paired_data <- phi_estimates[phi_indices_ws,] %>%
  inner_join(phi_estimates[phi_indices_sw,], by = c("region", "Year", "a04"), suffix = c("_WS", "_SW")) %>%
  mutate(Annual_Survival = estimate_WS^0.5 * estimate_SW^0.5)

paired_data$estimate_WS == phi_estimates[phi_indices_ws,'estimate'] # check that the order remained unchanged

#### Extract Variance-Covariance Matrix for Paired Estimates ####

# Extract the corresponding submatrix
var_cov_matrix_phi_uni <- var_cov_matrix_phi[c(phi_indices_ws, phi_indices_sw), c(phi_indices_ws, phi_indices_sw)]
dim(var_cov_matrix_phi_uni)

#### Compute Variance Using the Delta Method ####
# Initialize variance vector
annual_variance <- numeric(nrow(paired_data))

# Loop through each paired estimate to compute variance
for (i in 1:length(phi_indices_ws)) {
  phi1 <- paired_data$estimate_WS[i]  # W-S
  phi2 <- paired_data$estimate_SW[i]  # S-W
  
  sub_matrix <- var_cov_matrix_annual[c(i, i + nrow(paired_data)), c(i, i + nrow(paired_data))]
  # alternative method 1 which gives the same result:
  #var_phi1 <- sub_matrix[1,1]
  #var_phi2 <- sub_matrix[2,2]
  #cov_phi1_phi2 <- sub_matrix[1,2]
  # annual_variance[i] <- (phi2^2 * var_phi1) + (phi1^2 * var_phi2) + (2 * phi1 * phi2 * cov_phi1_phi2) # alternative calculation which gives the same result
  # alternative method 2:
  # annual_variance[i] <- deltamethod(~x1*x2, mean=c(phi1,phi2), cov=sub_matrix, ses=T)^2 # alternative 2 which gives the same result
  
  D <- c(phi2, phi1)  # Partial derivatives vector
  annual_variance[i] <- t(D) %*% sub_matrix %*% D
}

# Store variance and standard error in paired data
paired_data <- paired_data %>%
  mutate(Variance = annual_variance, SE = sqrt(annual_variance))

head(paired_data)

#### Compute Overall Annual Survival using the geometric mean and the Delta Method ####
### using Inverse-variance weighted annual survival may not be valid as for this, the estimates need to be independent, which they are not as they are estimated from (partly) the same individuals. Therefore, one should also account for covariance.
### However, it is a nice method that gives more weight to estimates with lower uncertainty when calculating the mean. 

### Nevertheless, I will try to get an overall estimate of annual survival from all the seasonal survival estimates using the Delta Method
### Averaging over age-specific, seasonal or regional differences using the geometric mean:

# first compare methods with only three estimates:
phi3 = paired_data$estimate_WS[1:3]
vcv3 = var_cov_matrix_phi_uni[1:3,1:3]
n <- length(phi3)
# method 1:
phi_geom = prod(phi3)^(1/n)
phi_geom^2*sum((1/(n^2*outer(phi3, phi3))) * vcv3)
# method 2 using the deltamethod function (which requires writing out ~(x1*x2...*x95*x96) entirely so not ideal):
deltamethod(~(x1*x2*x3)^(1/n), mean=phi3, cov=vcv3)^2
# both methods give the same estimate of variance

z_value <- 1.96  

# function to calculate overall and region-specific seasonal and annual survival estimates:

retrieve.regional.estimates <- function(region = NULL) {
  # if no region is specified, use all estimates from paired data, otherwise only the region-specific estimates:
  if (is.null(region)) indices_paired_data = 1:dim(paired_data)[1] else indices_paired_data <- which(paired_data$region==region)
  WS_estimates <- paired_data$estimate_WS[indices_paired_data] 
  SW_estimates <- paired_data$estimate_SW[indices_paired_data] 
  var_cov_matrix_WS = var_cov_matrix_phi_uni[indices_paired_data,indices_paired_data]
  var_cov_matrix_SW = var_cov_matrix_phi_uni[indices_paired_data+96,indices_paired_data+96]
  # calculate WS survival
  n <- length(WS_estimates)
  phiWS_geom = prod(WS_estimates)^(1/n)
  var_phiWS_geom <- phiWS_geom^2*sum((1/(n^2*outer(WS_estimates, WS_estimates))) * var_cov_matrix_WS)
  se_phiWS_geom <- sqrt(var_phiWS_geom)
  phiWS_lower <- phiWS_geom - z_value * se_phiWS_geom
  phiWS_upper <- phiWS_geom + z_value * se_phiWS_geom
  WS = round(c(phiWS_geom^0.5, phiWS_lower^0.5, phiWS_upper^0.5), 3) 
  # calculate SW survival
  n <- length(SW_estimates)
  phiSW_geom = prod(SW_estimates)^(1/n)
  var_phiSW_geom <- phiSW_geom^2*sum((1/(n^2*outer(SW_estimates, SW_estimates))) * var_cov_matrix_SW)
  se_phiSW_geom <- sqrt(var_phiSW_geom)
  phiSW_lower <- phiSW_geom - z_value * se_phiSW_geom
  phiSW_upper <- phiSW_geom + z_value * se_phiSW_geom
  SW = round(c(phiSW_geom^0.5, phiSW_lower^0.5, phiSW_upper^0.5), 3) 
  # calculate annual survival
  seasonal_estimates = c(WS_estimates, SW_estimates)
  n <- length(seasonal_estimates)
  var_cov_matrix_seasonal = var_cov_matrix_phi_uni[c(indices_paired_data, indices_paired_data+96),c(indices_paired_data, indices_paired_data+96)]
  phi_geom = prod(seasonal_estimates)^(1/n)
  var_phi_geom <- phi_geom^2*sum((1/(n^2*outer(seasonal_estimates, seasonal_estimates))) * var_cov_matrix_seasonal)
  se_phi_geom <- sqrt(var_phi_geom)
  ci_lower <- phi_geom - z_value * se_phi_geom
  ci_upper <- phi_geom + z_value * se_phi_geom
  ANNUAL = round(c(phi_geom, ci_lower, ci_upper), 3) # mean, CImin, CImax of overall annual survival. 
  rbind(WS, SW, ANNUAL)
}

overall.survival = retrieve.regional.estimates()
Phi.means.Japan  <- retrieve.regional.estimates("J")
Phi.means.SouthChina  <- retrieve.regional.estimates("S")
Phi.means.Taiwan  <- retrieve.regional.estimates("T")

write.csv(round(cbind(overall.survival, Phi.means.Japan, Phi.means.SouthChina, Phi.means.Taiwan),2), 'overall.seasonal.annual.survival.csv')

#### Plotting Regional Survival ####
# Load necessary libraries
library(ggplot2)
library(scales)  # For formatting y-axis labels

# Create the interval plot
ggplot(data.frame(Region = c("Japan", "South China", "Taiwan")), aes(x = Region)) +
  geom_point(aes(y = c(Phi.means.Japan[3,1],
                       Phi.means.SouthChina[3,1],
                       Phi.means.Taiwan[3,1])), 
             size = 3) +
  geom_errorbar(aes(ymin = c(Phi.means.Japan[3,2],
                             Phi.means.SouthChina[3,2],
                             Phi.means.Taiwan[3,2]),
                    ymax = c(Phi.means.Japan[3,3],
                             Phi.means.SouthChina[3,3],
                             Phi.means.Taiwan[3,3])),
                width = 0.08) +
  labs(title = "Regional Survival Estimates with 95% CI",
       x = "Wintering Region",
       y = "Survival Probability") +
  theme_bw() +  # Use theme_bw() for a clean black-and-white theme
  scale_y_continuous(limits = c(0.7, 1.0), labels = number_format(accuracy = 0.01)) +  # Set y-axis range & format
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")  # Center title
  )
