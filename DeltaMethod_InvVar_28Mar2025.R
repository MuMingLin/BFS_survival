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
# Keep unique rows based on key survival variables
phi_estimates_unique <- phi_estimates %>%
  select(estimate, se, lcl, ucl, region, Year, season, parameter, a04) %>%
  distinct()
nrow(phi_estimates_unique) 

# Split into W-S (winter-to-summer) and S-W (summer-to-winter) survival estimates
ws_data <- phi_estimates_unique %>% filter(season == "W-S")
sw_data <- phi_estimates_unique %>% filter(season == "S-W")

#### Remove Extra W-S Estimates ####
# Find and remove the earliest W-S estimates per region and age class
extra_ws <- ws_data %>%
  group_by(region, a04) %>%
  filter(Year == min(Year)) 

ws_data_clean <- ws_data %>%
  anti_join(extra_ws, by = c("region", "Year", "a04")) 

# Verify that W-S and S-W now have equal sample sizes
nrow(ws_data_clean)  # Should match nrow(sw_data)
nrow(sw_data)

#### Pair W-S and S-W Estimates and Compute Annual Survival ####
# Pair W-S and S-W estimates by region, year, and age class
paired_data <- ws_data_clean %>%
  inner_join(sw_data, by = c("region", "Year", "a04"), suffix = c("_WS", "_SW")) %>%
  mutate(Annual_Survival = estimate_WS * estimate_SW)

head(paired_data)

#### Extract Variance-Covariance Matrix for Paired Estimates ####
# Identify indices for paired W-S and S-W estimates in the variance-covariance matrix
ws_indices <- match(paired_data$estimate_WS, phi_estimates$estimate)
sw_indices <- match(paired_data$estimate_SW, phi_estimates$estimate)

# Extract the corresponding submatrix
var_cov_matrix_annual <- var_cov_matrix_phi[c(ws_indices, sw_indices), c(ws_indices, sw_indices)]
dim(var_cov_matrix_annual)  # Should be (n_pairs x n_pairs)

#### Compute Variance Using the Delta Method ####
# Initialize variance vector
annual_variance <- numeric(nrow(paired_data))

# Loop through each paired estimate to compute variance
for (i in seq_along(ws_indices)) {
  phi1 <- paired_data$estimate_WS[i]  # W-S
  phi2 <- paired_data$estimate_SW[i]  # S-W
  
  sub_matrix <- var_cov_matrix_annual[c(i, i + nrow(paired_data)), c(i, i + nrow(paired_data))]
  
  D <- c(phi2, phi1)  # Partial derivatives vector
  annual_variance[i] <- t(D) %*% sub_matrix %*% D
}

# Store variance and standard error in paired data
paired_data <- paired_data %>%
  mutate(Variance = annual_variance, SE = sqrt(annual_variance))

head(paired_data)

#### Compute Overall Annual Survival ####
# Compute inverse-variance weighted annual survival estimate
weights <- 1 / paired_data$Variance
average_annual_survival <- sum(weights * paired_data$Annual_Survival) / sum(weights)
average_variance <- 1 / sum(weights)
average_se <- sqrt(average_variance)

# Compute 95% confidence interval
z_value <- 1.96  
ci_lower <- average_annual_survival - z_value * average_se
ci_upper <- average_annual_survival + z_value * average_se

# Output results
annual_survival_results <- list(
  overall_annual_survival = average_annual_survival,
  variance = average_variance,
  standard_error = average_se,
  confidence_interval = c(ci_lower, ci_upper)
)

print(annual_survival_results)

#### Compare Empirical and Delta Method Variance ####
empirical_variance <- var(paired_data$Annual_Survival)

cat("Empirical Variance:", empirical_variance, "\n")
cat("Delta Method Variance:", average_variance, "\n")

#### Compute Seasonal Survival Estimates ####

# Compute inverse-variance weighted W-S survival estimate
ws_weights <- 1 / ws_data_clean$se^2  
ws_weighted_mean <- sum(ws_weights * ws_data_clean$estimate) / sum(ws_weights)
ws_variance_weighted <- 1 / sum(ws_weights)
ws_se_weighted <- sqrt(ws_variance_weighted)

# Compute inverse-variance weighted S-W survival estimate
sw_weights <- 1 / sw_data$se^2  
sw_weighted_mean <- sum(sw_weights * sw_data$estimate) / sum(sw_weights)
sw_variance_weighted <- 1 / sum(sw_weights)
sw_se_weighted <- sqrt(sw_variance_weighted)

# Store and display seasonal survival estimates
seasonal_survival_results <- list(
  ws_survival = ws_weighted_mean,
  ws_se = ws_se_weighted,
  ws_ci = c(ws_weighted_mean - 1.96 * ws_se_weighted, ws_weighted_mean + 1.96 * ws_se_weighted),
  
  sw_survival = sw_weighted_mean,
  sw_se = sw_se_weighted,
  sw_ci = c(sw_weighted_mean - 1.96 * sw_se_weighted, sw_weighted_mean + 1.96 * sw_se_weighted)
)

# Display seasonal survival results
print(seasonal_survival_results)

#### Compute Regional Survival Estimates ####

# Function to compute inverse-variance weighted survival for each region
compute_weighted_survival <- function(region_data) {
  region_weights <- 1 / region_data$se^2  
  region_weighted_mean <- sum(region_weights * region_data$estimate) / sum(region_weights)
  region_variance_weighted <- 1 / sum(region_weights)
  region_se_weighted <- sqrt(region_variance_weighted)
  
  return(list(
    survival = region_weighted_mean,
    se = region_se_weighted,
    ci = c(region_weighted_mean - 1.96 * region_se_weighted, region_weighted_mean + 1.96 * region_se_weighted)
  ))
}

# Compute regional survival estimates for each wintering region (J, S, T)
regional_survival_results <- list(
  japan = compute_weighted_survival(phi_estimates_unique %>% filter(region == "J")),
  south_china = compute_weighted_survival(phi_estimates_unique %>% filter(region == "S")),
  taiwan = compute_weighted_survival(phi_estimates_unique %>% filter(region == "T"))
)

# Display regional survival results
print(regional_survival_results)

#### Plotting Regional Survival ####
# Load necessary libraries
library(ggplot2)
library(scales)  # For formatting y-axis labels

# Create the interval plot
ggplot(data.frame(Region = c("Japan", "South China", "Taiwan")), aes(x = Region)) +
  geom_point(aes(y = c(regional_survival_results$japan$survival,
                       regional_survival_results$south_china$survival,
                       regional_survival_results$taiwan$survival)), 
             size = 3) +
  geom_errorbar(aes(ymin = c(regional_survival_results$japan$ci[1],
                             regional_survival_results$south_china$ci[1],
                             regional_survival_results$taiwan$ci[1]),
                    ymax = c(regional_survival_results$japan$ci[2],
                             regional_survival_results$south_china$ci[2],
                             regional_survival_results$taiwan$ci[2])),
                width = 0.08) +
  labs(title = "Regional Survival Estimates with 95% CI",
       x = "Wintering Region",
       y = "Survival Probability") +
  theme_bw() +  # Use theme_bw() for a clean black-and-white theme
  scale_y_continuous(limits = c(0.8, 0.9), labels = number_format(accuracy = 0.01)) +  # Set y-axis range & format
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")  # Center title
  )
