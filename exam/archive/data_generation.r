# Set random seed for reproducibility
set.seed(20241001)

# Generate sample study
n_samples <- 200

# Create study frame
study <- data.frame(
  patient_id = 1:n_samples,
  age = round(rnorm(n_samples, mean = 70, sd = 8)),
  sex = sample(c("male", "female"), n_samples, replace = TRUE),
  apoe4_status = sample(0:2, n_samples, replace = TRUE, prob = c(0.6, 0.25, 0.15)),
  cognitive_score = round(rnorm(n_samples, mean = 22, sd = 5), 2),
  brain_volume = round(rnorm(n_samples, mean = 1000, sd = 50), 2),
  amyloid_beta = round(rnorm(n_samples, mean = 40, sd = 10), 2),
  tau_protein = round(rnorm(n_samples, mean = 60, sd = 15), 2),
  treatment = sample(c("placebo", "drug_a", "drug_b"), n_samples, replace = TRUE),
  months_followup = sample(c(6, 12, 18, 24), n_samples, replace = TRUE)
)

# Introduce some correlations and effects
study$cognitive_score[study$apoe4_status > 0] <- 
  study$cognitive_score[study$apoe4_status > 0] - study$apoe4_status[study$apoe4_status > 0] * 2

study$brain_volume[study$apoe4_status > 0] <- 
  study$brain_volume[study$apoe4_status > 0] - study$apoe4_status[study$apoe4_status > 0] * 10

study$amyloid_beta[study$apoe4_status > 0] <- 
  study$amyloid_beta[study$apoe4_status > 0] + study$apoe4_status[study$apoe4_status > 0] * 3

study$tau_protein[study$apoe4_status > 0] <- 
  study$tau_protein[study$apoe4_status > 0] + study$apoe4_status[study$apoe4_status > 0] * 5

study$cognitive_score[study$treatment == "drug_a"] <- 
  study$cognitive_score[study$treatment == "drug_a"] + 1.5

study$cognitive_score[study$treatment == "drug_b"] <- 
  study$cognitive_score[study$treatment == "drug_b"] + 2.5

study$amyloid_beta[study$treatment == "drug_a"] <- 
  study$amyloid_beta[study$treatment == "drug_a"] - 3

study$amyloid_beta[study$treatment == "drug_b"] <- 
  study$amyloid_beta[study$treatment == "drug_b"] - 5

# Ensure values are within realistic ranges
study$cognitive_score <- pmin(pmax(study$cognitive_score, 0), 30)
study$brain_volume <- pmin(pmax(study$brain_volume, 800), 1200)
study$amyloid_beta <- pmin(pmax(study$amyloid_beta, 0), 100)
study$tau_protein <- pmin(pmax(study$tau_protein, 0), 120)

# Remove problematic point
study = subset(study, patient_id != 90)
study$patient_id = seq(nrow(study))


# Save to CSV
write.csv(study, "data/alzheimers_study_data.csv", row.names = FALSE)

# Print head and summary of the studyset
print(head(study))
print(summary(study))
