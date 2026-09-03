# Create the heart health dataset
set.seed(12345)
n <- 150

# Generate patient demographics
patient_id <- 1:n
age <- round(runif(n, 25, 65))
gender <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.52, 0.48))

# Generate BMI with gender differences
bmi_base <- rnorm(n, 25, 4)
bmi <- ifelse(gender == "Male", bmi_base + 1, bmi_base)
bmi <- pmax(18, pmin(40, bmi))

# Generate blood pressure (age-dependent)
systolic_bp <- 90 + 0.8 * age + rnorm(n, 0, 12)
diastolic_bp <- 60 + 0.4 * age + rnorm(n, 0, 8)

# Generate cholesterol (age and gender dependent)
cholesterol_base <- 160 + 1.2 * age + rnorm(n, 0, 30)
cholesterol <- ifelse(gender == "Male", cholesterol_base + 15, cholesterol_base)

# Generate exercise frequency
exercise_freq <- sample(0:7, n, replace = TRUE, prob = c(0.1, 0.1, 0.15, 0.2, 0.2, 0.15, 0.05, 0.05))

# Generate smoking status
smoking_status <- sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2))

# Generate heart disease (influenced by multiple factors)
heart_disease_prob <- 0.05 + 
	0.002 * (age - 25) + 
	0.01 * pmax(0, bmi - 25) + 
	0.001 * pmax(0, systolic_bp - 120) + 
	0.0005 * pmax(0, cholesterol - 200) + 
	ifelse(smoking_status == "Current", 0.15, ifelse(smoking_status == "Former", 0.05, 0)) -
	0.02 * exercise_freq

heart_disease_prob <- pmax(0.01, pmin(0.8, heart_disease_prob))
heart_disease <- ifelse(runif(n) < heart_disease_prob, "Yes", "No")

# Create dataset
heart_health <- data.frame(
	patient_id, age, gender, bmi, systolic_bp, diastolic_bp, 
	cholesterol, exercise_freq, smoking_status, heart_disease,
	stringsAsFactors = TRUE
)

# Round numeric values
heart_health$bmi <- round(heart_health$bmi, 1)
heart_health$systolic_bp <- round(heart_health$systolic_bp)
heart_health$diastolic_bp <- round(heart_health$diastolic_bp)
heart_health$cholesterol <- round(heart_health$cholesterol)

# Save dataset
write.csv(heart_health, "heart_health.csv", row.names = FALSE)
