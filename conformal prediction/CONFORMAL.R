rm(list = ls())
pdf('conformalpredictions.pdf')
library(reshape2)
library(vcd)             
library(ggplot2)
library(dplyr)
library(tidyr)
library(xtable)
library(modeest) 
library(dplyr)
library(caret)       
library(randomForest) 
library(purrr)


# Set working directory to the folder where adult.data is located (change this path to yours)
setwd("C:/Users/jeane/Documents/conformal/census")

# Read  and combine the dataset
adult_data <- read.csv("adult.data", header = FALSE)
adult_test<-read.csv("adult.test",header=FALSE)
adult_test <- adult_test[-1, ]
adult<- rbind(adult_data, adult_test)

column_names <- c("age", "workclass", "fnlwgt", "education", "education-num", 
                  "marital-status", "occupation", "relationship", "race", "sex", 
                  "capital-gain", "capital-loss", "hours-per-week", "native-country", "income")

# Assigner les noms de colonnes
colnames(adult) <- column_names
#RECODER INCOME en 0 et 1 selon le seuil de 50K
adult$income<- trimws(adult$income)  # Removes spaces
adult$income <- ifelse(adult$income == "<=50K", 0, 1)

unique(adult$income)
#DONNEES MANQUANTES
any(is.na(adult))
#TYPOLOGIES DES VARIABLES 
str(adult)
unique(adult$workclass)
unique(adult$education)
unique(adult$`marital-status`)
unique(adult$occupation)
unique(adult$education)
unique(adult$relationship)
unique(adult$race)
unique(adult$sex)
unique(adult$`native-country`)
#percentage of missing values
View(adult$workclass)
ppa<-sum(adult$workclass == " ?") / nrow(adult) * 100
print(ppa)
ppo<-sum(adult$occupation == " ?") / nrow(adult) * 100
print(ppo)

# Convert the specified variables to factors
# --- Step 1: Convert relevant columns to factors ---
factor_columns <- c("workclass", "education", "marital-status", "occupation",
                    "relationship", "race", "sex", "native-country", "income")

adult <-adult%>%
  mutate(across(all_of(factor_columns), as.factor))

# View structure to confirm conversion
str(adult)

# --- Step 2: Identify and visualize missing values coded as " ?" ---
# Columns to check for " ?" entries
missing_columns <- c("workclass", "occupation", "native-country")

# Compute percentage of " ?" in each column
missing_percentages <- sapply(adult[missing_columns], function(col) {
  mean(col == " ?") * 100
})

# Convert to dataframe for plotting
missing_df <- tibble(
  Variable = names(missing_percentages),
  Percentage = as.numeric(missing_percentages)
)

# Bar plot of missing value percentages
ggplot(missing_df, aes(x = Variable, y = Percentage, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Pourcentage de valeurs manquantes par variable",
    x = "", y = "Pourcentage (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# --- Step 3: Compute missing data per row ---
adult<- adult%>%
  rowwise() %>%
  mutate(
    missing_count = sum(c_across(all_of(missing_columns)) == " ?"),
    missing_pct = (missing_count / length(missing_columns)) * 100
  ) %>%
  ungroup()

# Keep only rows with missing data
rows_with_missing <- adult%>%
  filter(missing_count > 0)

# Group rows by missing percentage
grouped_missing <- rows_with_missing %>%
  count(missing_pct, name = "row_count")

# Plot number of rows per missing percentage
ggplot(grouped_missing, aes(x = missing_pct, y = row_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Nombre de lignes par pourcentage de valeurs manquantes",
    x = "Pourcentage de valeurs manquantes (%)",
    y = "Nombre de lignes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Step 4: Impute missing values using the most frequent value (Mode) ---
# Helper function to compute mode
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Impute each variable
for (var in missing_columns) {
  adult[[var]] <- as.character(adult[[var]])
  adult[[var]][adult[[var]] == " ?"] <- NA
  
  mode_value <- get_mode(adult[[var]])
  adult[[var]][is.na(adult[[var]])] <- mode_value
}

# Optionally convert imputed columns back to factors
adult <- adult %>%
  mutate(across(all_of(missing_columns), as.factor))


str(adult)
#STATISTIQUES DESCRIPTIVES 
factor_vars <- names(Filter(is.factor, adult))
pval_matrix <- matrix(NA, nrow = length(factor_vars), ncol = length(factor_vars),
                      dimnames = list(factor_vars, factor_vars))
#Fill the matrix with p-values
for (i in seq_along(factor_vars)) {
  for (j in seq_along(factor_vars)) {
    var1 <- factor_vars[i]
    var2 <- factor_vars[j]
    if (i <= j) {
      tbl <- table(adult[[var1]], adult[[var2]])
      if (all(dim(tbl) > 1)) {
        pval <- tryCatch(chisq.test(tbl)$p.value, error = function(e) NA)
      } else {
        pval <- NA
      }
      pval_matrix[i, j] <- pval
      pval_matrix[j, i] <- pval  # Ensure symmetry
    }
  }
}

#Round and convert to data.frame for LaTeX
pval_df <- as.data.frame(round(pval_matrix, 4))

#Convert to LaTeX
pval_latex <- xtable(pval_df,
                     caption = "Matrice des p-values du test du Chi-deux entre les variables catégorielles",
                     label = "tab:chi_squared_matrix",
                     align = c("l", rep("r", ncol(pval_df))))

#Print LaTeX table
print(pval_latex, include.rownames = TRUE, caption.placement = "top")

cramer_matrix <- matrix(NA, nrow = length(factor_vars), ncol = length(factor_vars),
                        dimnames = list(factor_vars, factor_vars))

for (i in seq_along(factor_vars)) {
  for (j in seq_along(factor_vars)) {
    tbl <- table(adult[[factor_vars[i]]], adult[[factor_vars[j]]])
    cramer_matrix[i, j] <- assocstats(tbl)$cramer
  }
}

# Melt and plot
cramer_df <- melt(cramer_matrix, na.rm = TRUE)
ggplot(cramer_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Matrice de Cramér's V", x = "", y = "", fill = "Association")

###QUANTITATIVE VARIABLES
numeric_vars <- adult %>% select(where(is.numeric))
# Function to calculate summary stats
summary_list <- lapply(names(numeric_vars), function(var) {
  values <- numeric_vars[[var]]
  data.frame(
    Variable = var,
    Mean  = mean(values, na.rm = TRUE),
    SD    = sd(values, na.rm = TRUE),
    Min   = min(values, na.rm = TRUE),
    Max   = max(values, na.rm = TRUE),
    Mode  = as.numeric(mfv(values, na_rm = TRUE)[1])  # safe scalar
  )
})

# Combine all rows into a single data frame
summary_long <- do.call(rbind, summary_list)

# Step 3: Round values
summary_long <- summary_long %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Step 4: Create and print LaTeX table
latex_table <- xtable(summary_long,
                      caption = "Statistiques descriptives des variables numériques",
                      label = "tab:summary_stats")

print(latex_table, include.rownames = FALSE, sanitize.text.function = identity)


###Proportions with income
# List of variables of interest
cat_vars <- c("sex", "race", "occupation", "marital-status")
get_proportions <- function(var) {
  adult %>%
    group_by(.data[[var]], income) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(income) %>%
    mutate(Proportion = round(100 * n / sum(n), 2)) %>%
    rename(Modalité = !!var)
}

# Apply to each variable and bind results
prop_tables <- lapply(cat_vars, function(v) {
  df <- get_proportions(v)
  df$Variable <- v
  df
})

# Combine all into one data frame
final_table <- bind_rows(prop_tables) %>%
  select(Variable, Modalité, income, Proportion) %>%
  pivot_wider(names_from = income, values_from = Proportion, values_fill = 0)

# Optional: Reorder columns for readability
final_table <- final_table %>% select(Variable, Modalité, everything())

# Generate LaTeX table
latex_table <- xtable(final_table,
                      caption = "Proportions des modalités par niveau de revenu",
                      label = "tab:modalities_by_income")

# Print the LaTeX table code
print(latex_table, include.rownames = FALSE, sanitize.text.function = identity)

###Least Ambiguous Classifier
set.seed(4)
data <- adult %>% select(-missing_count, -missing_pct)
names(data)
names(data) <- gsub("education-num", "educationnum", names(data))
names(data) <- gsub("marital-status", "maritalstatus", names(data))
names(data) <- gsub("capital-gain", "capitalgain", names(data))
names(data) <- gsub("capital-loss", "capitalloss", names(data))
names(data) <- gsub("hours-per-week", "hoursperweek", names(data))
names(data) <- gsub("native-country", "nativecountry", names(data))
# 2. Split into training (80%) and remaining (20%)
train_index <- createDataPartition(data$income, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
remaining_data <- data[-train_index, ]

# 3. Split remaining 20% into calibration (10%) and test (10%)
calib_index <- createDataPartition(remaining_data$income, p = 0.5, list = FALSE)
calib_data <- remaining_data[calib_index, ]
test_data <- remaining_data[-calib_index, ]

# 4. Train a model (Random Forest example)
model <- randomForest(income ~ ., data = train_data, ntree = 300, probability = TRUE)
names(train_data)


# 5. Predict probabilities for calibration and test sets
calib_probs <- predict(model, calib_data, type = "prob")
test_probs <- predict(model, test_data, type = "prob")

# 6. Compute LAC conformity scores for calibration
true_labels <- calib_data$income
conformity_scores <- 1 - map2_dbl(seq_along(true_labels), true_labels, 
                                  ~ calib_probs[.x, as.character(.y)])

# Define alpha values
# Define alpha values
# Define alpha values
alpha_values <- c(0.05, 0.1, 0.15, 0.2)

for (alpha in alpha_values) {
  
  #Compute threshold
  threshold <- quantile(conformity_scores, probs = 1 - alpha, type = 1)
  
  #Build prediction sets
  prediction_sets <- apply(test_probs, 1, function(probs) {
    names(probs)[which(1 - probs <= threshold)]
  })
  
  #Build data frame for results
  results_df <- data.frame(
    conformity_score = conformity_scores,
    set_size = sapply(prediction_sets, length),
    covered = mapply(function(set, label) label %in% set, prediction_sets, test_data$income)
  )
  
  # Count most frequent conformity scores
  top_scores <- results_df %>%
    count(conformity_score) %>%
    arrange(desc(n)) %>%
    slice(1:5) %>%
    pull(conformity_score)
  
  #Summarise by conformity score (only top frequent ones)
  summary_by_score <- results_df %>%
    filter(conformity_score %in% top_scores) %>%
    group_by(conformity_score) %>%
    summarise(
      Avg_Set_Size = round(mean(set_size), 3),
      SD_Set_Size = round(sd(set_size), 3),
      Coverage = round(mean(covered), 3)
    ) %>%
    arrange(conformity_score)
  
  #Create LaTeX table
  latex_table <- xtable(summary_by_score,
                        caption = paste("Conformal prediction metrics for α =", alpha),
                        label = paste0("tab:cp_alpha_", gsub("\\.", "", as.character(alpha))))
  
  #Print LaTeX table
  print(latex_table, include.rownames = FALSE, sanitize.text.function = identity)
}
print(summary_by_score)
length(unique(conformity_scores))

dev.off()
getwd()







