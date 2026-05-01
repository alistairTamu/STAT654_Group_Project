# Classes used for Casualty measures
classes <- c("No Casualty", "Low", "Medium", "High")

# Measures used
make_class_table <- function(precision, recall, f1, support, model_name) {
  tibble(
    Class     = classes,
    Precision = precision,
    Recall    = recall,
    F1        = f1,
    Support   = support
  ) %>%
    knitr::kable(caption = paste("Classification Report —", model_name))
}

# Table for Logistic
make_class_table(
  precision = c(0.77, 0.68, 0.42, 0.20),
  recall    = c(0.72, 0.50, 0.41, 0.57),
  f1        = c(0.75, 0.58, 0.41, 0.29),
  support   = c(5641, 6593, 3890, 1176),
  model_name = "Logistic Regression"
)

# Table for RF
make_class_table(
  precision = c(0.78, 0.68, 0.44, 0.21),
  recall    = c(0.74, 0.52, 0.41, 0.60),
  f1        = c(0.76, 0.59, 0.43, 0.31),
  support   = c(5641, 6593, 3890, 1176),
  model_name = "Random Forest"
)

# Table for XGBoost
make_class_table(
  precision = c(0.78, 0.61, 0.48, 0.40),
  recall    = c(0.76, 0.70, 0.47, 0.11),
  f1        = c(0.77, 0.65, 0.47, 0.17),
  support   = c(5641, 6593, 3890, 1176),
  model_name = "XGBoost"
)

# Table for LightGBM
make_class_table(
  precision = c(0.80, 0.68, 0.45, 0.20),
  recall    = c(0.74, 0.53, 0.36, 0.67),
  f1        = c(0.76, 0.59, 0.40, 0.31),
  support   = c(5641, 6593, 3890, 1176),
  model_name = "LightGBM"
)

# Comparison Table
comparison <- tibble(
  Model       = c("Regression RF (converted)", "Logistic Regression", 
                  "Random Forest", "XGBoost", "LightGBM"),
  Macro_F1    = c(0.360, 0.509, 0.522, 0.517, 0.517),
  Accuracy    = c(0.479, 0.558, 0.572, 0.629, 0.567)
) %>%
  arrange(desc(Accuracy))

knitr::kable(comparison,
             col.names = c("Model", "Macro F1", "Accuracy"),
             caption   = "Regression vs Classification — Final Comparison")

# Regression results table for each model
regression_results <- tibble(
  Model    = c("Linear", "Ridge", "Lasso", "Elastic Net", "RF", "XGBoost", "LightGBM"),
  RMSE_sev    = c(12.089, 12.091, 12.094, 12.136, 11.607, 11.741, 11.692),
  R2_sev      = c(0.049, 0.049, 0.049, 0.042, 0.124, 0.103, 0.111)
) 

# Organized table
knitr::kable(regression_results,
             col.names = c("Model", "RMSE", "R²"),
             caption = "Regression Model Results") %>%
  kableExtra::add_header_above(c(" " = 1, "Severity" = 2)) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width        = FALSE,
    position          = "center"
  ) %>%
  kableExtra::column_spec(1, width = "8em") %>%
  kableExtra::column_spec(2, width = "6em") %>%
  kableExtra::column_spec(3, width = "6em")

# Comparison table for all models
results_comparison <- tibble(
  Model    = c("Linear (Reg)", "Ridge (Reg)", "Lasso (Reg)", "Elastic Net (Reg)",
               "RF (Reg)", "XGBoost (Reg)", "LightGBM (Reg)", "Logistic (Clf)",
               "RF (Clf)", "XGBoost (Clf)", "LightGBM (Clf)"),
  Macro_F1 = c(0.226, 0.226, 0.226, 0.225, 0.280, 0.268, 0.272, 0.486, 0.514, 0.510, 0.520),
  Accuracy = c(0.386, 0.386, 0.386, 0.388, 0.427, 0.422, 0.425, 0.605, 0.621, 0.629, 0.629),
  R2_sev      = c(0.049, 0.049, 0.049, 0.042, 0.124, 0.103, 0.111, "N/A", "N/A", "N/A", "N/A"),
) 

knitr::kable(results_comparison,
             col.names = c("Model", "Macro_F1", "Accuracy", "R²"),
             caption = "Regression vs Classification Comparison",
             na        = "—") %>%  
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width        = FALSE,
    position          = "center"
  ) %>%
  kableExtra::column_spec(1, width = "8em") %>%
  kableExtra::column_spec(2, width = "6em") %>%
  kableExtra::column_spec(3, width = "6em") %>%
  kableExtra::column_spec(4, width = "6em")



# Classification Models 
library(kableExtra)

classification_results <- tibble(
  Model     = c(rep("Logistic Regression", 4), rep("Random Forest", 4), 
                rep("XGBoost", 4),             rep("LightGBM", 4)),
  Class     = rep(c("No Casualty (0)", "Low (1)", "Medium (2)", "High (3)"), 4),
  Precision = c(0.75, 0.59, 0.45, 0.36,
                0.77, 0.60, 0.47, 0.39,
                0.78, 0.60, 0.47, 0.41,
                0.77, 0.61, 0.48, 0.38),
  Recall    = c(0.74, 0.66, 0.48, 0.07,
                0.76, 0.69, 0.45, 0.13,
                0.76, 0.71, 0.47, 0.09,
                0.77, 0.69, 0.47, 0.12),
  F1        = c(0.74, 0.62, 0.46, 0.11,
                0.77, 0.64, 0.46, 0.19,
                0.77, 0.65, 0.47, 0.15,
                0.77, 0.65, 0.47, 0.19),
  Support   = rep(c(5641, 6593, 3890, 1176), 4)
)

# Summary row per model
model_summary <- tibble(
  Model     = c("Logistic Regression", "Random Forest", "XGBoost", "LightGBM"),
  Accuracy  = c(0.60, 0.62, 0.63, 0.63),
  Macro_F1  = c(0.49, 0.51, 0.51, 0.52),
  Weighted_F1 = c(0.59, 0.61, 0.62, 0.62)
)


knitr::kable(classification_results,
             col.names = c("Model", "Class", "Precision", "Recall", "F1", "Support"),
             caption   = "Classification Results by Model and Class") %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width        = FALSE,
    position          = "center"
  ) %>%
  kableExtra::column_spec(1, width = "10em") %>%
  kableExtra::column_spec(2, width = "9em") %>%
  kableExtra::collapse_rows(columns = 1, valign = "middle")

# Overall summary table
knitr::kable(model_summary,
             col.names = c("Model", "Accuracy", "Macro F1", "Weighted F1"),
             caption   = "Classification Model Summary") %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width        = FALSE,
    position          = "center"
  )