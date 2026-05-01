
library(tidyverse)
library(psych)
library(caret)
library(skimr)
library(readxl)
library(dplyr)

suppressWarnings(
gtd_data <- read_excel("globalterrorismdb_0221dist.xlsx")
)

# First df with initial variables
gtdclean <- gtd_data %>%
  filter(iyear >= 2012) %>%
  select(
    eventid, iyear, imonth, approxdate, nkill, nwound, property, propextent, crit1, crit2,
    crit3, region, region_txt, attacktype1, attacktype2,
    attacktype3, weaptype1, weaptype2, weaptype3, targtype1,
    claimed, success, attacktype1_txt, weaptype1_txt, weaptype2_txt, weaptype3,
    extended, resolution, country, country_txt, provstate, city, latitude, longitude,
    specificity, vicinity, location, summary, doubtterr, alternative, alternative_txt,
    multiple, suicide, attacktype2_txt, attacktype3_txt,targtype1_txt, targsubtype1,
    targsubtype1_txt, corp1, target1, natlty1, natlty1_txt, gname, gname2, motive, nperps,
    claimed, ishostkid, nhostkid, nhours, ndays, ransom
  )

# Remove all NAs from Kills and Wounded
gtdclean <- gtdclean %>%
  filter(!is.na(nkill) & !is.na(nwound))

# Remove outliers
kill_thresh <- quantile(gtdclean$nkill, 0.997, na.rm = TRUE)
wound_thresh <- quantile(gtdclean$nwound, 0.997, na.rm = TRUE)

# Filtering
gtdclean <- gtdclean %>%
  filter(nkill <= kill_thresh & nwound <= wound_thresh)


# Number of data points
dim(gtdclean)
skim(gtdclean)

# Summary Stats on numeric variables
describe(numeric_vars)

# Categorical variables
categorical_vars <- gtdclean %>% select(where(is.character))
describe(categorical_vars)

# Average deaths
mean(gtdclean$nkill, na.rm = TRUE)

# Average injuries
mean(gtdclean$nwound, na.rm = TRUE)

# Max deaths (most severe attack)
max(gtdclean$nkill, na.rm = TRUE)

# Total deaths across dataset
sum(gtdclean$nkill, na.rm = TRUE)

# Summary Statistics for 2012+
byyear_stats <- gtdclean %>%
  filter(iyear >= 2012) %>%
  group_by(iyear) %>%
  summarise(
    num_attacks = n(),
    avg_injuries = mean(nwound, na.rm = TRUE), 
    avg_deaths = mean(nkill, na.rm = TRUE),
    max_deaths = max(nkill, na.rm = TRUE), 
    total_deaths = sum(nkill, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    med_injuries = round(avg_injuries, 2),
    med_deaths = round(avg_deaths, 2)
  )
# Table of Summary Stats
knitr::kable(byyear_stats, caption = "Yearly Terrorism Stats (2012+)")

# Total Deaths Graph
ggplot(byyear_stats, aes(x = iyear, y= total_deaths)) + 
  geom_line() +
  geom_point() +
  labs(
    title = "Total Deaths from Terrorist Attacks (2012+)",
    x = "Year",
    y = "Total Deaths"
  ) +
  theme_minimal()

# Average injuries graph
ggplot(byyear_stats, aes(x = iyear, y= avg_injuries)) + 
  geom_line() +
  geom_point() +
  labs(
    title = "Average Injuries from Terrorist Attacks (2012+)",
    x = "Year",
    y = "Average Injuries"
  ) +
  theme_minimal()

# Average deaths graph
ggplot(byyear_stats, aes(x = iyear, y= avg_deaths)) + 
  geom_line() +
  geom_point() +
  labs(
    title = "Average Deaths from Terrorist Attacks (2012+)",
    x = "Year",
    y = "Average Deaths"
  ) +
  theme_minimal()

# Regions clean up 
gtdclean <- gtdclean %>%
  mutate(region_name = case_when(
    region == 1  ~ "North America",
    region == 2  ~ "Central America & Caribbean",
    region == 3  ~ "South America",
    region == 4  ~ "East Asia",
    region == 5  ~ "Southeast Asia",
    region == 6  ~ "South Asia",
    region == 7  ~ "Central Asia",
    region == 8  ~ "Western Europe",
    region == 9  ~ "Eastern Europe",
    region == 10 ~ "Middle East & North Africa",
    region == 11 ~ "Sub-Saharan Africa",
    region == 12 ~ "Australasia & Oceania",
    TRUE ~ "Other"
  ))

# Region variables 
region_year_stats <- gtdclean %>%
  group_by(iyear, region_name) %>%
  summarise(
    total_deaths = sum(nkill, na.rm = TRUE),
    avg_deaths   = mean(nkill, na.rm = TRUE),
    avg_injuries = mean(nwound, na.rm = TRUE),
    n_attacks    = n(),
    .groups = "drop"
  )


# Graph for half of regions
region_plot1 <- region_year_stats %>%
  filter(region_name %in% c(
    "North America",
    "Central America & Caribbean",
    "South America",
    "East Asia",
    "Southeast Asia",
    "South Asia"
  ))

# Graph for second half regions
region_plot2 <- region_year_stats %>%
  filter(region_name %in% c(
    "Central Asia",
    "Western Europe",
    "Eastern Europe",
    "Middle East & North Africa",
    "Sub-Saharan Africa",
    "Australasia & Oceania"
  ))

# Average Deaths Plots
ggplot(region_plot1, aes(x = iyear, y = avg_deaths, color = region_name)) +
  geom_line() +
  labs(title = "Terrorism Average Deaths (Regions Group 1)") +
  theme(legend.text = element_text(size = 6))


ggplot(region_plot2, aes(x = iyear, y = avg_deaths, color = region_name)) +
  geom_line() +
  labs(title = "Terrorism Average Deaths (Regions Group 2)") +
  theme(legend.text = element_text(size = 6))


#Average Injuries Plots
ggplot(region_plot1, aes(x = iyear, y = avg_injuries, color = region_name)) +
  geom_line() +
  labs(title = "Terrorism Average Injuries (Regions Group 1)") +
  theme(legend.text = element_text(size = 6))

ggplot(region_plot2, aes(x = iyear, y = avg_injuries, color = region_name)) +
  geom_line() +
  labs(title = "Terrorism Average Injuries (Regions Group 2)") +
  theme(legend.text = element_text(size = 6))


#Attack frequency plots
ggplot(region_plot1, aes(x = iyear, y = n_attacks, color = region_name)) +
  geom_line() +
  labs(title = "Attack Frequency (Regions Group 1)") +
  theme(legend.text = element_text(size = 6))

ggplot(region_plot2, aes(x = iyear, y = n_attacks, color = region_name)) +
  geom_line() +
  labs(title = "Attack Frequency (Regions Group 2)") +
  theme(legend.text = element_text(size = 6))

# Attack frequency (only general categories)
ggplot(gtd_data, aes(x = reorder(attacktype1_txt, attacktype1_txt, length))) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Frequency of Attack Types",
    x = "Attack Type",
    y = "Count"
  ) +
  theme_minimal()

# Weapon types
top_weapons <- gtdclean %>%
  count(weaptype1_txt, sort = TRUE) %>%
  slice_head(n = 10)

top_weapons <- gtdclean %>%
  mutate(weaptype1_txt = ifelse(
    weaptype1_txt == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", 
    "Vehicle",
    weaptype1_txt
  )) %>%
  count(weaptype1_txt, sort = TRUE) %>%
  slice_head(n = 10)

# Top weapon types graph 
ggplot(top_weapons, aes(x = reorder(weaptype1_txt, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Weapon Types",
    x = "Weapon Type",
    y = "Number of Attacks"
  ) +
  theme_minimal()

# Severity measure
attack_severity <- gtdclean %>%
  group_by(attacktype1_txt) %>%
  summarise(avg_deaths = mean(nkill, na.rm = TRUE)) %>%
  arrange(desc(avg_deaths)) %>%
  slice_head(n = 10)

# Severity graph 
ggplot(attack_severity, aes(x = reorder(attacktype1_txt, avg_deaths), y = avg_deaths)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Lethal Attack Types (Avg Deaths)",
    x = "Attack Type",
    y = "Average Deaths"
  ) +
  theme_minimal()


#Modeling prep (NOT USED)
gtd_mod2 <- gtdclean %>%
  mutate(severity_index = 2 * nkill + nwound) %>%
  select(
    attacktype1_txt,
    weaptype1_txt,
    targtype1_txt,
    region_txt,
    success,
    severity_index,
    multiple,        
    claimed,         
    ishostkid,       
    ransom,          
    extended    
  ) %>%
  drop_na() %>%
  mutate(across(c(attacktype1_txt, weaptype1_txt, 
                  targtype1_txt, region_txt), as.factor))


set.seed(123)

train_index <- createDataPartition(gtd_mod2$severity_index, p = 0.7, list = FALSE)

train <- gtd_mod2[train_index, ]
test  <- gtd_mod2[-train_index, ]

# Fix: align test factor levels to exactly match train
for (col in c("attacktype1_txt", "weaptype1_txt", "targtype1_txt", "region_txt")) {
  test[[col]] <- factor(test[[col]], levels = levels(train[[col]]))
}

# Drop any test rows with factor levels that didn't exist in train
test <- test %>% drop_na(attacktype1_txt, weaptype1_txt, targtype1_txt, region_txt)

# Linear Regression
lm_mod <- lm(severity_index ~ ., data = train)
summary(lm_mod)

lm_pred <- predict(lm_mod, test)

lm_rmse <- RMSE(lm_pred, test$severity_index)
lm_r2   <- R2(lm_pred,   test$severity_index)


cat("Linear Regression — RMSE:", lm_rmse, " R²:", lm_r2, "\n")

levels(test$region_txt)[!levels(test$region_txt) %in% levels(train$region_txt)]

# Ridge and Lasso
library(glmnet)

# Build matrices
x_train <- model.matrix(severity_index ~ ., data = train)[, -1]
y_train  <- train$severity_index

x_test   <- model.matrix(severity_index ~ ., data = test)[, -1]
y_test   <- test$severity_index

# Ridge (alpha = 0) 
ridge_cv    <- cv.glmnet(x_train, y_train, alpha = 0)
ridge_pred  <- predict(ridge_cv, s = "lambda.min", newx = x_test)
ridge_rmse  <- RMSE(as.vector(ridge_pred), y_test)
ridge_r2    <- R2(as.vector(ridge_pred),   y_test)

cat("Ridge — RMSE:", ridge_rmse, " R²:", ridge_r2, "\n")

# Lasso (alpha = 1) 
lasso_cv    <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_pred  <- predict(lasso_cv, s = "lambda.min", newx = x_test)
lasso_rmse  <- RMSE(as.vector(lasso_pred), y_test)
lasso_r2    <- R2(as.vector(lasso_pred),   y_test)

cat("Lasso — RMSE:", lasso_rmse, " R²:", lasso_r2, "\n")

# Results 
results <- data.frame(
  Model = c("Linear Regression", "Ridge", "Lasso"),
  RMSE  = c(lm_rmse, ridge_rmse, lasso_rmse),
  R2    = c(lm_r2,   ridge_r2,   lasso_r2)
) %>%
  arrange(RMSE)   # best model at top

knitr::kable(results, digits = 3, caption = "Model Comparison: Predicting Severity Index")











