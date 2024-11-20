# Import ggplot2 and dplyr for better visualization of large data sets
library(ggplot2)
library(dplyr)
library(GGally)
library(car)
library(MASS)
library(forcats)
library(boot)

df <- read.csv("combined_midwest_data_with_dup.csv")
attach(df)

dim(df)

# Show the variables 
# Response: (FATALS)
# Categorical: (RUR-URBAN), (LGT_CONDNAME), (ROUTENAME)
# Continuous:(TRAV_SP) (AGE) (HOUR) 

# Continuous Variable Plots
plot(df$FATALS)
plot(df$TRAV_SP)
plot(df$AGE)
plot(df$HOUR.x)

# Box-plots
boxplot(df$FATALS, main="Fatalities Boxplot", ylab="# Fatalities", col = "lightblue")
boxplot(df$TRAV_SP, main="Travel Speed Boxplot", ylab="Speed (mph)", col="lightblue")
boxplot(df$AGE, main="Age of Driver Boxplot", ylab="Age", col="lightblue")
boxplot(df$HOUR.x, main="Hour of Accident", ylab="Hour", col="lightblue")

# Initial Categorical Variable Plots - Bar Graphs
ggplot(df, aes(x = RUR_URBNAME.x)) +
  geom_bar() +
  labs(title = "Bar Graph of Area Classification (Rural/Urban)", x = "State Names", y = "Count") +
  theme_minimal()

ggplot(df, aes(x = LGT_CONDNAME)) +
  geom_bar() +
  labs(title = "Bar Graph of Lighting Conditions", x = "Injury Severity Name", y = "Count") +
  theme_minimal()

ggplot(df, aes(x = ROUTENAME)) +
  geom_bar() +
  labs(title = "Bar Graph of Road Type", x = "Weather Condition", y = "Count") +
  theme_minimal()

## VARIABLE VALUE CORRECTION - Combine Values for Categorical Variables ##
# ROUTENAME
df <- df %>%
  mutate(RouteCategory = case_when(
    ROUTENAME %in% c("County Road") ~ "County Road",
    ROUTENAME %in% c("Interstate") ~ "Interstate",
    ROUTENAME %in% c("Local Street - Frontage Road", "Local Street - Municipality", "Local Street - Township") ~ "Local Street",
    ROUTENAME %in% c("State Highway", "US Highway") ~ "Highway",
    TRUE ~ "Other"  # Catch-all for "Other" and "Unknown"
  ))

ggplot(df, aes(x = RouteCategory)) +
  geom_bar() +
  labs(title = "Bar Graph of Road Type", x = "Road Type", y = "Count") +
  theme_minimal()

# LGT_CONDNAME
df <- df %>%
  mutate(LightCond = case_when(
    LGT_CONDNAME %in% c("Dark - Lighted") ~ "Dark - Lighted",
    LGT_CONDNAME %in% c("Dark - Not Lighted") ~ "Dark - Not Lighted",
    LGT_CONDNAME %in% c("Daylight") ~ "Daylight",
    LGT_CONDNAME %in% c("Dusk", "Dawn") ~ "Twilight",
    TRUE ~ "Other"  # Catch-all for "Other" and "Unknown"
  ))

ggplot(df, aes(x = LightCond)) +
  geom_bar() +
  labs(title = "Bar Graph of Lighting Conditions", x = "Condition Name", y = "Count") +
  theme_minimal()

# RUR_URBNAME.x
df <- df %>%
  mutate(AreaClass = case_when(
    RUR_URBNAME.x %in% c("Rural") ~ "Rural",
    RUR_URBNAME.x %in% c("Urban") ~ "Urban",
    TRUE ~ "Other"  # Catch-all for "Other" and "Unknown"
  ))

ggplot(df, aes(x = AreaClass)) +
  geom_bar() +
  labs(title = "Bar Graph of Area Classification (Rural/Urban)", x = "Classification", y = "Count") +
  theme_minimal()

## VARIABLE VALUE CORRECTION - Remove Obvious Outliers from Continuous Variables ##
# Filter out rows in all continuous columns where the value is greater than or equal to 997
df_filtered <- df %>% filter(FATALS < 900, TRAV_SP < 900, AGE < 98, HOUR.x <= 24,
  RouteCategory != "Other",
  LightCond != "Other",
  AreaClass != "Other")

# Add sine and cosine transformations of HOUR.x
df_filtered <- df_filtered %>%
  mutate(HOUR_sin = sin(2 * pi * HOUR.x / 24),
         HOUR_cos = cos(2 * pi * HOUR.x / 24))

## SHOW CORRELATION ##
# Correlation Matrix for Continuous
subset_df <- df[c("FATALS", "TRAV_SP", "AGE", "HOUR.x")]
cor_matrix_sub <- cor(subset_df)
print(cor_matrix_sub)

# Heat Maps for Categoricals
# Route x Fatalities
route_fatals_counts <- df %>%
  count(RouteCategory, FATALS)

ggplot(route_fatals_counts, aes(x = RouteCategory, y = FATALS, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Count")

# Lighting Conditions x Fatalities
light_fatals_counts <- df %>%
  count(LightCond, FATALS)

ggplot(light_fatals_counts, aes(x = LightCond, y = FATALS, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Count")

# Area Classification x Fatalities
area_fatals_counts <- df %>%
  count(AreaClass, FATALS)

ggplot(area_fatals_counts, aes(x = AreaClass, y = FATALS, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Count")

## RESIDUAL PLOTS ##
# Convert categorical variables to dummies
df_filtered$RouteCategory <- factor(df_filtered$RouteCategory)
df_filtered$LightCond <- factor(df_filtered$LightCond)
df_filtered$AreaClass <- factor(df_filtered$AreaClass)

model <- lm(FATALS ~ RouteCategory + LightCond + AreaClass + TRAV_SP + AGE + HOUR.x, data = df_filtered)

summary(model)
crPlots(model)

# Poisson Regression Model
poisson_model <- glm(FATALS ~ RouteCategory + LightCond + AreaClass + TRAV_SP + AGE + HOUR_sin + HOUR_cos, 
                     family = poisson(link = "log"), data = df_filtered)
summary(poisson_model)

# Quasi-Poisson to Handle Overdispersion
quasi_poisson_model <- glm(FATALS ~ RouteCategory + LightCond + AreaClass + TRAV_SP + AGE + HOUR_sin + HOUR_cos, 
                           family = quasipoisson(link = "log"), data = df_filtered)
summary(quasi_poisson_model)

interaction_model <- glm(FATALS ~ RouteCategory * LightCond + AreaClass + TRAV_SP + AGE + HOUR_sin + HOUR_cos, 
                         family = quasipoisson(link = "log"), data = df_filtered)
summary(interaction_model)

nonlinear_model <- glm(FATALS ~ RouteCategory + LightCond + AreaClass + poly(TRAV_SP, 2) + poly(AGE, 2) + HOUR_sin + HOUR_cos, 
                       family = quasipoisson(link = "log"), data = df_filtered)
summary(nonlinear_model)

# Predicted vs Actual Plot
df_filtered$predicted <- predict(quasi_poisson_model, type = "response")
ggplot(df_filtered, aes(x = predicted, y = FATALS)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Fatalities", x = "Predicted Fatalities", y = "Actual Fatalities")

# Residual Plot
df_filtered$residuals <- residuals(quasi_poisson_model, type = "pearson")
ggplot(df_filtered, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Predicted for Quasi-Poisson Model", x = "Predicted Values", y = "Residuals")

# Bar Plot for RouteCategory
ggplot(df_filtered, aes(x = RouteCategory, fill = FATALS)) +
  geom_bar(position = "dodge") +
  labs(title = "Fatalities by Route Category", x = "Route Category", y = "Count") +
  theme_minimal()

exp(coef(quasi_poisson_model))

final_model <- glm(FATALS ~ RouteCategory * LightCond + AreaClass + 
                   poly(TRAV_SP, 2) + poly(AGE, 2) + HOUR_sin + HOUR_cos, 
                   family = quasipoisson(link = "log"), data = df_filtered)
summary(final_model)

exp_coef <- exp(coef(final_model))
exp_confint <- exp(confint(final_model))
data.frame(Coefficient = names(exp_coef), Estimate = exp_coef, 
           `2.5%` = exp_confint[, 1], `97.5%` = exp_confint[, 2])

library(car)
influencePlot(final_model, id.method = "identify")

ggplot(df_filtered, aes(x = predict(final_model), y = FATALS)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual Fatalities", x = "Predicted Fatalities", y = "Actual Fatalities")

ggplot(df_filtered, aes(x = predict(final_model), y = residuals(final_model, type = "pearson"))) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Predicted", x = "Predicted Values", y = "Residuals")

# Aggregate data for RouteCategory and LightCond
interaction_data <- df_filtered %>%
  group_by(RouteCategory, LightCond) %>%
  summarise(TotalFatalities = sum(FATALS, na.rm = TRUE))

# Create the interaction plot
interaction_plot <- ggplot(interaction_data, aes(x = RouteCategory, y = TotalFatalities, fill = LightCond)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Interaction Effect: Route Category and Light Conditions", 
       x = "Route Category", y = "Total Fatalities") +
  theme_minimal()

# Print the plot
print(interaction_plot)

## MODEL RELIABILITY ##
# Calculate Pseudo R-squared
null_deviance <- final_model$null.deviance
residual_deviance <- final_model$deviance
pseudo_r2 <- 1 - (residual_deviance / null_deviance)
cat("Pseudo R^2: ", pseudo_r2, "\n")

# Predictions
df_filtered$predicted <- predict(final_model, type = "response")

# Calculate MSE and MAE
mse <- mean((df_filtered$FATALS - df_filtered$predicted)^2)
mae <- mean(abs(df_filtered$FATALS - df_filtered$predicted))
cat("Mean Squared Error (MSE): ", mse, "\n")
cat("Mean Absolute Error (MAE): ", mae, "\n")

# Split data into training and testing sets (70/30 split)
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(df_filtered), size = 0.7 * nrow(df_filtered))
train_data <- df_filtered[train_indices, ]
test_data <- df_filtered[-train_indices, ]

# Fit model on training data
train_model <- glm(FATALS ~ RouteCategory * LightCond + AreaClass + 
                   poly(TRAV_SP, 2) + poly(AGE, 2) + HOUR_sin + HOUR_cos, 
                   family = quasipoisson(link = "log"), data = train_data)

# Predict on test data
test_data$predicted <- predict(train_model, newdata = test_data, type = "response")

# Calculate test MSE and MAE
test_mse <- mean((test_data$FATALS - test_data$predicted)^2)
test_mae <- mean(abs(test_data$FATALS - test_data$predicted))
cat("Test Mean Squared Error (MSE): ", test_mse, "\n")
cat("Test Mean Absolute Error (MAE): ", test_mae, "\n")

# Compare train vs. test errors
cat("Train/Test Error Ratio (MSE): ", mse / test_mse, "\n")

# 10-fold Cross-Validation
cv_results <- cv.glm(data = df_filtered, glmfit = final_model, K = 10)

# Cross-validated MSE
cat("Cross-validated MSE: ", cv_results$delta[1], "\n")