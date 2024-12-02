# Kzzy Centeno, Tatiana Madeam, Krystal Moore-Gayle
# Project B
# Decemeber 2, 2024

# LIBRARIES
library(ggplot2) # ggplot2 and dplyr for better visualization of large data sets
library(dplyr)
library(GGally)
library(car)
library(MASS)
library(boot)
library(effects)

# DATASET
df <- read.csv("combined_midwest_data_with_dup.csv")
attach(df)
dim(df)

# Response: (FATALS)
# Categorical: (RUR-URBAN), (LGT_CONDNAME), (ROUTENAME)
# Continuous:(TRAV_SP) (AGE) (HOUR) 


## PLOTS BEFORE FILTERING
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

## DESCRIPTIVE STATISTICS FOR MODEL VARIABLES ##
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

# Summary statistics for continuous variables
summary(df_filtered[, c("FATALS", "TRAV_SP", "AGE", "HOUR.x")])

# Frequency tables for categorical variables
table(df_filtered$RouteCategory)
table(df_filtered$LightCond)
table(df_filtered$AreaClass)

## CORRELATION MATRIX ##
# Correlation Matrix for Continuous
subset_df <- df_filtered[c("FATALS", "TRAV_SP", "AGE", "HOUR.x")]
cor_matrix_sub <- cor(subset_df)
print(cor_matrix_sub)

## HEAT MAPS
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

## INITIAL MODEL ##

## RESIDUAL PLOTS ##
# Convert categorical variables to dummies
df_filtered$RouteCategory <- factor(df_filtered$RouteCategory)
df_filtered$LightCond <- factor(df_filtered$LightCond)
df_filtered$AreaClass <- factor(df_filtered$AreaClass)

model <- lm(FATALS ~ RouteCategory + LightCond + AreaClass + TRAV_SP + AGE + HOUR.x, data = df_filtered)

summary(model)
AIC(model)
crPlots(model)

# transforming TRAV_SP and AGE to address linearity
model2 <- lm(FATALS ~ RouteCategory + LightCond + AreaClass + poly(TRAV_SP, 2) + poly(AGE, 2) + HOUR_sin + HOUR_cos,  data = df_filtered)
summary(model2)
AIC(model2)
crPlots(model2)

# using stepwise selection
stepwise_model <- stepAIC(model2, direction = "both")
summary(stepwise_model)

plot(model2)

# using interaction terms to improve fit
model3 <- lm(FATALS ~ RouteCategory * LightCond + AreaClass + 
               poly(TRAV_SP, 2) + poly(AGE, 2) + HOUR_sin + HOUR_cos, 
             data = df_filtered)
summary(model3)
AIC(model3)
plot(model3)

# using stepwise selection for model with interaction term
stepwise_model2 <- stepAIC(model3, direction = "both")
summary(stepwise_model2)

## POISSON MODEL ##
# Poisson Regression Model
poisson_model <- glm(FATALS ~ RouteCategory * LightCond + AreaClass + 
                       poly(TRAV_SP, 2) + poly(AGE, 2) + HOUR_sin + HOUR_cos,  
                     family = poisson(link = "log"), data = df_filtered)
summary(poisson_model)

# calculate pseudo r^2
null_deviance <- poisson_model$null.deviance
residual_deviance <- poisson_model$deviance
pseudo_r2 <- 1 - (residual_deviance / null_deviance)
cat("Pseudo R^2: ", pseudo_r2, "\n")

plot(poisson_model)

## QUASI-POISSON MODEL ##
# Quasi-Poisson Regression Model
quasi_poisson_model <- glm(FATALS ~ RouteCategory * LightCond + AreaClass + 
                             poly(TRAV_SP, 2) + poly(AGE, 2) + HOUR_sin + HOUR_cos,  
                           family = quasipoisson(link = "log"), data = df_filtered)
summary(quasi_poisson_model)

# calculate pseudo r^2
quasi_null_deviance <- quasi_poisson_model$null.deviance
quasi_residual_deviance <- quasi_poisson_model$deviance
quasi_pseudo_r2 <- 1 - (quasi_residual_deviance / quasi_null_deviance)
cat("Pseudo R^2: ", quasi_pseudo_r2, "\n")

plot(quasi_poisson_model)

## NEGATIVE BINOMIAL MODEL ##
nb_model <- glm.nb(FATALS ~ RouteCategory * LightCond + AreaClass + 
                     poly(TRAV_SP, 2) + poly(AGE, 2) + HOUR_sin + HOUR_cos, 
                   data = df_filtered)

summary(nb_model)
AIC(nb_model)
plot(nb_model)

## MODEL DIAGNOSTICS ##

# VIF
vif_values <- vif(model3)
print(vif_values)

# studres, leverage, and cooke's
influencePlot(model3)

## MODEL EVALUATION AND VALIDATION ##

# comparing AIC of all models
AIC(model, model2, model3, poisson_model, nb_model)

# Predictions
df_filtered$predicted <- predict(model3, type = "response")

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
train_model <- lm(FATALS ~ RouteCategory * LightCond + AreaClass + 
                    poly(TRAV_SP, 2) + poly(AGE, 2) + HOUR_sin + HOUR_cos, 
                  data = df_filtered)

# Predict on test data
test_data$predicted <- predict(train_model, newdata = test_data, type = "response")

# Calculate test MSE and MAE
test_mse <- mean((test_data$FATALS - test_data$predicted)^2)
test_mae <- mean(abs(test_data$FATALS - test_data$predicted))
cat("Test Mean Squared Error (MSE): ", test_mse, "\n")
cat("Test Mean Absolute Error (MAE): ", test_mae, "\n")

# Compare train vs. test errors
cat("Train/Test Error Ratio (MSE): ", mse / test_mse, "\n")

## FINAL MODEL ##

final_model <- model3
summary(final_model)

plot(allEffects(final_model), multiline = TRUE)
plot(final_model)





