# Import ggplot2 and dplyr for better visualization of large data sets
library(ggplot2)
library(dplyr)
library(GGally)

# Import and attach data set
 df <- data.frame(combined_midwest_data_unique)
#df <- read.csv("combined_midwest_data_unique.csv")
attach(df)

dim(df)
print(df)
plot(df)

plot(df$STATE.x)
# Separate correlation matrices
# Heat map
ggplot(df, aes(x = STATENAME.x, y = TRAV_SP)) + geom_point()

# Plot as bar graph, State and Injury Severity Count
ggplot(df, aes(x = STATENAME.x)) +
  geom_bar() +
  labs(title = "Bar Graph of State Count", x = "State Names", y = "Count") +
  theme_minimal()

ggplot(df, aes(x = INJ_SEVNAME)) +
  geom_bar() +
  labs(title = "Bar Graph of Injury Severity Count", x = "Injury Severity Name", y = "Count") +
  theme_minimal()

ggplot(df, aes(x = WEATHERNAME)) +
  geom_bar() +
  labs(title = "Bar Graph of Weather Condition Count", x = "Weather Condition", y = "Count") +
  theme_minimal()

# Heat Map Visualization
# Weather x Injury Severity
weather_sev_counts <- df %>%
  count(WEATHERNAME, INJ_SEVNAME)

ggplot(weather_sev_counts, aes(x = WEATHERNAME, y = INJ_SEVNAME, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Count")

ggpairs(df, cardinality_threshold = 500)