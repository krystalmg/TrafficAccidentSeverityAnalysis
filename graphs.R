# Import ggplot2 for better visualization of large data sets
library(ggplot2)

# Import Mosaic Plot from VCD library
library(vcd)

# Import and attach data set
df <- data.frame(combined_midwest_data_unique)
attach(df)

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

layout(matrix(1), widths = c(1.5))
mosaic(~ MONTHNAME + WEATHERNAME, data = df)