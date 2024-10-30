# Import and attach data set
df <- data.frame(combined_midwest_data_unique)
attach(df)

print(df)
par(mar=c(1,1,1,1))
plot(df)

