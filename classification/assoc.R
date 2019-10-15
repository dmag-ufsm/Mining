library(rpart)
library(rpart.plot)

# Session -> Set Working Directiory -> To Source File Location

#filenames <- list.files(path = "../datasets", pattern = "*.csv", full.names=TRUE)
#df <- na.omit(do.call(rbind, lapply(filenames, read.csv)))

df3 <- na.omit(read.csv("../datasets/7wonders_3.csv"))
df4 <- na.omit(read.csv("../datasets/7wonders_4.csv"))
df5 <- na.omit(read.csv("../datasets/7wonders_5.csv"))
df6 <- na.omit(read.csv("../datasets/7wonders_6.csv"))
df7 <- na.omit(read.csv("../datasets/7wonders_7.csv"))

fitPlot <- function(df, y, x, label) {
  # method="class" for a classification tree
  # method="anova" for a regression tree
  fit <- rpart(y ~ x, method="class", data=df)
  rpart.plot(fit, extra=4, main=label)
}

fitPlot(df3, df3$Place, df3$Shields, "Classification by x = Shield count: 3 players")
fitPlot(df4, df4$Place, df4$Shields, "Classification by x = Shield count: 4 players")
fitPlot(df5, df5$Place, df5$Shields, "Classification by x = Shield count: 5 players")
fitPlot(df6, df6$Place, df6$Shields, "Classification by x = Shield count: 6 players")
fitPlot(df7, df7$Place, df7$Shields, "Classification by x = Shield count: 7 players")

# Conclusion 1:
# Players with lots of shields (but not excessively so (> 9)) tend to score better,
# proving that an agressive playstyle may be worth the investment.

# Continue with other classifications and see which results are interesting.
# (in case you're wondering, I did merge all the data into a single dataframe earlier,
#  but the results were pretty garbage in general, as expected)