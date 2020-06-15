library(rpart)
library(rpart.plot)

# Session -> Set Working Directiory -> To Source File Location

#df3 <- na.omit(read.csv("../datasets/oct_19/7wonders_3.csv"))
df3 <- na.omit(read.csv("../datasets/mar_20/7wonders_3.csv"))
df4 <- na.omit(read.csv("../datasets/mar_20/7wonders_4.csv"))
df5 <- na.omit(read.csv("../datasets/mar_20/7wonders_5.csv"))
df6 <- na.omit(read.csv("../datasets/mar_20/7wonders_6.csv"))
df7 <- na.omit(read.csv("../datasets/mar_20/7wonders_7.csv"))

# The data has a certain order (placement), so we'll need to "shuffle" the dataframes
# so it'll be representative of the actual population.
set.seed(123)

createTrainTest <- function(data, size = 0.8, train = TRUE) {
  train_sample <- 1:(size * nrow(data))
  if (train == TRUE) 
    return (data[train_sample, ])
  else
    return (data[-train_sample, ])
}

g <- runif(nrow(df3))
df3 <- df3[order(g),]
df3 <- createTrainTest(df3, 0.8, train = TRUE)
df3_test <- createTrainTest(df3, 0.8, train = FALSE)

g <- runif(nrow(df4))
df4 <- df4[order(g),]
df4 <- createTrainTest(df4, 0.8, train = TRUE)
df4_test <- createTrainTest(df4, 0.8, train = FALSE)

g <- runif(nrow(df5))
df5 <- df5[order(g),]
df5 <- createTrainTest(df5, 0.8, train = TRUE)
df5_test <- createTrainTest(df5, 0.8, train = FALSE)

g <- runif(nrow(df6))
df6 <- df6[order(g),]
df6 <- createTrainTest(df6, 0.8, train = TRUE)
df6_test <- createTrainTest(df6, 0.8, train = FALSE)

g <- runif(nrow(df7))
df7 <- df7[order(g),]
df7 <- createTrainTest(df7, 0.8, train = TRUE)
df7_test <- createTrainTest(df7, 0.8, train = FALSE)

fitPlot <- function(df, df_test, y, x, label) {
  # method="class" for a classification tree
  # method="anova" for a regression tree
  fit <- rpart(y ~ x, method="class", data=df)
  rpart.plot(fit, main=label, branch.lty=3, shadow.col="gray")
  df_predict <- predict(fit, df_test, type="class")
  table(df$Place, df_predict)
}

# PLACE ~ SHIELDS 
fitPlot(df3, df3_test, df3$Place, df3$Shields, "Classification by x = Shield count: 3 players")
fitPlot(df4, df4_test, df4$Place, df4$Shields, "Classification by x = Shield count: 4 players")
fitPlot(df5, df5_test, df5$Place, df5$Shields, "Classification by x = Shield count: 5 players")
fitPlot(df6, df6_test, df6$Place, df6$Shields, "Classification by x = Shield count: 6 players")
fitPlot(df7, df7_test, df7$Place, df7$Shields, "Classification by x = Shield count: 7 players")
# Conclusion:
# Players with lots of shields (but not excessively so (> 9)) tend to score better,
# proving that an agressive playstyle may be worth the investment.


# PLACE ~ TREASURY VP (Nº COINS)
#fitPlot(df3, df3_test, df3$Place, df3$VP.from.Treasury.Contents, "Classification by x = VP Treasury: 3 players")
#fitPlot(df4, df4_test, df4$Place, df4$VP.from.Treasury.Contents, "Classification by x = VP Treasury: 4 players")
#fitPlot(df5, df5_test, df5$Place, df5$VP.from.Treasury.Contents, "Classification by x = VP Treasury: 5 players")
#fitPlot(df6, df6_test, df6$Place, df6$VP.from.Treasury.Contents, "Classification by x = VP Treasury: 6 players")
#fitPlot(df7, df7_test, df7$Place, df7$VP.from.Treasury.Contents, "Classification by x = VP Treasury: 7 players")
# Conclusion:
# Players that don't spend many coins tend to score better (< 7 players)

# PLACE ~ DISCARDED CARDS
#fitPlot(df3, df3_test, df3$Place, df3$Cards.discarded, "Classification by x = Cards discarded: 3 players")
#fitPlot(df4, df4_test, df4$Place, df4$Cards.discarded, "Classification by x = Cards discarded: 4 players")
#fitPlot(df5, df5_test, df5$Place, df5$Cards.discarded, "Classification by x = Cards discarded: 5 players")
#fitPlot(df6, df6_test, df6$Place, df6$Cards.discarded, "Classification by x = Cards discarded: 6 players")
#fitPlot(df7, df7_test, df7$Place, df7$Cards.discarded, "Classification by x = Cards discarded: 7 players")

# PLACE ~ WONDER VP
#fitPlot(df3, df3_test, df3$Place, df3$VP.from.Wonder, "Classification by x = VP Wonder: 3 players")
#fitPlot(df4, df4_test, df4$Place, df4$VP.from.Wonder, "Classification by x = VP Wonder: 4 players")
#fitPlot(df5, df5_test, df5$Place, df5$VP.from.Wonder, "Classification by x = VP Wonder: 5 players")
#fitPlot(df6, df6_test, df6$Place, df6$VP.from.Wonder, "Classification by x = VP Wonder: 6 players")
#fitPlot(df7, df7_test, df7$Place, df7$VP.from.Wonder, "Classification by x = VP Wonder: 7 players")
# Conclusion:
# Interestingly, top players don't tend to have that much VPs from Wonders -- this is due to the fact
# that they tend to play with side B, which provides less VPs in general.

# PLACE ~ RAW MATERIALS 
#fitPlot(df3, df3_test, df3$Place, df3$Raw.Materials, "Classification by x = Raw Materials: 3 players")
#fitPlot(df4, df4_test, df4$Place, df4$Raw.Materials, "Classification by x = Raw Materials: 4 players")
#fitPlot(df5, df5_test, df5$Place, df5$Raw.Materials, "Classification by x = Raw Materials: 5 players")
#fitPlot(df6, df6_test, df6$Place, df6$Raw.Materials, "Classification by x = Raw Materials: 6 players")
#fitPlot(df7, df7_test, df7$Place, df7$Raw.Materials, "Classification by x = Raw Materials: 7 players")

# PLACE ~ MANUFACTURED GOODS 
#fitPlot(df3, df3_test, df3$Place, df3$Manufactured.Goods, "Classification by x = Manufactured Goods: 3 players")
#fitPlot(df4, df4_test, df4$Place, df4$Manufactured.Goods, "Classification by x = Manufactured Goods: 4 players")
#fitPlot(df5, df5_test, df5$Place, df5$Manufactured.Goods, "Classification by x = Manufactured Goods: 5 players")
#fitPlot(df6, df6_test, df6$Place, df6$Manufactured.Goods, "Classification by x = Manufactured Goods: 6 players")
#fitPlot(df7, df7_test, df7$Place, df7$Manufactured.Goods, "Classification by x = Manufactured Goods: 7 players")


fitPlotVP <- function(df, df_test, label) {
  names(df)[names(df) == "VP.from.Wonder"] <- "wonder"
  names(df)[names(df) == "VP.from.Military.Conflicts..Victory."] <- "military.victory"
  names(df)[names(df) == "VP.from.Civilian.Structures"] <- "civil"
  names(df)[names(df) == "VP.from.Commercial.Structures"] <- "commerce"
  names(df)[names(df) == "VP.from.Guilds"] <- "guild"
  names(df)[names(df) == "VP.from.Scientific.Structures"] <- "sci"
  names(df)[names(df) == "VP.from.Treasury.Contents"] <- "treasury"
  names(df_test)[names(df_test) == "VP.from.Wonder"] <- "wonder"
  names(df_test)[names(df_test) == "VP.from.Military.Conflicts..Victory."] <- "military.victory"
  names(df_test)[names(df_test) == "VP.from.Civilian.Structures"] <- "civil"
  names(df_test)[names(df_test) == "VP.from.Commercial.Structures"] <- "commerce"
  names(df_test)[names(df_test) == "VP.from.Guilds"] <- "guild"
  names(df_test)[names(df_test) == "VP.from.Scientific.Structures"] <- "sci"
  names(df_test)[names(df_test) == "VP.from.Treasury.Contents"] <- "treasury"
  
  fit <- rpart(Place ~ wonder+military.victory+civil+commerce+guild+sci+treasury, method="class", data=df, cp=0.005)
  rpart.plot(fit, main=label, branch.lty=3)
  df_predict <- predict(fit, newdata = df_test, type="class")
  table(df_test$Place, df_predict)
}

# PLACE ~ COMBINED VPs
fitPlotVP(df3,
          df3_test,
          "Classification based on y = Place, x = combined VPs (3 PLAYERS)")

fitPlotVP(df4,
          df4_test,
          "Classification based on y = Place, x = combined VPs (4 PLAYERS)")

fitPlotVP(df5,
          df5_test,
          "Classification based on y = Place, x = combined VPs (5 PLAYERS)")

fitPlotVP(df6,
          df6_test,
          "Classification based on y = Place, x = combined VPs (6 PLAYERS)")

fitPlotVP(df7,
          df7_test,
          "Classification based on y = Place, x = combined VPs (7 PLAYERS)")


# Continue with other classifications and see which results are interesting.
# (in case you're wondering, I did merge all the data into a single dataframe earlier,
#  but the results were pretty garbage in general, as expected)

# Classification based on place and quantity of military structures
#fitPlot(df3, df3_test, df3$Place, df3$Military.Structures, "Classification | y = Place, x = Military Structures(3 PLAYERS)")
#fitPlot(df4, df4_test, df4$Place, df4$Military.Structures, "Classification | y = Place, x = Military Structures(4 PLAYERS)")
#fitPlot(df5, df5_test, df5$Place, df5$Military.Structures, "Classification | y = Place, x = Military Structures(5 PLAYERS)")
#fitPlot(df6, df6_test, df6$Place, df6$Military.Structures, "Classification | y = Place, x = Military Structures(6 PLAYERS)")
#fitPlot(df7, df7_test, df7$Place, df7$Military.Structures, "Classification | y = Place, x = Military Structures(7 PLAYERS)")

# Classification based on place and quantity of scientific structures
#fitPlot(df3, df3_test, df3$Place, df3$Scientific.Structures, "Classification | y = Place, x = Scientific Structures(3 PLAYERS)")
#fitPlot(df4, df4_test, df4$Place, df4$Scientific.Structures, "Classification | y = Place, x = Scientific Structures(4 PLAYERS)")
#fitPlot(df5, df5_test, df5$Place, df5$Scientific.Structures, "Classification | y = Place, x = Scientific Structures(5 PLAYERS)")
#fitPlot(df6, df6_test, df6$Place, df6$Scientific.Structures, "Classification | y = Place, x = Scientific Structures(6 PLAYERS)")
#fitPlot(df7, df7_test, df7$Place, df7$Scientific.Structures, "Classification | y = Place, x = Scientific Structures(7 PLAYERS)")

# Classification based on place and coins spent on commerce 
#fitPlot(df3, df3_test, df3$Place, df3$Coins.spent.on.commerce, "Classification | y = Place, x = Coins spent on commerce(3 PLAYERS)")
#fitPlot(df4, df4_test, df4$Place, df4$Coins.spent.on.commerce, "Classification | y = Place, x = Coins spent on commerce(4 PLAYERS)")
#fitPlot(df5, df5_test, df5$Place, df5$Coins.spent.on.commerce, "Classification | y = Place, x = Coins spent on commerce(5 PLAYERS)")
#fitPlot(df6, df6_test, df6$Place, df6$Coins.spent.on.commerce, "Classification | y = Place, x = Coins spent on commerce(6 PLAYERS)")
#fitPlot(df7, df7_test, df7$Place, df7$Coins.spent.on.commerce, "Classification | y = Place, x = Coins spent on commerce(7 PLAYERS)")

# Classification based on place and commercial structures
#fitPlot(df3, df3_test, df3$Place, df3$Commercial.Structures, "Classification | y = Place, x = Commercial Structures(3 PLAYERS)")
#fitPlot(df4, df4_test, df4$Place, df4$Commercial.Structures, "Classification | y = Place, x = Commercial Structures(4 PLAYERS)")
#fitPlot(df5, df5_test, df5$Place, df5$Commercial.Structures, "Classification | y = Place, x = Commercial Structures(5 PLAYERS)")
#fitPlot(df6, df6_test, df6$Place, df6$Commercial.Structures, "Classification | y = Place, x = Commercial Structures(6 PLAYERS)")
#fitPlot(df7, df7_test, df7$Place, df7$Commercial.Structures, "Classification | y = Place, x = Commercial Structures(7 PLAYERS)")

