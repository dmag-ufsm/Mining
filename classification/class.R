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

# The data has a certain order (placement), so we'll need to "shuffle" the dataframes
# so it'll be representative of the actual population.
# (thankfully the result didn't change for the images as of 2019-11-05; 
# however, it's good practice)
set.seed(123)

g <- runif(nrow(df3))
df3 <- df3[order(g),]

g <- runif(nrow(df4))
df4 <- df4[order(g),]

g <- runif(nrow(df5))
df5 <- df5[order(g),]

g <- runif(nrow(df6))
df6 <- df6[order(g),]

g <- runif(nrow(df7))
df7 <- df7[order(g),]

fitPlot <- function(df, y, x, label) {
  # method="class" for a classification tree
  # method="anova" for a regression tree
  fit <- rpart(y ~ x, method="class", data=df)
  rpart.plot(fit, main=label, branch.lty=3, shadow.col="gray")
}

# PLACE ~ SHIELDS 
#fitPlot(df3, df3$Place, df3$Shields, "Classification by x = Shield count: 3 players")
#fitPlot(df4, df4$Place, df4$Shields, "Classification by x = Shield count: 4 players")
#fitPlot(df5, df5$Place, df5$Shields, "Classification by x = Shield count: 5 players")
#fitPlot(df6, df6$Place, df6$Shields, "Classification by x = Shield count: 6 players")
#fitPlot(df7, df7$Place, df7$Shields, "Classification by x = Shield count: 7 players")
# Conclusion:
# Players with lots of shields (but not excessively so (> 9)) tend to score better,
# proving that an agressive playstyle may be worth the investment.


# PLACE ~ TREASURY VP (Nº COINS)
#fitPlot(df3, df3$Place, df3$VP.from.Treasury.Contents, "Classification by x = VP Treasury: 3 players")
#fitPlot(df4, df4$Place, df4$VP.from.Treasury.Contents, "Classification by x = VP Treasury: 4 players")
#fitPlot(df5, df5$Place, df5$VP.from.Treasury.Contents, "Classification by x = VP Treasury: 5 players")
#fitPlot(df6, df6$Place, df6$VP.from.Treasury.Contents, "Classification by x = VP Treasury: 6 players")
#fitPlot(df7, df7$Place, df7$VP.from.Treasury.Contents, "Classification by x = VP Treasury: 7 players")
# Conclusion:
# Players that don't spend many coins tend to score better (< 7 players)


# PLACE ~ WONDER VP
#fitPlot(df3, df3$Place, df3$VP.from.Wonder, "Classification by x = VP Wonder: 3 players")
#fitPlot(df4, df4$Place, df4$VP.from.Wonder, "Classification by x = VP Wonder: 4 players")
#fitPlot(df5, df5$Place, df5$VP.from.Wonder, "Classification by x = VP Wonder: 5 players")
#fitPlot(df6, df6$Place, df6$VP.from.Wonder, "Classification by x = VP Wonder: 6 players")
#fitPlot(df7, df7$Place, df7$VP.from.Wonder, "Classification by x = VP Wonder: 7 players")
# Conclusion:
# Interestingly, top players don't tend to have that much VPs from Wonders -- this is due to the fact
# that they tend to play with side B, which provides less VPs in general.

# PLACE ~ RAW MATERIALS 
#fitPlot(df3, df3$Place, df3$Raw.Materials, "Classification by x = Raw Materials: 3 players")
#fitPlot(df4, df4$Place, df4$Raw.Materials, "Classification by x = Raw Materials: 4 players")
#fitPlot(df5, df5$Place, df5$Raw.Materials, "Classification by x = Raw Materials: 5 players")
#fitPlot(df6, df6$Place, df6$Raw.Materials, "Classification by x = Raw Materials: 6 players")
#fitPlot(df7, df7$Place, df7$Raw.Materials, "Classification by x = Raw Materials: 7 players")

# PLACE ~ MANUFACTURED GOODS 
#fitPlot(df3, df3$Place, df3$Manufactured.Goods, "Classification by x = Manufactured Goods: 3 players")
#fitPlot(df4, df4$Place, df4$Manufactured.Goods, "Classification by x = Manufactured Goods: 4 players")
#fitPlot(df5, df5$Place, df5$Manufactured.Goods, "Classification by x = Manufactured Goods: 5 players")
#fitPlot(df6, df6$Place, df6$Manufactured.Goods, "Classification by x = Manufactured Goods: 6 players")
#fitPlot(df7, df7$Place, df7$Manufactured.Goods, "Classification by x = Manufactured Goods: 7 players")

# PLACE ~ Wonder ID
# Didn't work. Probably need to change some rpart settings?

fitPlotVP <- function(df, y, wonder, conflict.victory, civil, comm, guild, sci, treasury, label) {
  fit <- rpart(y ~ wonder+conflict.victory+civil+comm+guild+sci+treasury, method="class", data=df)
  rpart.plot(fit, main=label, branch.lty=3, shadow.col="gray")
}


# PLACE ~ COMBINED VPs
fitPlotVP(df3, 
          df3$Place, 
          df3$VP.from.Wonder,
          df3$VP.from.Military.Conflicts..Victory,
          df3$VP.from.Civilian.Structures,
          df3$VP.from.Commercial.Structures,
          df3$VP.from.Guilds,
          df3$VP.from.Scientific.Structures,
          df3$VP.from.Treasury.Contents,
          "Classification based on y = Place, x = combined VPs (3 PLAYERS)")

fitPlotVP(df4, 
          df4$Place, 
          df4$VP.from.Wonder,
          df4$VP.from.Military.Conflicts..Victory,
          df4$VP.from.Civilian.Structures,
          df4$VP.from.Commercial.Structures,
          df4$VP.from.Guilds,
          df4$VP.from.Scientific.Structures,
          df4$VP.from.Treasury.Contents,
          "Classification based on y = Place, x = combined VPs (4 PLAYERS)")

fitPlotVP(df5, 
          df5$Place, 
          df5$VP.from.Wonder,
          df5$VP.from.Military.Conflicts..Victory,
          df5$VP.from.Civilian.Structures,
          df5$VP.from.Commercial.Structures,
          df5$VP.from.Guilds,
          df5$VP.from.Scientific.Structures,
          df5$VP.from.Treasury.Contents,
          "Classification based on y = Place, x = combined VPs (5 PLAYERS)")

fitPlotVP(df6, 
          df6$Place, 
          df6$VP.from.Wonder,
          df6$VP.from.Military.Conflicts..Victory,
          df6$VP.from.Civilian.Structures,
          df6$VP.from.Commercial.Structures,
          df6$VP.from.Guilds,
          df6$VP.from.Scientific.Structures,
          df6$VP.from.Treasury.Contents,
          "Classification based on y = Place, x = combined VPs (6 PLAYERS)")

fitPlotVP(df7, 
          df7$Place, 
          df7$VP.from.Wonder,
          df7$VP.from.Military.Conflicts..Victory,
          df7$VP.from.Civilian.Structures,
          df7$VP.from.Commercial.Structures,
          df7$VP.from.Guilds,
          df7$VP.from.Scientific.Structures,
          df7$VP.from.Treasury.Contents,
          "Classification based on y = Place, x = combined VPs (7 PLAYERS)")



# Continue with other classifications and see which results are interesting.
# (in case you're wondering, I did merge all the data into a single dataframe earlier,
#  but the results were pretty garbage in general, as expected)

# Classification based on place and quantity of military structures
#fitPlot(df3, df3$Place, df3$Military.Structures, "Classification | y = Place, x = Military Structures(3 PLAYERS)")
#fitPlot(df4, df4$Place, df4$Military.Structures, "Classification | y = Place, x = Military Structures(4 PLAYERS)")
#fitPlot(df5, df5$Place, df5$Military.Structures, "Classification | y = Place, x = Military Structures(5 PLAYERS)")
#fitPlot(df6, df6$Place, df6$Military.Structures, "Classification | y = Place, x = Military Structures(6 PLAYERS)")
#fitPlot(df7, df7$Place, df7$Military.Structures, "Classification | y = Place, x = Military Structures(7 PLAYERS)")

# Classification based on place and quantity of scientific structures
#fitPlot(df3, df3$Place, df3$Scientific.Structures, "Classification | y = Place, x = Scientific Structures(3 PLAYERS)")
#fitPlot(df4, df4$Place, df4$Scientific.Structures, "Classification | y = Place, x = Scientific Structures(4 PLAYERS)")
#fitPlot(df5, df5$Place, df5$Scientific.Structures, "Classification | y = Place, x = Scientific Structures(5 PLAYERS)")
#fitPlot(df6, df6$Place, df6$Scientific.Structures, "Classification | y = Place, x = Scientific Structures(6 PLAYERS)")
#fitPlot(df7, df7$Place, df7$Scientific.Structures, "Classification | y = Place, x = Scientific Structures(7 PLAYERS)")

# Classification based on place and coins spent on commerce 
#fitPlot(df3, df3$Place, df3$Coins.spent.on.commerce, "Classification | y = Place, x = Coins spent on commerce(3 PLAYERS)")
#fitPlot(df4, df4$Place, df4$Coins.spent.on.commerce, "Classification | y = Place, x = Coins spent on commerce(4 PLAYERS)")
#fitPlot(df5, df5$Place, df5$Coins.spent.on.commerce, "Classification | y = Place, x = Coins spent on commerce(5 PLAYERS)")
#fitPlot(df6, df6$Place, df6$Coins.spent.on.commerce, "Classification | y = Place, x = Coins spent on commerce(6 PLAYERS)")
#fitPlot(df7, df7$Place, df7$Coins.spent.on.commerce, "Classification | y = Place, x = Coins spent on commerce(7 PLAYERS)")

# Classification based on place and commercial structures
#fitPlot(df3, df3$Place, df3$Commercial.Structures, "Classification | y = Place, x = Commercial Structures(3 PLAYERS)")
#fitPlot(df4, df4$Place, df4$Commercial.Structures, "Classification | y = Place, x = Commercial Structures(4 PLAYERS)")
#fitPlot(df5, df5$Place, df5$Commercial.Structures, "Classification | y = Place, x = Commercial Structures(5 PLAYERS)")
#fitPlot(df6, df6$Place, df6$Commercial.Structures, "Classification | y = Place, x = Commercial Structures(6 PLAYERS)")
#fitPlot(df7, df7$Place, df7$Commercial.Structures, "Classification | y = Place, x = Commercial Structures(7 PLAYERS)")

