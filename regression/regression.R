
data <- read.csv('7wonders_3.csv')
data <- read.csv('7wonders_4.csv')
data <- read.csv('7wonders_5.csv')
data <- read.csv('7wonders_6.csv')
data <- read.csv('7Wonders_7.csv')

#REGRESSÃO LINEAR SIMPLES
# coins spent on commerce x vp total
cor(data$Coins.spent.on.commerce, data$VP.total, use = 'complete.obs')
ggplot(data, aes(x = Coins.spent.on.commerce, y = VP.total)) + geom_point()
model <- lm(data = data, formula = VP.total ~ Commercial.Structures)
summary(model)
ggplot(data, aes(x = Coins.spent.on.commerce, y = VP.total)) + geom_point() + geom_smooth(method = lm, see = FALSE)



# Scientific Structures x vp total
cor(data$Scientific.Structures, data$VP.total, use = "complete.obs")
ggplot(data, aes(x = Scientific.Structures, y = VP.total)) + geom_point()
model <- lm(data = data, formula = VP.total ~  Scientific.Structures)
summary(model)
ggplot(data, aes( x = Scientific.Structures, y = VP.total)) + geom_point() + geom_smooth(method = lm, see = FALSE)

# cards discarded ax vp total
cor(data$Cards.discarded, data$VP.total, use = "complete.obs")
ggplot(data, aes(x = Cards.discarded, y = VP.total)) + geom_point()
model <- lm(data = data, formula = VP.total ~ Cards.discarded)
summary(model)
ggplot(data, aes( x = Cards.discarded, y = VP.total)) + geom_point() + geom_smooth(method = lm, see = FALSE)

# civilian structures x vp total
cor(data$Civilian.Structures, data$VP.total, use = "complete.obs")
ggplot(data, aes(x = Civilian.Structures, y = VP.total)) + geom_point()
model <- lm(data = data, formula = VP.total ~ Civilian.Structures)
summary(model)
ggplot(data, aes(x = Civilian.Structures, y = VP.total)) + geom_point() + geom_smooth(method = lm, see = FALSE)

# commercial structures x vp total
cor(data$Commercial.Structures, data$VP.total, use = "complete.obs")
ggplot(data, aes(x = Commercial.Structures, y = VP.total)) + geom_point()
model <- lm(data = data, formula = VP.total ~ Commercial.Structures)
summary(model)
ggplot(data, aes(x =  Commercial.Structures, y = VP.total)) + geom_point() + geom_smooth(method = lm, see = FALSE)


