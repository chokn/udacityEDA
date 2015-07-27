library(ggplot2)
data("diamonds")
names(diamonds)


# price versus x
ggplot(data = diamonds, (aes(x=x, y = price))) + geom_point()

# correlations
cor.test(diamonds$x, diamonds$price)
cor.test(diamonds$y, diamonds$price)
cor.test(diamonds$z, diamonds$price)

ggplot(data = diamonds, (aes(x=depth, y = price))) + geom_point()

# adjustments price versus depth
ggplot(data = diamonds, (aes(x=depth, y = price))) + geom_point(alpha= 1/100) + 
  scale_x_continuous(breaks = seq(from = 0, to =80, by = 2) )

cor.test(diamonds$depth, diamonds$price)

quantile(diamonds$price, .99)

ggplot(data = diamonds, (aes(x=carat, y = price))) + geom_point() +
  xlim(0, quantile(diamonds$carat, .99)) + ylim(0, quantile(diamonds$price, .99))

#price versus volume
diamonds$volume <- diamonds[,'x'] * diamonds[,'y'] * diamonds[,'z']

ggplot(data = diamonds, (aes(x=volume, y = price))) + geom_point() 

library(plyr)
count(diamonds$volume == 0)
detach("package:plyr", unload=TRUE)

# cor(subset(diamonds, diamonds$volume < 800 & diamonds$volume > 0)$volume, diamonds$price)
diamonds.sub <- subset(diamonds, volume < 800 & volume > 0)
cor(diamond.sub$price, diamond.sub$volume)

# plot
ggplot(diamonds.sub, aes(x=volume, y = price)) + geom_point(alpha = 1/10) + geom_smooth()

#diamonds by clarity
library(dplyr)
diamondsByClarity <- group_by(diamonds, clarity)
# diamondsByClarity$mean_price <- mean(diamondsByClarity$price)
  