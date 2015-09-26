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

# grp <- group_by(diamonds, clarity)
# diamondsByClarity$mean_price <- group_by(diamonds, clarity)

diamondsByClarity <- diamonds %>% 
    group_by(clarity) %>%
        summarise(mean_price = mean(price),
              median_price = median(price),
              min_price = min(price),
              max_price = max(price),
              n= n()
              )
              
# Bar Charts of Mean Price
library(gridExtra)
library(ggplot2)
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

# plot1 <- barplot(diamonds_mp_by_color$mean_price, names.arg= diamonds_mp_by_color$color )
# plot2 <- barplot(diamonds_mp_by_clarity$mean_price, names.arg = diamonds_mp_by_clarity$clarity)             

plot1 <- ggplot(diamonds_mp_by_color, aes(x=color, y = mean_price )) + geom_bar(stat="identity")
plot2 <- ggplot(diamonds_mp_by_clarity, aes(x=clarity, y = mean_price )) + geom_bar(stat="identity")

grid.arrange(plot1, plot2)
