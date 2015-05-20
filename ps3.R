# Problem set 3
library(ggplot2)

data(diamonds)
summary(diamonds)
dim(diamonds)
myplot <- qplot(data=diamonds, x=price, xlim=c(0, 1000))

myplot + facet_grid(cut ~ .)
by(data=diamonds$price, diamonds$cut, summary)
by(data=diamonds$price, diamonds$cut, median)
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales="free_y")
myplot <- qplot(x=price / carat, data=diamonds ) +facet_wrap(~cut)+scale_x_log10()
myplot <- qplot(y=price, data=diamonds, x=cut)


myplot + geom_boxplot()
by(data=diamonds$price, diamonds$color, summary)
IQR(subset(diamonds, diamonds$color=="J")$price)
by(data=diamonds$price/diamonds$carat, diamonds$color, summary)

myplot <- qplot(data=diamonds, y=(price/carat), x=color, xlim=c(0, 1000))
myplot + geom_boxplot()
myplot
ggplot(aes(x=color, y = price), data = diamonds) +geom_boxplot()+coord_cartesian(ylim = c(0, 10000))
