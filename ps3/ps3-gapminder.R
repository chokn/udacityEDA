# tax as % of gdp
# from gapminder
# from http://spreadsheets.google.com/pub?key=0AkBd6lyS3EmpdFgzT1ZJWW4tdDB4Q2NETTVoTG1ZYlE&output=xls
library(readxl)
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(choroplethr)
library(stringi)
dat <- read_excel("Tax revenue (p of GDP).xlsx")
typeof(dat)
tidy.dat <- gather(dat, "year", "taxRevenue", 2:23, convert=TRUE)
names(tidy.dat)[1] <- "country"
tidy.dat %>%
  group_by(country)
max(as.numeric(tidy.dat$year))
qplot(data=filter(tidy.dat, year == 2010, !is.na(taxRevenue)), x=country, y = taxRevenue)
qplot(data=na.omit(filter(tidy.dat, year == 2010) ), x = country, y = taxRevenue, stat="identity") +geom_bar() + coord_flip()

# map
choropleth.dat <- filter(tidy.dat, year==2010) %>%
  select( region = country, value = taxRevenue)
choropleth.dat$region <- stri_trans_tolower(choropleth.dat$region)
choropleth.dat[203, 1] <- "united states of america"
country_choropleth(choropleth.dat)

# looking at US data
taxRev.US  <- filter(tidy.dat, country=="United States")
qplot(data=taxRev.US, x = year, y = taxRevenue, breaks=c(2000:2010), xlim = c(1999,2011)) +geom_line()

# comparing countries
# based on tutorial by Kyle Walker http://walkerke.github.io/
tidy.dat1 <- filter(tidy.dat, !is.na(taxRevenue), year == 2010 ) %>%
  arrange(desc(taxRevenue)) %>%
  filter(row_number() < 20)

taxRev.chart <- ggplot(tidy.dat1, aes(x=reorder(country, taxRevenue), y = taxRevenue)) +
  geom_point(size=12, stat="identity") +
  geom_text(aes(label = round(taxRevenue, digits =1), fontface = "bold"), color = "white", size = 4) +
  coord_flip() +
  theme_minimal(base_size = 20) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Trebhuchet MS")) +
  xlab("") +
  ylab("Tax Revenue 2010 (% of GDP)") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank()) +
  ggtitle("Countries with highest tax revenue as % of GDP")

taxRev.chart
