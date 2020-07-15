##  R script: Google Trends mini-project  ##
##  Rotem Dvir  ##
##  July 2020  ##

# set WD
setwd("~/Dropbox/TAMU/Extra_Projects/Git_edits/GTrends")

# load packages
library(psych)
library(dplyr)
library(ggplot2)
library(gtrendsR)
library(maps)
library(ggpubr)
library(gridExtra)

####################  Compare terms over time ####################

### Collect data from Google
# Define search terms and time frame
keywords = c("curbside", "backyard")
searchtime = ("2020-03-02 2020-07-06")

d1 <- gtrends(keywords, 
              geo = "US", 
              time = searchtime)

d.trend <- d1$interest_over_time
head(d.trend)

# plot results
plot1 <- ggplot(data=d.trend, aes(x=date, y=hits,group=keyword,col=keyword)) +
  geom_line(size = 1.5) + xlab("") + ylab("Relative Interest (# of Hits)") + theme_pubr()+
  scale_color_manual(values=c('navyblue','maroon')) + ggtitle("Google Trends: March-July 2020") +
  theme(legend.title = element_text(face = "bold"),legend.position="top",legend.text=element_text(size=12),
        legend.background = element_rect(size = 0.5, linetype = "solid",colour = "black")) 

plot1 <- plot1 + labs(col = "Search Terms") + theme(plot.title = element_text(size = 16, face = "bold"))

### Plots to compare data by city
# Create city dataset
d.city <- d1$interest_by_city

# Remove missing
d.city <- d.city %>%
  filter(!is.na(hits))
 
# Create groups by search term (and grouping varible)
d.city1 <- d.city %>%
  filter(keyword == "curbside") 

d.city1$grp <- NA
d.city1$grp[d.city1$hits < 10] <- 0
d.city1$grp[d.city1$hits > 10 & d.city1$hits < 60] <- 1
d.city1$grp[d.city1$hits > 60] <- 2

# Plot for term 1
p.city1 <- ggplot(d.city1, aes(x = location, y = hits, label = location)) +
  geom_point(color = "white") + theme_bw() + ylab("# of Hits") + xlab("") +
  geom_text(aes(label = location, color = factor(grp)),hjust=0.5, vjust=0) + ggtitle("US Cities: Curbside") +
  theme(axis.text.x = element_blank(),
        legend.position = "none") 


d.city2 <- d.city %>%
  filter(keyword == "backyard") 

d.city2$grp <- NA
d.city2$grp[d.city2$hits < 70] <- 0
d.city2$grp[d.city2$hits > 69 & d.city2$hits < 80] <- 1
d.city2$grp[d.city2$hits > 80] <- 2

# Plot for term 2
p.city2 <- ggplot(d.city2, aes(x = location, y = hits, label = location)) +
  geom_point(color = "white") + theme_bw() + ylab("# of Hits") + xlab("") + ylim(50, 100) +
  geom_text(aes(label = location, color = factor(grp)),hjust=0.5, vjust=0) + ggtitle("US Cities: Backyard") +
  theme(axis.text.x = element_blank(),
        legend.position = "none") 

### Compare search terms by state: TX, OH
# Create two datasets of terms by state
d.tx <- gtrends(keywords, 
              geo = "US-TX", 
              time = searchtime)

d.oh <- gtrends(keywords, 
                geo = "US-OH", 
                time = searchtime)

d.tx1 <- d.tx$interest_over_time
d.oh1 <- d.oh$interest_over_time

# Join datasets to one for comparison
comp.data <- bind_rows(d.tx1, d.oh1)

# Plot for comparison
plot.comp <- ggplot(data=comp.data, aes(x=date, y=hits,group=keyword,col=keyword)) +
  geom_line(size = 1.5) + xlab('') + ylab('Relative Interest') + theme_bw() + scale_color_manual(values=c('navyblue','maroon')) +
  theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12), 
        plot.title = element_text(size = 14, face = "bold")) + 
  ggtitle("US States: Compare terms over time") + facet_wrap(~geo, ncol = 1)
plot.comp

## combine plots
grid.arrange(arrangeGrob(plot1, p.city1, p.city2), plot.comp, ncol = 2)



