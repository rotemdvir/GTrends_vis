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
library(ggthemes)

#################### Map for search term  #########################

### Google trends data
# Define search timeframe
searchtime = ("2020-03-02 2020-07-15")

# Pull Data
d2 <- gtrends("mask", 
              geo = "US", 
              time = searchtime)

# Create datasets for maps
state <- map_data("state")
d.reg <- d2$interest_by_region %>%
  mutate(region = tolower(location)) %>%
  filter(region %in% state$region) %>%
  select(region, hits) 

# Initial basic map with search term frequency 
plotmap <- ggplot() +
  geom_map(data = state, map = state,
           aes(x = long, y = lat, map_id = region),
           fill="#ffffff", color="#ffffff", size=0.15) +
  geom_map(data = d.reg,
           map = state,
           aes(fill = hits, map_id = region),
           color="#ffffff", size=0.15) +
  scale_fill_continuous(low = 'yellow', high = 'red') +
  ggtitle("Search for term 'Mask': March - July, 2020") +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face = "bold"),legend.position="bottom",legend.text=element_text(size=12),
        legend.background = element_rect(size = 0.5, linetype = "solid",colour = "black"))

plotmap <- plotmap + labs(subtitle = "US - By state", fill = "# of Hits") 

## Create cities layer
# Check high frequency serach by city
cit.dat <- d2$interest_by_city

# Create dataset of main cities and list to fit into map
cit <- data.frame(
  city = c("SanDiego", "LA", "Chicago", "Austin", "Philadelphia"),
  long = c(-117.161087, -118.243683, -87.629799, -97.743057, -75.165222),
  lat = c(32.715736, 34.052235, 41.878113, 30.267153, 39.952583),
  hjust = c(-0.1, -0.1, -0.1, -0.1, -0.1),
  vjust = c(1, 0, 0, 1, 0))

us_format <- list(
  xlab(""), 
  ylab(""),
  coord_map("polyconic"),
  geom_point(aes(long,lat),data=cit,size=1.5),
  geom_text(aes(x=long,y=lat,label=city,hjust=hjust,vjust=vjust),face="bold",size=3.5,data=cit),
  theme_bw(),
  theme(legend.justification=c(1,0),legend.position=c(1,0), legend.background=element_rect(colour="black")))

# Add cities layer to basic map
plot1 <- plotmap + us_format + theme_map()


####################  Comparison Map  ####################
# Comparison data downloaded from google trends as csv file
# Add grouping by most frequent search term
geoMap <- as_tibble(geoMap)
geoMap$grp <- NA
geoMap$grp[geoMap$backyard > 0.5] <- "Backyard"
geoMap$grp[geoMap$backyard < 0.5] <- "Curbside"

# Filter comparison data to necessary variables
geoMap2 <- geoMap %>%
  select(region, grp)

# Join comparison and state datasets for mapping
map_all <- state %>%
  left_join(geoMap2, by = "region")

# Map by states, grouped by search terms frequency
plotbase <- ggplot(data = map_all,
            mapping = aes(x = long, y = lat,
                          group = group, fill = grp))

plot2 <- plotbase + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Google Search Terms: March - July 2020", 
       subtitle = "US - by State",
       fill = "Search term") +
  scale_fill_brewer(palette = "Set1") +
  theme_map()


### Combined maps
ggarrange(plot1, plot2, 
          nrow = 2, ncol = 1)



