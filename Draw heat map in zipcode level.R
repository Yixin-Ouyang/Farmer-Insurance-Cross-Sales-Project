library(tidyverse)
library(rgdal)
library(rgeos)
library(maptools)
library(ggalt)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(maps)
library(zipcode)

# Prepare the zip poly data for US
mydata = readOGR(dsn = ".", layer = "cb_2017_us_zcta510_500k")

# Arizona zip code data
data(zipcode)
az = zipcode[zipcode$state=="AZ",]

# Get polygon data for AZ only
mypoly = subset(mydata, ZCTA5CE10 %in% az$zip)


# Drop unnecessary factor levels.
mypoly$ZCTA5CE10 = droplevels(mypoly$ZCTA5CE10)


# Merge polygons using the group variable
# Create a data frame for ggplot.
mypoly.union = unionSpatialPolygons(mypoly, mypoly$ZCTA5CE10)

mymap = fortify(mypoly.union)


# load the dataset
data = read.csv("HAData.csv")
data$AH_DISC_IND = ifelse(data$AH_DISC_IND == 'Y', 1,0)
df = data %>%
  group_by(PROP_ZIP_CD) %>%
  summarise(purchase_rate = sum(AH_DISC_IND)/n())

df2 = data %>%
  group_by(PROP_ZIP_CD) %>%
  summarise(count = n())


# Finally, drawing a graphic
ggplot() +
  geom_cartogram(data = df, aes(fill = purchase_rate, map_id = PROP_ZIP_CD), map = mymap) +
  scale_fill_gradient2(low = "white", high = "darkred") +
  borders("state",regions="arizona", colour = "lightgrey") +
  coord_map() +
  theme_map()

ggplot() +
  geom_cartogram(data = df2, aes(fill = count, map_id = PROP_ZIP_CD), map = mymap) +
  scale_fill_gradient2(low = "white", high = "darkred") +
  borders("state",regions="arizona", colour = "lightgrey") +
  coord_map() +
  theme_map()
