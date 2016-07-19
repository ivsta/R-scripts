library(data.table)
library(maptools)
library(RColorBrewer)
library(ggmap)
library(rgdal)
library(fastshp)

parks_shapefile <- '/Users/wilsonpok/R_scripts/shapefiles/ne_10m_parks_and_protected_lands/ne_10m_parks_and_protected_lands_area.shp'
area <- readShapePoly(fn = parks_shapefile, verbose = TRUE)

colors <- brewer.pal(9, "BuGn")

mapImage <- get_map(location = c(lon = -118, lat = 37.5),
                    color = "color",
                    source = "osm",
                    # maptype = "terrain",
                    zoom = 6)


area.points <- fortify(area)

ggmap(mapImage) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = area.points,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")

#====================

suburbs <- readOGR(dsn = '/Users/wilsonpok/R_scripts/shapefiles/1270055003_ssc_2011_aust_shape', layer = 'SSC_2011_AUST')

suburbs.points <- data.table(fortify(suburbs))

syd.suburbs <- suburbs.points[lat <= -33.4 & lat >= -34.4]

rm(suburbs.points)

Sydmap <- get_map(location = 'Sydney',
                    color = "bw",
                    # source = "osm",
                    maptype = "terrain",
                    zoom = 10)

ggmap(Sydmap) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = syd.suburbs,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")

# #====================
# 
# suburbs <- read.shp('/Users/wilsonpok/R_scripts/shapefiles/1270055003_ssc_2011_aust_shape/SSC_2011_AUST.shp')
