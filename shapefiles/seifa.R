library(data.table)
library(RColorBrewer)
library(ggmap)
library(rgdal)
library(readxl)
library(broom)
library(gpclib)

gpclibPermit()


#----------------------------------
# SEIFA
#----------------------------------
seifa_sa1_xls <- '/Users/wilsonpok/R_scripts/shapefiles/2033.0.55.001 sa1 indexes.xls'
seifa_sa1_tbl <- read_excel(path = seifa_sa1_xls, sheet = 2, col_names = FALSE, skip = 6)
seifa_sa1 <- data.table(seifa_sa1_tbl)

seifa_sa1 <- seifa_sa1[!is.na(sa1)]

setnames(seifa_sa1, c('sa1', 'irsead_sc', 'irsead_dec'
                      , 'irsed_sc', 'irsed_dec'
                      , 'ier_sc', 'ier_dec'
                      , 'ieo_sc', 'ieo_dec', 'pop'))

seifa_sa1[, sa1 := as.character(sa1)]
setkey(seifa_sa1, sa1)


#----------------------------------
# SA1
#----------------------------------

sa1 <- readOGR(dsn = '/Users/wilsonpok/R_scripts/shapefiles/1270055001_sa1_2011_aust_shape', layer = 'SA1_2011_AUST')

summary(sa1)
summary(sa1$GCC_NAME11)

summary(sa1@data$GCC_NAME11)
summary(sa1[sa1@data$GCC_NAME11 == 'Greater Sydney',])

syd <- sa1[sa1@data$GCC_NAME11 == 'Greater Sydney',]
summary(syd)

syd@data$id <- syd@data$SA1_7DIG11


syd.points <- data.table(tidy(syd, region = 'id'))
str(syd.points)
summary(syd.points)


setkey(syd.points, id)



syd.seifa <- seifa_sa1[syd.points]

Sydmap <- get_map(location = 'Sydney',
                  color = "bw",
                  # source = "osm",
                  maptype = "terrain",
                  zoom = 11)

ggmap(Sydmap) +
  geom_polygon(data = syd.seifa, aes(x = long, y = lat, group = group, fill = irsead_dec)
               , color = NA, alpha = 0.7) +
  scale_fill_continuous(low = 'red', high = 'blue') +
  labs(x = "Longitude", y = "Latitude")

ggmap(Sydmap) +
  geom_polygon(data = syd.seifa, aes(x = long, y = lat, group = group, fill = as.factor(irsead_dec))
               , color = NA, alpha = 0.7) +
  scale_fill_brewer(type = 'div', palette = 2) +
  labs(x = "Longitude", y = "Latitude")


