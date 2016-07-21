library(data.table)
library(RColorBrewer)
library(ggmap)
library(rgdal)
library(readxl)
library(broom)
library(maptools)

# Inputs
seifa_sa1_xls <- '/Users/wilsonpok/R_scripts/shapefiles/2033.0.55.001 sa1 indexes.xls'
sa1 <- readOGR(dsn = '/Users/wilsonpok/R_scripts/shapefiles/1270055001_sa1_2011_aust_shape', layer = 'SA1_2011_AUST')

# Outputs
map_dir <- '/Users/wilsonpok/R_scripts/shapefiles/maps/'
dir.create(map_dir, showWarnings = FALSE, recursive = TRUE)

#----------------------------------
# Functions
#----------------------------------

get_city_sa1 <- function(city){
  tmp <- sa1[sa1@data$GCC_NAME11 == city,]
  tmp@data$id <- tmp@data$SA1_7DIG11
  tmp.points <- data.table(tidy(tmp, region = 'id'))
  setkey(tmp.points, id)
  return(tmp.points)
}

get_city_map <- function(city){
  city_map <- get_map(location = city,
                      color = "bw",
                      source = "stamen",
                      maptype = "toner",
                      zoom = 11)
  return(city_map)
}

plot_map_cat <- function(map, seifa, var){
  p <- ggmap(map) +
    geom_polygon(data = seifa, aes_string(x = 'long', y = 'lat', group = 'group', fill = var), color = NA, alpha = 0.7) +
    scale_fill_brewer(type = 'div', palette = 2) +
    theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.title = element_blank())
  return(p)
}

save_map <- function(filename){
  ggsave(paste0(map_dir, filename), width = 8, height = 8)
}

#----------------------------------
# SEIFA
#----------------------------------
seifa_sa1_tbl <- read_excel(path = seifa_sa1_xls, sheet = 2, col_names = FALSE, skip = 6)
seifa_sa1 <- data.table(seifa_sa1_tbl)

seifa_sa1 <- seifa_sa1[!is.na(sa1)]

setnames(seifa_sa1, c('sa1', 'irsead_sc', 'irsead_dec'
                      , 'irsed_sc', 'irsed_dec'
                      , 'ier_sc', 'ier_dec'
                      , 'ieo_sc', 'ieo_dec', 'pop'))

seifa_sa1[, sa1 := as.character(sa1)]
seifa_sa1[, irsead_dec := as.factor(irsead_dec)]
seifa_sa1[, ieo_dec := as.factor(ieo_dec)]
setkey(seifa_sa1, sa1)

#----------------------------------
# SA1
#----------------------------------
summary(sa1)
summary(sa1[sa1@data$GCC_NAME11 == 'Greater Sydney',])

syd.points <- get_city_sa1('Greater Sydney')
mel.points <- get_city_sa1('Greater Melbourne')
bne.points <- get_city_sa1('Greater Brisbane')

str(syd.points)
summary(syd.points)


#----------------------------------
# Join SA1 to SEIFA
#----------------------------------
syd.seifa <- seifa_sa1[syd.points]
mel.seifa <- seifa_sa1[mel.points]
bne.seifa <- seifa_sa1[bne.points]

#----------------------------------
# Plot maps
#----------------------------------
sydmap <- get_city_map(city = 'Sydney')
melmap <- get_city_map(city = 'Melbourne, Australia')
bnemap <- get_city_map(city = 'Brisbane')


# irsead_dec
plot_map_cat(map = sydmap, seifa = syd.seifa, var = 'irsead_dec')
save_map('irsead_dec_syd.png')

plot_map_cat(map = melmap, seifa = mel.seifa, var = 'irsead_dec')
save_map('irsead_dec_mel.png')

plot_map_cat(map = bnemap, seifa = bne.seifa, var = 'irsead_dec')
save_map('irsead_dec_bne.png')


# ieo_dec
plot_map_cat(map = sydmap, seifa = syd.seifa, var = 'ieo_dec')
save_map('ieo_dec_syd.png')

plot_map_cat(map = melmap, seifa = mel.seifa, var = 'ieo_dec')
save_map('ieo_dec_mel.png')

plot_map_cat(map = bnemap, seifa = bne.seifa, var = 'ieo_dec')
save_map('ieo_dec_bne.png')

