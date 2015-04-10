
load(file.path(pathOut, 'orgData.RData'))
library('rgdal'); library('ggplot2'); library('grid')
setwd('/home/dag/Dropbox/Buero/Dissertation/2015/duke/classes/mle/termPaper/dta/mapdta')

theme_nothing <- function(base_size = 12, base_family = "serif")
  {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
            rect             = element_blank(),
            line             = element_blank(),
            #text             = element_blank(),
            axis.ticks.margin = unit(0, "lines")
           )
  }
# check that it is a complete theme
attr(theme_nothing(), "complete")


map.bbox <- readOGR(dsn="ne_110m_wgs84_bounding_box.shp", layer="ne_110m_wgs84_bounding_box")
map.bbox <- spTransform(map.bbox, CRS("+proj=wag4"))
map.land <- readOGR(dsn="ne_110m_land.shp", layer="ne_110m_land")
map.land <- spTransform(map.land, CRS("+proj=wag4"))
map.ctry <- readOGR(dsn="ne_110m_admin_0_countries.shp", layer="ne_110m_admin_0_countries")
map.ctry <- spTransform(map.ctry, CRS("+proj=wag4"))
map.bbox <- fortify(map.bbox)
map.land <- fortify(map.land)
map.ctry <- fortify(map.ctry, region = 'iso_a3')

map.dta <- subset(org.dta, 
  year == 2007, select = c('cowcode', 'cooptation_discrete', 'geddes_casename')
)
library('countrycode')
map.dta <- within(map.dta, {
  id <- countrycode(
    sourcevar = cowcode, origin = 'cown', destination = 'iso3c', 
    warn = TRUE
  )
  id <- ifelse(cowcode == 678, 'YEM', id)
  cooptation_discrete <- as.character(cooptation_discrete)
  }
)
table(map.dta[, 'id'], map.dta[, 'cooptation_discrete'], exclude = 'NULL')
detach(package:countrycode)
library(ETLUtils)
map.dta <- matchmerge(
  x = map.ctry, y = map.dta, 
  by.x = map.ctry[, "id"],
  by.y = map.dta[, "id"],
  by.iskey = TRUE,
  all.x = TRUE,
  check.duplicates = FALSE,
  trace = TRUE
)
detach(package:ETLUtils)
detach(package:ff)
detach(package:bit)
map.dta <- within(map.dta, {
  cooptation_discrete <- ifelse(is.na(cooptation_discrete), 'Democracy', cooptation_discrete)
  }
)
# library('RColorBrewer')
# colors <- brewer.pal(n = 5, name = 'Dark2')
#colors <- c('transparent', '#9E3173', '#0380B5', '#619933', '#727272')
colors <- c('transparent', '#9E3173', '#9E3173', '#9E3173', '#727272')

p <- ggplot() +
geom_polygon(
  data = map.bbox, aes(x = long, y = lat), 
  fill = 'transparent', colour = 'black', size = .1, show_guide = FALSE
) +
geom_polygon(
  data = map.dta, 
  aes(x = long, y = lat, group = group, fill = as.factor(cooptation_discrete)), 
  colour = 'white', size = .1, show_guide = FALSE
) +
geom_polygon(
  data = map.land, aes(x = long, y = lat, group = group),
  fill = 'transparent', colour = 'black', size = .1, show_guide = FALSE
) +
scale_fill_manual(values = colors, 
  labels = c("", "Singleparty parl.", 
    'Multiparty parl.', 'Single party only', 
    'Neither'
  )
) +
labs(title = 'Autocracies in 2007', fill = '2007', y= '2007') +
theme_nothing(base_size = .5*12) +
theme(
  legend.position = c(.16, .525), legend.direction = 'vertical',
  legend.title = element_blank(),
  legend.key.size = unit(.5, "lines"),
  legend.background = element_rect(fill = 'white', colour = 'black'),
  axis.text = element_blank(),
  axis.title = element_blank(),
  title = element_blank(),
  plot.margin = unit(c(-.9, -.7, -1.2, -1.5), 'lines')
)
ggsave(
  plot = p, 
  file = file.path(pathOut, 'worldmapEyecatcher.pdf'),
  width = 3, height = 3/1.618, family = 'serif'
)