# functions

cdb_fetch <- function(cdb) {
  # get url or path
  if (tolower(cdb) == 'comadre') {
    path <- url('https://compadre-db.org/Data/ComadreDownload')
  } else if (tolower(cdb) == 'compadre') {
    path <- url('https://compadre-db.org/Data/CompadreDownload')
  } else {
    path <- path.expand(cdb)
  }
  # fetch and load
  env <- new.env()
  x <- load(path, env)[1]
  dbFetch <- env[[x]]@data

  sel_cols <- c('SpeciesAccepted', 'Kingdom', 'Lat', 'Lon')

  dbOut <- dbFetch[ , sel_cols]

  dbOut <- dbOut[!is.na(dbOut$Kingdom) , ]
  dbOut <- dbOut[!is.na(dbOut$Lat) & !is.na(dbOut$Lon), ]

  return(dbOut)

}


# collect compadre data

source('functions.R')
cpd <- cdb_fetch('compadre')
cmd <- cdb_fetch('comadre')

db <- rbind(cmd, cpd)

db$Model <- 'MPM'

# read in padrino data. Padrino is currently static and there's no where
# to harvest the coordinates online. This will remain true for a while,
# so created a table with species name, kingdom, lat, and long data. Stored
# on github, should be readable in the CRON environment (I think).

pdb <- read.csv('padrino/padrino.csv',
                stringsAsFactors = FALSE)

pdb$Model <- 'IPM'

pdb$Lat <- as.numeric(pdb$Lat)
pdb$Lon <- as.numeric(pdb$Lon)

pdb <- pdb[!is.na(pdb$Lat) & !is.na(pdb$Lon), ]
db <- rbind(db, pdb)

# Build plots

library(ggplot2)
library(viridis)

wrld <- map_data('world')

dir.create('figures', FALSE)

png(filename = 'figures/compadre-padrino-worldmap-color.png',
    height = 12,
    width = 8,
    units = 'in',
    res = 450)

  ggplot(wrld, aes(x = long, y = lat, group = group)) +
    geom_polygon(color = 'grey50',
                 fill = NA) +
    coord_map(xlim = c(-180, 180)) +
    geom_point(data = db,
               aes(shape = Model,
                   color = Kingdom,
                   x = Lon,
                   y = Lat),
               inherit.aes = FALSE,
               alpha = 0.9) +
    theme(panel.background = element_rect(fill = NA),
          panel.grid = element_line(),
          legend.key = element_rect(fill = NA,
                                    color = 'black'),
          legend.key.size = unit(0.03, 'npc'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 18)) +
    scale_color_manual(breaks = c('Animalia',
                                  'Chromalveolata',
                                  'Chromista',
                                  "Fungi",
                                  'Plantae'),
                       values = viridis::viridis(5)) +
    scale_x_continuous("Longitude") +
    scale_y_continuous("Latitude",
                       breaks = c(-45, 0, 45))


dev.off()

png(filename = 'figures/compadre-padrino-worldmap-bw.png',
    height = 12,
    width = 8,
    units = 'in',
    res = 450)

ggplot(wrld, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = 'grey50',
               fill = NA) +
  coord_map(xlim = c(-180, 180)) +
  geom_point(data = db,
             aes(shape = Model,
                 color = Kingdom,
                 x = Lon,
                 y = Lat),
             inherit.aes = FALSE) +
  theme(panel.background = element_rect(fill = NA),
        panel.grid = element_line(),
        legend.key = element_rect(fill = NA,
                                  color = 'black'),
        legend.key.size = unit(0.03, 'npc'),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18)) +
  scale_color_manual(breaks = c('Animalia',
                                'Chromalveolata',
                                'Chromista',
                                "Fungi",
                                'Plantae'),
                     values = grey(seq(0, 0.8, length.out = 5))) +
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude",
                     breaks = c(-45, 0, 45))


dev.off()
