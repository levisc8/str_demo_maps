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
  dbFetch <- env[[x]]

  # Deal with differences between s4 and s3 versions of database

  if(inherits(dbFetch, 'list')) {
    dbFetch <- dbFetch[[1]]
  } else if(inherits(dbFetch, "CompadreDB")) {
    dbFetch <- dbFetch@data
  } else {
    stop("Cannot recognize class of currently fetched com(p)adre object",
         call. = FALSE)
  }

  sel_cols <- c('SpeciesAccepted', 'Kingdom', 'Lat', 'Lon')

  dbOut <- dbFetch[ , sel_cols]

  dbOut <- dbOut[!is.na(dbOut$Kingdom) , ]
  dbOut <- dbOut[!is.na(dbOut$Lat) & !is.na(dbOut$Lon), ]

  return(dbOut)

}


# collect compadre data

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

kingdoms <- unique(db$Kingdom)

for(i in seq_along(kingdoms)) {

  if(kingdoms[i] == "NA") next

  use_king <- kingdoms[i]

  col_path <- paste('figures/compadre-padrino-worldmap-color-',
                    use_king,
                    '.png',
                    sep = '')
  bw_path <- paste('figures/compadre-padrino-worldmap-bw-',
                    use_king,
                    '.png',
                    sep = '')

  use_db <- subset(db, Kingdom == use_king)

  png(filename = col_path,
      height = 12,
      width = 8,
      units = 'in',
      res = 450)

    plt <- ggplot(wrld,
                  aes(x     = long,
                      y     = lat,
                      group = group)) +
      geom_polygon(color = 'grey50',
                   fill  = NA) +
      coord_map(xlim  = c(-180, 180)) +
      geom_point(data = use_db,
                 aes(color = Model,
                     x     = Lon,
                     y     = Lat),
                 inherit.aes = FALSE,
                 alpha       = 0.5) +
      theme(panel.background = element_rect(fill = NA),
            panel.grid = element_line(),
            legend.key = element_rect(fill  = NA,
                                      color = 'black'),
            legend.key.size = unit(0.03, 'npc'),
            legend.text     = element_text(size  = 14),
            legend.title    = element_text(size = 18)) +
      scale_color_manual(breaks = c("MPM", "IPM"),
                         values = viridis::inferno(2,
                                                   begin     = 0.7,
                                                   end       = 0,
                                                   direction = -1)) +
      scale_x_continuous("Longitude") +
      scale_y_continuous("Latitude",
                         breaks = c(-45, 0, 45)) +
      ggtitle(use_king)

    print(plt)


  dev.off()

  png(filename = bw_path,
      height   = 12,
      width    = 8,
      units    = 'in',
      res      = 450)

    plt <- ggplot(wrld,
                  aes(x     = long,
                      y     = lat,
                      group = group)) +
      geom_polygon(color = 'grey50',
                   fill  = NA) +
      coord_map(xlim  = c(-180, 180)) +
      geom_point(data = use_db,
                 aes(color = Model,
                     x     = Lon,
                     y     = Lat),
                 inherit.aes = FALSE,
                 alpha       = 0.5) +
      scale_color_manual(breaks = c("MPM", "IPM"),
                         values = grey(c(0, 0.5))) +
      theme(panel.background = element_rect(fill = NA),
            panel.grid = element_line(),
            legend.key = element_rect(fill = NA,
                                      color = 'black'),
            legend.key.size = unit(0.03, 'npc'),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 18)) +
      scale_x_continuous("Longitude") +
      scale_y_continuous("Latitude",
                         breaks = c(-45, 0, 45)) +
      ggtitle(use_king)

    print(plt)


  dev.off()

}

