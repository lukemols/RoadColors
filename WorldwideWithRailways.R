#-----------you need to install the following packages. this only needs to be done once.
#install.packages(c('sf', 'foreign', 'tidyverse', 'stringi', 'lwgeom'))

#-----------initialize libraries. This needs to be done for each new R session 
library(sf)
library(foreign)
library(tidyverse)
library(lwgeom)
library(stringi)
options(stringsAsFactors = FALSE)

#-----------download files
#pick a region and download/unzip the .shp.zip file: http://download.geofabrik.de/

#-----------set the working directory to wherever you unzipped the downloaded files to
setwd("C:/Users/luca.mollo/Downloads/NordOvestMap")


#-----------set some basic info about the city you're mapping
city <- "taggia"
lat <- 43.843858 #center point latitude
long <- 7.850928 #center point longitude
rad <- 5000 #radius, in meters, around the center point to map
crs <- 102013 #ESRI projection for mapping. I found mine here: https://spatialreference.org/ref/esri/europe-albers-equal-area-conic/ 102013 will give good results for Europe.


#-----------set up the road types you want to plot and what colors they should be
plottypes <-  c('Via', 'Autostrada', 'Strada', 'Corso', 'Galleria', 'San', 'Lungo', 'Pista', 'Madonna', 'Salita', 'Piazza', 'Svincolo', 'Mulattiera', 'Vico', 'Viadotto', 'Santo', 'Aurelia', 'Argine', 'Lungomare', 'Ponte', 'Viale', 'Antica', 'Banchette', 'Porta', 'Piazzale', 'Traversa', 'Salida')
plotcolors <-  c('Via' = '#59c8e5', 'Autostrada' = '#ff9223', 'Strada' = '#37ed64', 'Corso' = '#429657', 
                 'Galleria' = '#e81a1a', 'San' = '#fed032', 'Lungo' = '#a27ffa', 'Pista' = '#dbb435', 
                 'Madonna' = '#ba9c3a', 'Salita' = '#a18d4c', 'Piazza' = '#fe4d64', 'Svincolo' = '#cf6700',
                 'Mulattiera' = '#ab9e76', 'Vico' = '#1696db', 'Viadotto' = '#e3a96f', 'Santo' = '#c46666', 
                 'Aurelia' = '#ff0000', 'Argine' = '#9872f7', 'Lungomare' = '#6a37ed', 'Ponte' = '#b09ede', 
                 'Viale' = '#47c466', 'Antica' = '#80caf2', 'Banchette' = '#f2b1b1', 'Porta' = '#d19099', 
                 'Piazzale' = '#eb102e', 'Traversa' = '#7faec7', 'Salida' = '#ab9038', 'Other' = '#cccccc', 
                 'Unnamed' ='#7F7F7F','Rail' ='#000000')
#ff9223
#-----------get to plotting
#import  road geography
filename <- "gis_osm_roads_free_1"
allroads <- read_sf(".", filename)

rail_filename <- "gis_osm_railways_free_1"
allrailways <- read_sf(".", rail_filename)

#subset the roads into a circle.
pt <- data.frame(lat = lat, long = long)
pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>%  st_transform(crs) 
circle <- st_buffer(pt, dist = rad)
circle_roads <- circle %>% st_transform(st_crs(allroads))
circle_rails <- circle %>% st_transform(st_crs(allrailways))
allroads <- st_intersection(circle_roads, allroads)
allrailways <- st_intersection(circle_rails, allrailways)

#remove unnamed footpaths
unnamed <- allroads[(allroads$fclass  == "footway" | is.na(allroads$name)),]
allroads <- allroads[!(allroads$fclass  == "footway" & is.na(allroads$name)),]
allrailways <- allrailways[(allrailways$fclass  == "rail"),]

#add in length 
allroads$len <- st_length(allroads)
allrailways$len <- st_length(allrailways)

#-----derive road suffixes-----
#run this line if your suffixes are at the END of the name (e.g. Canal Street)
#allroads$TYPE <- substr(allroads$name, stri_locate_last(allroads$name, regex = " ")[, 1] + 1,  nchar(allroads$name)) %>% stri_trans_general(id = "Title")

#run this line if your "suffixes" are at the BEGINNING of the name (e.g. Calle de los Gatos) for road prefixes
allroads$TYPE <- substr(allroads$name, 1,  str_locate(allroads$name, " ")[, 1] -1)  %>% stri_trans_general(id = "Title")
allrailways$TYPE <- "Rail"

#--------uncomment and run this code to get the top roads by length.
#--------i usually run this to decide what road types to plot
#plottype <- allroads %>% select(TYPE,len)
#plottype$geometry <- NULL
#plottype <- subset(plottype, !is.na(TYPE))
#plottype <- plottype %>% group_by(TYPE) %>% summarise(Length = sum(len)) %>% arrange(-Length) 


#rename motorways that don't have some other designation
allroads$TYPE[allroads$fclass == 'motorway' & !(allroads$TYPE %in% plottypes)] <- "Motorway"
unnamed$TYPE[TRUE] <- "Unnamed"

#put other roads into their own dataframe
allroads$TYPE[!(allroads$TYPE %in% plottypes) & allroads$TYPE != 'Motorway'] <- "Other"
otherroads <- allroads[(allroads$TYPE  == "Other"),]
allroads <- allroads[(allroads$TYPE  != "Other"),]

#plot it
blankbg <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

ggplot() + blankbg + theme(panel.grid.major = element_line(colour = "transparent")) + 
  geom_sf(data=otherroads, size = .8, aes(color=TYPE)) + 
  geom_sf(data=unnamed, size=.8, aes(color=TYPE)) +
  geom_sf(data=allrailways, size=.8, aes(color=TYPE)) +
  geom_sf(data=allroads, size =1, aes(color=TYPE)) + 
  scale_color_manual(values = plotcolors, guide = "legend") 

ggsave(paste0("", city, ".png"), plot = last_plot(),
       scale = 1, width = 40, height = 40, units = "cm",
       dpi = 600)


