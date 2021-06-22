library(geojsonR)
library(ggplot2)
library(sf)
library(leaflet)
library(raster)
library(rgdal)
library(dplyr)
library(data.table)


# fonction trouv?e sur internet pour estimer la densit?
st_kde <- function(points,cellsize, bandwith, extent = NULL){
  require(MASS)
  require(raster)
  require(sf)
  if(is.null(extent)){
    extent_vec <- st_bbox(points)[c(1,3,2,4)]
  } else{
    extent_vec <- st_bbox(extent)[c(1,3,2,4)]
  }
  n_y <- ceiling((extent_vec[4]-extent_vec[3])/cellsize)
  n_x <- ceiling((extent_vec[2]-extent_vec[1])/cellsize)
  extent_vec[2] <- extent_vec[1]+(n_x*cellsize)-cellsize
  extent_vec[4] <- extent_vec[3]+(n_y*cellsize)-cellsize
  coords <- st_coordinates(points)
  matrix <- kde2d(coords[,1],coords[,2],h = bandwith,n = c(n_x,n_y),lims = extent_vec)
  raster(matrix)
}

setwd("I:/Documentos/5A/Stage Inge/DATA/GeoFabrik PaysLoire/ExportsCalcDensite")

boutiques <- st_read("BOUTIQUES.shp")
restaurants <- st_read("RESTAURANTS.shp")
tramway <- st_read("TRAMWAY.shp")
zonepieton <- st_read("ZPB_EXPLODE2.shp")
extent_zone <- st_bbox(zonepieton)[c(1,3,2,4)]

#estimation des densit?s
boutiques_dens <- st_kde(boutiques,20,400,extent = extent_zone)
restaurants_dens <- st_kde(restaurants,20,400,extent = extent_zone)
tramway_dens <- st_kde(tramway,20,400,extent = extent_zone)

#on donne les attributs de projection aux raster de densit? cr?es
projection(boutiques_dens) <- projection(boutiques)
projection(tramway_dens) <- projection(tramway)
projection(restaurants_dens) <-  projection(restaurants)

#Coeffs du travail de L?o
coeff_boutiques <-  5593
coeff_resto <-  3042
coeff_tram <-  36922

# on aligne les raster si jamais leur origine sont diff?rentes
template<- projectRaster(to = restaurants_dens, from= boutiques_dens, alignOnly=TRUE)

#template is an empty raster that has the projected extent of r2 but is aligned with r1 (i.e. same resolution, origin, and crs of r1)
resto_aligned<- projectRaster(from = restaurants_dens, to= template)
tram_aligned <-  projectRaster(from=tramway_dens, to=template)

# mosaic fait la somme des rasters
merged_rasters<- mosaic(coeff_boutiques*boutiques_dens,coeff_resto*resto_aligned, coeff_tram*tram_aligned, fun=sum, na.rm=TRUE)

# vectorisation du raster merg?
mailles_densiy_vec_sf <-  rasterToPolygons(merged_rasters, dissolve = F)
mailles_densiy_vec_sf <- st_as_sf(mailles_densiy_vec_sf)

# sauvegarde en fichier si besoin
st_write(Pietons_vec_sF, "pietons_dens_vec.geojson",delete_dsn = T)
# reprojection lambert 93

# A verifier , c'est sans doute faux
mailles_densiy_vec_sf <- st_transform(mailles_densiy_vec_sf, 2154)

#cellules dans zone marchable
xx <- st_intersection(zonepieton, mailles_densiy_vec_sf)
st_write(xx, "I:/Documentos/5A/Stage Inge/DATA/GeoFabrik PaysLoire/ExportsCalcDensite/ZonesMarchables.shp",layer = "ZonesMarchables")

ZonesMarchables <- xx

# affectation du nombre de pi?tons par cellules
MAX_PIETONS <- 10
xx$nb_pietons <-  (xx$layer - min(xx$layer)) / (max(xx$layer)- min(xx$layer))
xx$nb_pietons <- xx$nb_pietons * MAX_PIETONS
xx$nb_pietons <- round(xx$nb_pietons)
plot(xx["nb_pietons"])

######################################
# ECHANTILLONAGE SPATIAL DES POINTS
# Première façon avec une boucle
######################################

# filtrer les mailles trop petites
# il n'est pas r?aliste de mettre  1 personne dans moins d'un  m?tres carr? !
# ? discuter entre nous
# filtrage des mailles de moins d'un m?tre carr?
cells_to_fill <- xx[as.numeric(st_area(xx))  > 1,]
# pas la peine de remplir les mailles avec une densit? nulle
cells_to_fill <- cells_to_fill[cells_to_fill$nb_pietons > 0 ,]
plot(cells_to_fill["nb_pietons"])
sourcesPietons <-  list()
for (i in 1:nrow(cells_to_fill)){
  c <-cells_to_fill$geometry[i]
  n <- cells_to_fill$nb_pietons[i]
  cat("maille", i,": ", n, "points dans" , st_area(c), "m?\n")
  pts <- st_sample(c,n, type="regular") %>% st_sf()
  if(nrow(pts)>0){
    sourcesPietons[[i]] <- pts
  }
}
yy <- rbindlist(sourcesPietons)
yy <- yy %>% st_sf()
# attention le plot d?conne , mais l'ouverture dans Qgis confirme que c'est localis? dans la zone marchable
st_write(yy, "I:/Documentos/5A/Stage Inge/DATA/GeoFabrik PaysLoire/ExportsCalcDensite/sourcesPietonsPaul2.shp",layer = "sourcespietons2")
#affichage simple
plot(cells_to_fill$geometry, lwd=0.1)
plot(yy, add=T, cex=0.1, col="orange")
dev.off()

#---------------------------------
# Deuxi?me fa?on d'?chantilloner les points
#   (chez moi ?a fonctionne ^^)
#----------------------------------------
sourcesPietons <-  st_sample(cells_to_fill$geometry, size=cells_to_fill$nb_pietons, type="regular")
st_write(sourcesPietons, "I:/Documentos/5A/Stage Inge/DATA/GeoFabrik PaysLoire/ExportsCalcDensite/sourcesPietonsPaul.shp",layer = "sourcespietons")
library(raster)
#intersection entre sources et rasters de densit?s
dens_boutiques <-  extract(boutiques_dens, sourcesPietons %>% as_Spatial())
dens_tramway <-  extract(tramway_dens, sourcesPietons %>% as_Spatial())
dens_restaurants <- extract(restaurants_dens, sourcesPietons %>% as_Spatial())
# affectation des attributs
sourcesPietons$dens_boutiques <-  dens_boutiques
sourcesPietons$dens_traway <-  dens_tramway
sourcesPietons$dens_restaurants <-  dens_restaurants

##
#Trosieme fa?on
#Left join
##
sourcespietons_alt <- ZonesMarchables %>% filter(nb_pietons > 0) %>% st_centroid() #Filtrage des points avec une valeur = 0
# Convert to data tables
sourcespietons_alt$nb_pietons  <- ZonesMarchables$nb_pietons






dt1 <- data.table(BDD_Info) 
dt2 <- data.table(sourcespietons_alt)
#left join random
BDD_Info$nb_pietons <-BDD_Info$Nb.Pers
# dt3 <- dt1[dt2, on = .(nb_pietons),
#     {ri <- sample(.N, 1L)
#     .(Homme = Homme[ri], ID = ID[ri] )},by = .EACHI]
T4 <- dt2[, c("ID") := dt1[sample(.N)][.SD, on=.(nb_pietons), mult="first", .(x.ID)]]
st_write(T4, "I:/Documentos/5A/Stage Inge/DATA/GeoFabrik PaysLoire/ExportsCalcDensite/T4.shp",layer = "T4") #Export T4
