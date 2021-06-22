library(geojsonR)
library(ggplot2)
library(sf)
library(leaflet)
library(raster)
library(rgdal)
library(dplyr)
library(data.table)


# fonction trouvée sur internet pour estimer la densité
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


setwd("~/LASSO/PedestrianActivityModel/spatial_sample_sources_pietons/ExportsCalcDensite/")

boutiques <- st_read("BOUTIQUES.shp")
restaurants <- st_read("RESTAURANTS.shp")
tramway <- st_read("TRAMWAY.shp")
zonepieton <- st_read("ZPB_EXPLODE2.shp")


extent_zone <- st_bbox(zonepieton)[c(1,3,2,4)]

#estimation des densités
boutiques_dens <- st_kde(boutiques,10,400,extent = extent_zone)
restaurants_dens <- st_kde(restaurants,10,400,extent = extent_zone)
tramway_dens <- st_kde(tramway,10,400,extent = extent_zone)

#on donne les attributs de projection aux raster de densité crées
projection(boutiques_dens) <- projection(boutiques)
projection(tramway_dens) <- projection(tramway)
projection(restaurants_dens) <-  projection(restaurants)


coeff_boutiques <-  5593
coeff_resto <-  3042
coeff_tram <-  36922


# on aligne les raster si jamais leur origine sont différentes
template<- projectRaster(to = restaurants_dens, from= boutiques_dens, alignOnly=TRUE)
#template is an empty raster that has the projected extent of r2 but is aligned with r1 (i.e. same resolution, origin, and crs of r1)
resto_aligned<- projectRaster(from = restaurants_dens, to= template)
tram_aligned <-  projectRaster(from=tramway_dens, to=template)
# mosaic fait la somme des rasters
merged_rasters<- mosaic(coeff_boutiques*boutiques_dens,coeff_resto*resto_aligned, coeff_tram*tram_aligned, fun=sum, na.rm=TRUE)


# vectorisation du raster mergé
mailles_densiy_vec_sf <-  rasterToPolygons(merged_rasters, dissolve = F)
mailles_densiy_vec_sf <- st_as_sf(mailles_densiy_vec_sf)

# sauvegarde en fichier si besoin
#st_write(Pietons_vec_sF, "pietons_dens_vec.geojson",delete_dsn = T)


# reprojection lambert 93
# A verifier , c'est sans doute faux
mailles_densiy_vec_sf <- st_transform(mailles_densiy_vec_sf, 2154)


#cellules dans zone marchable
xx <- st_intersection(zonepieton, mailles_densiy_vec_sf)



# affectation du nombre de piétons par cellules
# renormalisation entre 0 et MAXPIETONS
MAX_PIETONS <- 10
xx$nb_pietons <-  (xx$layer - min(xx$layer)) / (max(xx$layer)- min(xx$layer))
xx$nb_pietons <- xx$nb_pietons * MAX_PIETONS
xx$nb_pietons <- round(xx$nb_pietons)
plot(xx["nb_pietons"])





######################################
# ECHANTILLONAGE SPATIAL DES POINTS
######################################

# Solution triviale 
#rasteriser à 10m , et prendre le centroide des mailles , affecté de la valeur du raster 


####################################"
# Première façon avec une boucle
######################################

# filtrer les mailles trop petites  
# il n'est pas réaliste de mettre  1 personne dans moins d'un  mètres carré ! 
# À discuter entre nous

# filtrage des mailles de moins d'un mètre carré
cells_to_fill <- xx[as.numeric(st_area(xx))  > 1,]
# pas la peine de remplir les mailles avec une densité nulle
cells_to_fill <- cells_to_fill[cells_to_fill$nb_pietons > 0 ,]

plot(cells_to_fill["nb_pietons"])



# le ty)pe de sampling peut être "regular" ou "random" 
typesampling <-  "random"

sourcesPietons <-  list()
for (i in 1:nrow(cells_to_fill)){
  c <-cells_to_fill$geometry[i]
  n <- cells_to_fill$nb_pietons[i]
  cat("maille", i,": ", n, "points dans" , st_area(c), "m²\n")
  pts <- st_sample(c,n, type=typesampling) %>% st_sf()
    if(nrow(pts)>0){
      sourcesPietons[[i]] <- pts
    }
}
yy <- rbindlist(sourcesPietons)
yy <- yy %>% st_sf()
# attention le plot déconne , mais l'ouverture dans Qgis confirme que c'est localisé dans la zone marchable 

#st_write(yy, "../sourcesPietons_2.shp",layer = "sourcespietons")


#affichage simple
plot(cells_to_fill$geometry, lwd=0.1)
plot(yy, add=T, cex=0.1, col="orange")
dev.off()



##################################
# Aggregation des points 
#################################


yy$ID <-  1:nrow(yy)
yy$weight <-  NA
p <-  head(yy)[4,]
for(p in head(yy)){
  print(p)
  cat("###\n")
  list_index <-  st_is_within_distance(p, yy, 10)
  if(length(list_index) > 0){
    p$weight <-  length(list)
    em
  }
  print(neigh)
}






# on donne un poids de 1 à chaque point
yy$weight <- 1 

#rayon d'agregation en mètres
rayonAggreg  <-  10
sourcesPtsAgg <-  aggregate(yy,
                            by=yy,
                            FUN=sum,
                            join=function(x, y){
                              st_is_within_distance(x, y, dist = rayonAggreg)
                              }
                            )
             

sourcesPtsAgg
                


zz <- st_join(yy,yy, left=F, join=function(x, y){st_is_within_distance(x, y, dist = rayonAggreg)})
zz



nrow(yy)
nrow(zz)
sourcesPtsAgg %>% class


plot(cells_to_fill$geometry, lwd=0.1)
plot(yy, add=T, cex=0.1, col="orange")
plot(sourcesPtsAgg, add=T, cex=0.1, col="purple")






st_write(sourcesPtsAgg, "../sourcesPietons_Aggregate10m.shp",layer = "sourcesPietonsAgg")














#---------------------------------
# Deuxième façon d'échantilloner les points
#   (chez moi ça fonctionne ^^)
#----------------------------------------
sourcesPietons <-  st_sample(cells_to_fill$geometry, size=cells_to_fill$nb_pietons, type=typesampling)


st_write(sourcesPietons, "../sourcesPietons.shp",layer = "sourcespietons")

















###############################
# Affecter les attributs des données aux points sources e.gt. densité de boutique, distances , tout ce qu'on veut 
########################################"



library(raster)
#intersection entre sources et rasters de densités
dens_boutiques <-  extract(boutiques_dens, sourcesPietons %>% as_Spatial())
dens_tramway <-  extract(tramway_dens, sourcesPietons %>% as_Spatial())
dens_restaurants <- extract(restaurants_dens, sourcesPietons %>% as_Spatial())



# affectation des attributs
sourcePietons$dens_boutiques <-  dens_boutiques
sourcePietons$dens_traway <-  dens_tramway
sourcePietons$dens_restaurants <-  dens_restaurants






