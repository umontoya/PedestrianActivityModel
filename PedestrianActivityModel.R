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
setwd("~/LASSO/PedestrianActivityModel/spatial_sample_sources_pietons/ExportsCalcDensite/")



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

# vectorisation du raster mergé
mailles_density_vec_sf <-  rasterToPolygons(merged_rasters, dissolve = F)
mailles_density_vec_sf <- st_as_sf(mailles_density_vec_sf)

# sauvegarde en fichier si besoin
#st_write(Pietons_vec_sF, "pietons_dens_vec.geojson",delete_dsn = T)
# reprojection lambert 93

# A verifier , c'est sans doute faux
mailles_density_vec_sf <- st_transform(mailles_density_vec_sf, 2154)

#cellules dans zone marchable
ZonesMarchables <- st_intersection(zonepieton, mailles_density_vec_sf)
names(ZonesMarchables) <-  c("EXPLOD_ID" ,"density",   "geometry")

plot(ZonesMarchables[,"density"])


##################################################
# Filtrage des cellules trop fines et trop petites
###############################################"


# Filtrage cellules trop petites 

# on définit un objet en unités "metres carrés"  avec une valeur , ici 1 
library(units)
area_threshold <-  as_units(1,"m^2")

 # on applique ce seuil avec un filtre 
ZonesMarchables <- ZonesMarchables %>%  filter(st_area(.) > area_threshold) 


#librairie lwgeom pour le calcul du perimetre
library(lwgeom)
# calcul de la compacité (non noramlisée)
ZonesMarchables$compacity <-  st_area(ZonesMarchables)/st_perimeter(ZonesMarchables)

#seuil
compacity_threshold <-  as_units(0.2, "m")
#affichage pour voir l'aspect des cellules 
une_zone_fine <- ZonesMarchables %>%  filter(compacity < compacity_threshold) 
plot(une_zone_fine[, "compacity"])

#filtrage des cellules au dessus du seuil de compacité
ZonesMarchables <-  ZonesMarchables %>%  filter(compacity > compacity_threshold)


plot(ZonesMarchables[,"density"])
plot(ZonesMarchables[,"densityNormalized"])



##################################################"
# affectation du nombre de piétons par cellules
##################################################"


#: nombre maximal de piétons par cellule 
# a ajuster à la amin en fonction de la taille d'une cellule , 
#avec la ligne ci_dessous
#st_area(ZonesMarchables) %>%  max

MAX_PIETONS <- 100 


ZonesMarchables$nb_pietons <- ZonesMarchables$density * st_area(ZonesMarchables)


# on renormalise entre 0 et 1  
ZonesMarchables$nb_pietons <-  (ZonesMarchables$nb_pietons - min(ZonesMarchables$nb_pietons))/(max(ZonesMarchables$nb_pietons) -min(ZonesMarchables$nb_pietons))

# on convertit en nombre entre 0 et MAX_PIETONS
ZonesMarchables$nb_pietons <- ZonesMarchables$nb_pietons * MAX_PIETONS 

ZonesMarchables$nb_pietons <- round(ZonesMarchables$nb_pietons) %>%  as.numeric()
plot(ZonesMarchables["nb_pietons"])


# pour vérifier visuellement que la densité n'a pas trop changé 
#ZonesMarchables$densityrecacl <-  ZonesMarchables$nb_pietons / st_area(ZonesMarchables)
#plot(ZonesMarchables["densityrecacl"])




######################################
# ECHANTILLONAGE SPATIAL DES POINTS
# Première façon avec une boucle
######################################

# filtrer les mailles trop petites
# il n'est pas réaliste de mettre  1 personne dans moins d'un  mètres carré !
# ? discuter entre nous
# filtrage des mailles de moins d'un metre carré
cells_to_fill <- ZonesMarchables
# pas la peine de remplir les mailles avec une densit? nulle
cells_to_fill <- cells_to_fill[cells_to_fill$nb_pietons > 0 ,]
plot(cells_to_fill["nb_pietons"])
sourcesPietons <-  list()
for (i in 1:nrow(cells_to_fill)){
  c <-cells_to_fill$geometry[i]
  n <- cells_to_fill$nb_pietons[i]
  cat("maille", i,": ", n, "points dans" , st_area(c), "m^2\n")
  pts <- st_sample(c,n, type="regular") %>% st_sf()
  if(nrow(pts)>0){
    sourcesPietons[[i]] <- pts
  }
}
sourcesPietons <- rbindlist(sourcesPietons)
sourcesPietons <- sourcesPietons %>% st_sf()

# attention le plot d?conne , mais l'ouverture dans Qgis confirme que c'est localis? dans la zone marchable
st_write(sourcesPietons, "I:/Documentos/5A/Stage Inge/DATA/GeoFabrik PaysLoire/ExportsCalcDensite/sourcesPietonsPaul2.shp",layer = "sourcespietons2")
#affichage simple
plot(cells_to_fill$geometry, lwd=0.1)
plot(sourcesPietons, add=T, cex=0.1, col="orange")
dev.off()

#---------------------------------
# Deuxième façon d'échantilloner les points
#   (chez moi ça fonctionne ^^)
#----------------------------------------
sourcesPietons <-  st_sample(cells_to_fill$geometry, size=cells_to_fill$nb_pietons, type="regular")
sourcesPietons <-  st_sf(sourcesPietons)
st_write(sourcesPietons, "I:/Documentos/5A/Stage Inge/DATA/GeoFabrik PaysLoire/ExportsCalcDensite/sourcesPietonsPaul.shp",layer = "sourcespietons")



##
#Troisieme façon
#Left join : TODO 
##






library(raster)
#intersection entre sources et rasters de densit?s
dens_boutiques <-  extract(boutiques_dens, sourcesPietons %>% as_Spatial())
dens_tramway <-  extract(tramway_dens, sourcesPietons %>% as_Spatial())
dens_restaurants <- extract(restaurants_dens, sourcesPietons %>% as_Spatial())
# affectation des attributs
sourcesPietons$dens_boutiques <-  dens_boutiques
sourcesPietons$dens_traway <-  dens_tramway
sourcesPietons$dens_restaurants <-  dens_restaurants




# ######################################################
# Snapping des  points qui ne sont pas dans les géométries
##########################################################


# on filtre les cellules sans piétons 
ZonesMarchablesNonVides <- ZonesMarchables %>% filter(nb_pietons > 0)
plot(ZonesMarchablesNonVides$geometry)
# On commence par tester si il existe des points en dehors des géométries des cellules



# liste les polygones dans lesquels chaque points des sourcesPietons est contenu 
list_poly_containing_srcPietons <-  st_within(sourcesPietons, ZonesMarchables) 

#pour compter combien de prédicats within sont juste, on prend la taille du résultat : si c'est 0 , le point n'est pas dans un polygone, si c'est 1 ou  plus , le point tombe dans un (ou plusieurs) polygones. ici on a pas de superposition, donc toutes les liste de polygones sont de taille 1 maximum 

liste_predicats <-  lengths(list_poly_containing_srcPietons) == 0

# quels sont les indices des points qui ne sont pas dans un polygone ? 
idx_pts_outside <- which(lengths(liste_predicats) == 0)


if(length(idx_pts_outside)==0){
  cat("aucun point en dehors de polygones")
}






#############################################
# Affectation des fichiers audios aux sources
##############################################""


# Code actuel

BDD_Info <-  read.csv("./../../BDD_Info.csv", sep = ";")
dt1 <- data.table(BDD_Info) 
dt2 <- data.table(sourcespietons_alt)
#left join random
dt1$nb_pietons <-BDD_Info$Nb.Pers
names(BDD_Info) <-  c("ID", "Origine", "Homme", "Femme" , "nb_pietons", "Enfants", "Sensation", "Ext.Int" , "Audio" , "Langue")
# dt3 <- dt1[dt2, on = .(nb_pietons),
#     {ri <- sample(.N, 1L)
#     .(Homme = Homme[ri], ID = ID[ri] )},by = .EACHI]
T4 <- dt2[, c("ID") := dt1[sample(.N)][.SD, on=.(nb_pietons), mult="first", .(x.ID)]]
st_write(T4, "I:/Documentos/5A/Stage Inge/DATA/GeoFabrik PaysLoire/ExportsCalcDensite/T4.shp",layer = "T4") #Export T4


Spectrum <-  read.csv("./../../Spectrums_500ms.csv", sep = ";")
T4_with_Spectrum <-  inner_join(T4, Spectrum, by=c("ID"  ="ID_File"))





# Proposition de réécriture 

BDD_Info <-  read.csv("./../../BDD_Info.csv", sep = ";")


# nombre personnes disponibles dans les fichiers de la BDD Audio
nb_pers_disponibles<-  BDD_Info$Nb.Pers %>% unique()


# Fonction qui choisit un ID de fichier Audio
AudioChooser <- function(source_nb_pers, nb_pers_disponibles , BDD_Info){ 
ifelse(source_nb_pers %in% nb_pers_disponibles,
       #si on trouve un fichier avec le bon nombre , on en tire un au hasard
       BDD_Info %>%  
         filter(Nb.Pers==source_nb_pers) %>% 
         dplyr::select(ID) %>%  
         slice_sample(n=1) %>%  
         unlist()
       ,
       # sinon , on met rien  
       NA)
}


# on applique avec un 'sapply'  la fonction AudioChooser aux nb.pietons de la couche des sourcesPietons (sourcespietons_alt)  pour créer un nouvel attribut : AudioFileID
ZonesMarchables$AudioFileID <-  sapply(ZonesMarchables$nb_pietons, AudioChooser, nb_pers_disponibles, BDD_Info )


ZonesMarchables %>% as.data.frame() %>% filter(nb_pietons==3) %>%  pull(AudioFileID) 

