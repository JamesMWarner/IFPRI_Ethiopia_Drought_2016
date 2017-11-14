
rm(list=ls())

#library(raster)
#library(sp)
library(maptools)
library(ggplot2)
library(readstata13)
library(rgeos)
#library(reshape)
library(rgdal)
#library(grid)
#library(gridExtra)
#library(plotly)
library(plyr)
library(spgwr)


# Polygon plots
setwd('./Documents/IFPRI_Ethiopia_Drought_2016/')
agss = read.dta13(paste("./IFPRI_Ethiopia_Drought_2016/Outputs4Pred/AgSS_2010_15_Compiled_panel_merged_clean_v4.dta",sep=''))
# coordinates(agss) = ~ x_coord + y_coord  # 
# writeOGR(agss[agss$Year==2015,],dsn="./Data/Outputs/agsstest.GeoJSON", layer="agss", driver="GeoJSON",overwrite=T)
#plot(agss)

eas = readOGR(dsn="./Data/AdminBoundaries/", layer="Ethiopia_adm4_UTM",stringsAsFactors = F)
eas@data$id = rownames(eas@data)
eas@data$RK_CODE = as.numeric(eas@data$RK_CODE)
# plot(eas)
# points(agss,add=T,color='red')
# eas_yx = coordinates(gCentroid(eas, byid = T))
# eas_yx <- cbind(eas_yx, eas@data) 
# coordinates(eas_yx) = ~x + y
# points(eas_yx,add=T, color='red')
    
head(sort(agss$RK_CODE))
head(sort(eas@data$RK_CODE[eas@data$RK_CODE!=0]))

# a =(agss$RK_NAME)[toupper(agss$RK_NAME) %in% toupper(eas@data$RK_NAME)]
#  
# agss$RK_CODE[toupper(agss$RK_NAME)==toupper('Ende Silase')]
# eas@data$RK_CODE[toupper(eas@data$RK_NAME)==toupper('Ende Silase')]

#numeric_variables = names(agss)[sapply(agss, is.numeric)]

agss$Agr_Eco = as.numeric(as.factor(agss$Agr_Eco)) #[1] "Dry Berha" 2"Dry Dega" 3"Dry Kolla"4"Dry Weyna Dega" 5Moist Dega"6Moist High Dega" 7Moist Kolla" [8] "Moist Weyna Dega" 9"Moist Wurch" 10"Wet Dega"         "Wet High Dega"    "Wet Kolla"        "Wet Weyna Dega"

agss_ag = aggregate(cbind(MAIZEOPH_W,MAIZEEXTAREA_P,MAIZESERRAREA_P,MAIZEMERR1AREA_P,MAIZEMERR2AREA_P,MAIZEMERR3AREA_P,
                          MAIZEMERR4AREA_P,MAIZEMERR5AREA_P,MAIZESEED1AREA_P,MAIZESEED2AREA_P,MAIZEIMSEED_P,MAIZENIMSEED_P,
                          MAIZEDAMAGEAREA_P,MAIZEFERT_NATURAL_AREA_P,MAIZEFERT_CHEMICAL_AREA_P,ZONECODE,elevation,Agr_Eco,
                          dist_rcap,roadden,dist_pp50k,soil_TAWC,G_mx,A_Qnt,PPT_G_AUC_Qnt,PPT_G_mn)~Year+RK_CODE,data=agss, 
                    FUN=function(x){median(x,na.rm = T)})

agss_ag_2015 = agss_ag[agss_ag$Year==2015,]

eas@data =   join(eas@data,agss_ag_2015,by='RK_CODE',type='left')
eas.agss.poly.maize = eas[!is.na(eas$MAIZEOPH_W),]
plot(eas.agss.poly.maize) 
eas.agss.yx.maize <- SpatialPointsDataFrame(gCentroid(eas.agss.poly.maize,byid = T), data= eas.agss.poly.maize@data) 

summary(eas.agss.yx.maize@data$MAIZEMERR4AREA_P)

# Export to KML  
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
eas.agss.yx.maize_wgs84<- spTransform(eas.agss.yx.maize, CRS= p4s)
writeOGR(eas.agss.yx.maize_wgs84, dsn="./eas.agss.yx.maize_wgs84.kml", layer= "sp_wgs84", driver="KML", dataset_options=c("NameField=name"),overwrite_layer = T)



plot(eas.agss.yx.maize)


# Maize GWR ---------------------------------------------------------------
form1 = MAIZEOPH_W ~ MAIZEEXTAREA_P+ MAIZESERRAREA_P+MAIZEMERR1AREA_P+MAIZEMERR2AREA_P+MAIZEMERR3AREA_P+MAIZEMERR4AREA_P+
  MAIZEMERR5AREA_P+MAIZENIMSEED_P+elevation+ dist_rcap+roadden+dist_pp50k+soil_TAWC+G_mx+A_Qnt+PPT_G_AUC_Qnt+PPT_G_mn
form1_rk = MAIZEOPH_W ~ MAIZEEXTAREA_P+ MAIZESERRAREA_P+MAIZEMERR1AREA_P+MAIZEMERR2AREA_P+MAIZEMERR3AREA_P+MAIZEMERR4AREA_P+
  MAIZEMERR5AREA_P+MAIZENIMSEED_P+elevation+ dist_rcap+roadden+dist_pp50k+soil_TAWC+G_mx+A_Qnt+PPT_G_AUC_Qnt+PPT_G_mn+RK_CODE
 
# check colinear 
cor(as.data.frame(scale(model.frame(form1,eas.agss.yx.maize@data),center = T,scale=T)),method="spearman")

#find spatial bandwidth# scale and center data

eas.agss.yx.maize@data = as.data.frame(scale(model.frame(form1,eas.agss.yx.maize@data),center = T,scale=T))


# robust GWR methods
library(GWmodel)
form1 = MAIZEOPH_W ~ MAIZEEXTAREA_P+ MAIZESERRAREA_P+MAIZENIMSEED_P+elevation+ dist_rcap+roadden+dist_pp50k+soil_TAWC+G_mx+A_Qnt+PPT_G_AUC_Qnt+PPT_G_mn
bw.gwr.1 = bw.gwr(form1, data = eas.agss.yx.maize, approach = "AICc", kernel = "bisquare", adaptive = T,)
bw.gwr.1   

# standard GWR
gwr.res = gwr.basic(form1, data = eas.agss.yx.maize, bw = bw.gwr.1,  kernel = "bisquare", adaptive = T, F123.test = T)
print(gwr.res)
#save(gwr.res, file='./IFPRI_Ethiopia_Drought_2016/Outputs4Pred/gwr.res.form1.RData')

# robust to outliers
gwr.rob = gwr.robust(MAIZEOPH_W ~ MAIZEEXTAREA_P+ MAIZESERRAREA_P+MAIZENIMSEED_P+elevation+ dist_rcap+roadden+dist_pp50k+soil_TAWC+G_mx+A_Qnt+PPT_G_AUC_Qnt+PPT_G_mn,
                     data = eas.agss.yx.maize, bw = bw.gwr.1,  kernel = "bisquare", adaptive = T, F123.test = T)
print(gwr.rob)
#save(gwr.rob, file='./IFPRI_Ethiopia_Drought_2016/Outputs4Pred/gwr.rob.form1.RData')

sp = gwr.rob$SDF



# convert to sf spatial data format (easier and faster)
library(sf)
sf <- st_as_sf(sp) 
names(sf)

ggplot() + geom_sf(data = sf, aes(colour=Local_R2)) +
  coord_sf() +
  ggtitle(paste("Local R2"))  

ggplot() + geom_sf(data = sf, aes(colour=MAIZEEXTAREA_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) 

ggplot() + geom_sf(data = sf, aes(colour=MAIZESERRAREA_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) 

ggplot() + geom_sf(data = sf, aes(colour=MAIZESERRAREA_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) 



# find kernel distance and form
GWRbandwidth <- gwr.sel(form1, data=eas.agss.yx.maize ,adapt=T) 
# fit model 
gwr.model = gwr(form1, data=eas.agss.yx.maize, adapt =  GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
gwr.model
# Create an object with the value of Quasi-global R2
globalR2 <- (1 - (gwr.model$results$rss/gwr.model$gTSS))
#get model results  https://gis.stackexchange.com/questions/241127/how-to-plot-output-from-gwr-in-r
sp = gwr.model$SDF

#sp$RK_CODE = model.frame(form1_rk,eas.agss.yx.maize@data)$RK_CODE
# sp_outliers = gwr.model$SDF[gwr.model$SDF$MAIZEMERR2AREA_P>1000,]
# sp_outliers

# Export to KML  
# p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# sp_wgs84<- spTransform(sp, CRS= p4s)
# sp_wgs84 = sp_wgs84[,c('MAIZEMERR2AREA_P')]
# writeOGR(sp_wgs84, dsn="./sp_wgs84.kml", layer= "sp_wgs84", driver="KML", dataset_options=c("NameField=name"),overwrite_layer = T)

# convert to sf spatial data format (easier and faster)
library(sf)
sf <- st_as_sf(sp) 

ggplot() + geom_sf(data = sf, aes(colour=localR2)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='localR2',subtitle = paste("Global R2:", round(globalR2, 2) ) ) 


ggplot() + geom_sf(data = sf, aes(colour=MAIZEEXTAREA_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='MAIZEEXTAREA_P Coefficient',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=MAIZENIMSEED_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='MAIZENIMSEED_P',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=MAIZESERRAREA_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='MAIZESERRAREA_P',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=MAIZEMERR1AREA_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='MAIZEMERR1AREA_P',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=MAIZEMERR2AREA_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='MAIZEMERR2AREA_P',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=MAIZEMERR3AREA_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='MAIZEMERR3AREA_P',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=MAIZEMERR4AREA_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='MAIZEMERR4AREA_P',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=MAIZEMERR5AREA_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='MAIZEMERR5AREA_P',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=MAIZENIMSEED_P)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='MAIZENIMSEED_P',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=elevation)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='elevation',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=dist_rcap)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='dist_rcap',subtitle =  'Localized Coefficient Estimates' ) 
ggplot() + geom_sf(data = sf, aes(colour=roadden)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='roadden',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=dist_pp50k)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='dist_pp50k',subtitle =  'Localized Coefficient Estimates' ) 

ggplot() + geom_sf(data = sf, aes(colour=soil_TAWC)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='soil_TAWC',subtitle =  'Localized Coefficient Estimates' ) 
ggplot() + geom_sf(data = sf, aes(colour=G_mx)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='G_mx',subtitle =  'Localized Coefficient Estimates' ) 
ggplot() + geom_sf(data = sf, aes(colour=A_Qnt)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='A_Qnt',subtitle =  'Localized Coefficient Estimates' ) 
ggplot() + geom_sf(data = sf, aes(colour=PPT_G_AUC_Qnt)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='PPT_G_AUC_Qnt',subtitle =  'Localized Coefficient Estimates' ) 
ggplot() + geom_sf(data = sf, aes(colour=PPT_G_mn)) +
  coord_sf() +
  ggtitle(paste("Local R2")) +
  labs(title='PPT_G_mn',subtitle =  'Localized Coefficient Estimates' ) 


#print the results of the model
gwr.model
results<-as.data.frame(gwr.model$SDF)
head(results)

#attach coefficients to original dataframe
LondonWards$coefUnauthAbsenceSchools11<-results$UnauthAbsenceSchools11
LondonWards$coefPctWithNoQual11<-results$PctWithNoQual11
LondonWards$coefCarsPerHH2011<-results$CarsPerHH2011

#read in the shapefile using the maptools function readShapePoly
boroughs <- readShapePoly("london_boroughs.shp")
#fortify for use in ggpplot2
boroughoutline <- fortify(boroughs, region="name")

#now plot the various GWR coefficients                       
gwr.point1<-ggplot(LondonWards, aes(x=x,y=y))+geom_point(aes(colour=LondonWards$coefUnauthAbsenceSchools11))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()



# other stuff -------------------------------------------------------------






eas = readOGR(dsn="./Data/EnumerationAreas/", layer="EnumerationAreasSIN_sub_agss_codes_wdata")
eas@data$id = rownames(eas@data)
eas.points = fortify(eas, region="id")
eas.df = join(eas.points, eas@data, by="id")




ggplot(eas.df) + 
  aes(long,lat,group=id,fill=dist_rcap,colour=dist_rcap) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal()

