

# 
# # Run the following in bash before starting R
# if [ -e $HOME/.Renviron ]; then cp $HOME/.Renviron $HOME/.Renviron.bkp; fi
# if [ ! -d $HOME/.Rtmp ] ; then mkdir $HOME/.Rtmp; fi
# echo "TMP='$HOME/.Rtmp'" > $HOME/.Renviron
# 
# module load proj.4/4.8.0
# module load gdal/gcc/1.11 
# module load R/3.3.3
# module load gcc/4.9.0
# R


rm(list=ls())

library(raster)
library(sp)
library(maptools)
library(ggplot2)
library(readstata13)
library(reshape)
library(rgdal)
library(grid)
library(gridExtra)
library(plotly)

# Set up parameters -------------------------------------------------------
 


# Visualize stacked histograms of damage by type -------------------------------------

setwd('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/IFPRI_Ethiopia_Drought_Code/Outputs4Pred/')
agss = read.dta13('./AgSS_2010_15_Compiled_panel_merged_clean_PCA_v3.dta') 

agss= agss[agss$REGIONCODE != 2,] # Drop afar

#WHEATDAMAGE_WEATHER_AREA_P,
damage = aggregate( cbind(WHEATDAMAGE_PESTS_AREA_P,WHEATDAMAGE_MANAGE_AREA_P,WHEATDAMAGE_OTHER_AREA_P,WHEATDAMAGE_DROUGHT_AREA_P) 
                    ~ REGIONCODE+Year, FUN = sum ,data=agss)
 
names(damage) = c('Region','Year','Pests','Manage','Other','Drought')
damage <- melt(damage, id=c("Region","Year"))
damage$regionyear = paste(damage$Region,damage$Year,sep='')
damage$Region[damage$Region==1]='Tigray'
damage$Region[damage$Region==3]='Amhara'
damage$Region[damage$Region==2]='Afar'
damage$Region[damage$Region==4]='Oromia'
damage$Region[damage$Region==5]='Somali'
damage$Region[damage$Region==6]='Benishangul'
damage$Region[damage$Region==7]='SNNP'

damage$Region = factor(damage$Region)
damage$Region = ordered(damage$Region, levels = c("Afar", "Benishangul", "Tigray",'SNNP','Amhara','Oromia'))
names(damage)[names(damage)=='variable'] = 'Type'

#ggplot(data=damage,aes(x = Year, y = value, colour=Region, fill=variable, group=regionyear)) +geom_bar(stat = "identity",position="dodge")


A = ggplot(data=damage,aes(x = Year, y = value,  fill=Type, group=Region)) +
  geom_bar(stat = "identity",position="stack",aes(alpha=0.8))+ scale_alpha(guide = 'none')+ 
  scale_fill_brewer(palette = "Set1",direction = -1)+
  facet_grid(.~(Region))+ylab('% of Planted Area Damaged')+ theme(axis.text.x = element_text(angle = 90))
plot(A)


# Visualize crop area by region -------------------------------------

  area = aggregate( cbind(WHEATAREA,MAIZEAREA,BARLEYAREA,SORGHUMAREA,TEFFAREA) 
                    ~ REGIONCODE+Year, FUN = sum ,data=agss)
  
  names(area) = c('Region','Year','Wheat','Maize','Barley','Sorghum','Teff')
  area = melt(area, id=c("Region","Year"))
  area$regionyear = paste(area$Region,damage$Year,sep='')
  area$Region[area$Region==1]='Tigray'
  area$Region[area$Region==3]='Amhara'
  area$Region[area$Region==2]='Afar'
  area$Region[area$Region==4]='Oromia'
  area$Region[area$Region==5]='Somali'
  area$Region[area$Region==6]='Benishangul'
  area$Region[area$Region==7]='SNNP'
  names(area)[names(area)=='variable'] = 'Crop'
  
  area$Region = ordered(area$Region, levels = c("Afar", "Benishangul", "Tigray",'SNNP','Amhara','Oromia'))
  area$value = area$value / 100  #scale
  
  B = ggplot(data=area,aes(x = Year, y = value,  fill=Crop, group=Region)) +
    geom_bar(stat = "identity",position="stack",alpha=0.8 )+ scale_alpha(guide = 'none')+facet_grid(.~(Region))+ylab('Hectares of Crop (100s)')+
    theme(axis.text.x = element_text(angle = 90))
  
  plot(B)


#  find proportion of total area for each crop by year  ------------------
  total_area = aggregate(value~Year, data = area,FUN=sum)
  names(total_area)[names(total_area)=='value']='Total_Area'
  crop_year_area = aggregate(value~Crop+Year, data = area,FUN=sum)
  names(crop_year_area)[names(crop_year_area)=='value']='crop_year_area'
  area2 = join(crop_year_area,total_area,by ='Year')
  area2$PerCropYear = area2$crop_year_area / area2$Total_Area *100
  
  
  B2= ggplot(data=area2,aes(x = Year, y = PerCropYear,  colour=Crop,alpha=0.8)) +
    geom_line( size=2 ) +ylab('% of Area Planted')+ scale_alpha(guide = 'none')+
    theme(axis.text.x = element_text(angle = 90))
  B2


#  find proportion of total area for each crop by year and Region -------------------
  Total_Area_Region = aggregate(value~Year+Region, data = area,FUN=sum)
  names(Total_Area_Region)[names(Total_Area_Region)=='value']='Total_Area_Region'
  
  
  crop_year_area_region = aggregate(value~Crop+Year+Region, data = area,FUN=sum)
  names(crop_year_area_region)[names(crop_year_area_region)=='value']='crop_year_area_region'
  area3 = join(Total_Area_Region,crop_year_area_region,by =c('Year','Region'))
  area3$PerCropYear = area3$crop_year_area_region / area3$Total_Area_Region *100
  
  
  B3 =  ggplot(data=area3,aes(x = Year, y = PerCropYear,  colour=Crop,alpha=0.8)) +
    geom_line( size=2 )+ scale_alpha(guide = 'none') + 
    ylab('% of Area Planted')+facet_grid(.~Region)+ theme(axis.text.x = element_text(angle = 90))

  
  
# Visualize crop area by region -------------------------------------


#WHEATDAMAGE_WEATHER_AREA_P,
oph = aggregate( cbind(WHEATOPH_W,MAIZEOPH_W,BARLEYOPH_W,SORGHUMOPH_W,TEFFOPH_W) 
                  ~ REGIONCODE+Year, FUN = median ,data=agss)

names(oph) = c('Region','Year','Wheat','Maize','Barley','Sorghum','Teff')
oph = melt(oph, id=c("Region","Year"))
oph$regionyear = paste(oph$Region,damage$Year,sep='')
oph$Region[oph$Region==1]='Tigray'
oph$Region[oph$Region==3]='Amhara'
oph$Region[oph$Region==2]='Afar'
oph$Region[oph$Region==4]='Oromia'
oph$Region[oph$Region==5]='Somali'
oph$Region[oph$Region==6]='Benishangul'
oph$Region[oph$Region==7]='SNNP'
names(oph)[names(oph)=='variable'] = 'Crop'

oph$Region = ordered(oph$Region, levels = c("Afar", "Benishangul", "Tigray",'SNNP','Amhara','Oromia'))
 
C=ggplot(data=oph,aes(x = Year, y = value,  fill=Crop, group=Region,alpha=0.8)) +
  geom_bar(stat = "identity",position="stack" )+ scale_alpha(guide = 'none')+
  facet_grid(Crop~Region)+ylab('Median Output Per Hectare')+ 
  scale_y_continuous(breaks=c(10, 20,30, 40))+ 
  theme(axis.text.x = element_text(angle = 90))

plot(C)

pdf("../Visualizations/DroughtIn1Graph.pdf", width = 8, height = 11) # Open a new pdf file
grid.arrange(B,A,C,ncol=1,heights=c(.25,.25,.50))
dev.off()

grid.arrange(A,B2,C,ncol=1,heights=c(.25,.25,.50))

pdf("../Visualizations/DroughtIn1Graph_v2.pdf", width = 8, height = 11) # Open a new pdf file
grid.arrange(A,B3,C,ncol=1,heights=c(.25,.25,.50))
dev.off()



# # OPH point graph 
# 
# oph2 = agss[,c("REGIONCODE","Year",'WHEATOPH_W','MAIZEOPH_W','BARLEYOPH_W','SORGHUMOPH_W','TEFFOPH_W')]
# names(oph2) = c('Region','Year','Wheat','Maize','Barley','Sorghum','Teff')
# 
# oph2 = melt(oph2, id=c("Region","Year"))
# oph2$regionyear = paste(oph2$Region,damage$Year,sep='')
# oph2$Region[oph2$Region==1]='Tigray'
# oph2$Region[oph2$Region==3]='Amhara'
# oph2$Region[oph2$Region==2]='Afar'
# oph2$Region[oph2$Region==4]='Oromia'
# oph2$Region[oph2$Region==5]='Somali'
# oph2$Region[oph2$Region==6]='Benishangul'
# oph2$Region[oph2$Region==7]='SNNP'
# names(oph2)[names(oph2)=='variable'] = 'Crop'
# 
# oph2$Region = ordered(oph2$Region, levels = c("Afar", "Benishangul", "Tigray",'SNNP','Amhara','Oromia'))
# oph2 = na.omit(oph2)
# 
# ggplot(data=oph2,aes(x = Year, y = value,  fill=Crop, group=Region)) + geom_boxplot()+ 
#   facet_grid(Crop~Region)+ylab('Output Per Hectare') 
# 
#   
# 
# 
# 
#  


# look at https://cran.r-project.org/web/packages/cowplot/vignettes/plot_grid.html




# 3 D Plots  --------------------------------------------------------------


#   -------------------------------------

setwd('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/IFPRI_Ethiopia_Drought_Code/Outputs4Pred/')
agss = read.dta13('./AgSS_2010_15_Compiled_panel_merged_clean_PCA_v3.dta') 

agss= agss[agss$REGIONCODE != 2,] # Drop afar

agss2 = agss[,c('A_PC1_v2','G_PC1_v2','WHEATEXTAREA_P','WHEATDAMAGEAREA_P','WHEATOPH_W','WHEATDAMAGE_WEATHER_AREA_P','REGIONCODE')]
agss2 = na.omit(agss2)
agss2$REGIONCODE = factor(agss2$REGIONCODE) 

 
p <- plot_ly(agss2, x = ~WHEATEXTAREA_P, y = ~WHEATDAMAGE_WEATHER_AREA_P, z = ~WHEATOPH_W, color = ~REGIONCODE ) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'WHEATEXTAREA_P'),
                      yaxis = list(title = 'WHEATDAMAGE_WEATHER_AREA_P'),
                      zaxis = list(title = 'WHEATOPH_W')))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="basic2")
chart_link
 




# Spatial Plots -----------------------------------------------------------
library(plyr)

setwd('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/IFPRI_Ethiopia_Drought_Code/Outputs4Pred/')
agss = read.dta13('./AgSS_2010_15_Compiled_panel_merged_clean_PCA_v3.dta') 

setwd('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/Data/AdminBoundaries/')
eas = readOGR(dsn=".", layer="ETH_adm3_UTM")
eas@data$id = rownames(eas@data)
eas.points = fortify(eas, region="id")
eas.df = join(eas.points, eas@data, by="id")
  
eas@data$id = rownames(eas@data)
eas.points = fortify(eas, region="id")
eas.df = join(eas.points, eas@data, by="id")
eas.df$WOREDACODE = eas.df$ID_3  # rename for join
eas.df$MYID = eas.df$ID_3  # rename for join

# ggplot(eas.df) + 
#   aes(long,lat,group=id,fill=NULL) + 
#   geom_polygon() +
#   geom_path(color="white") +
#   coord_equal() +
#   scale_fill_brewer("eas Ecoregion")


agss_ag = aggregate(cbind(WHEATEXTAREA_P,WHEATIRRGAREA_P,WHEATDAMAGE_DROUGHT_AREA_P)~Year+WOREDACODE,data=agss,
                      FUN=function(x){median(x,na.rm=T)})
agss_ag$MYID = as.integer(agss_ag$WOREDACODE)
agss_ag$WOREDACODE = as.integer(agss_ag$WOREDACODE)
agss_ag_2015 = agss_ag[agss_ag$Year==2015,]

eas.df = join(eas.df,agss_ag_2015,by='MYID')

summary(eas.df$WHEATDAMAGE_DROUGHT_AREA_P)



ggplot(eas.df) + 
  aes(long,lat,group=id,fill=WHEATEXTAREA_P) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal()


  scale_fill_brewer("eas Ecoregion")


