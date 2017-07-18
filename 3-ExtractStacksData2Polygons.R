# the file takes stack outputs from 2 - Stack Files.R and extracts data to EA polygons

# Run the following in bash before starting R
if [ -e $HOME/.Renviron ]; then cp $HOME/.Renviron $HOME/.Renviron.bkp; fi
if [ ! -d $HOME/.Rtmp ] ; then mkdir $HOME/.Rtmp; fi
echo "TMP='$HOME/.Rtmp'" > $HOME/.Renviron

module load proj.4/4.8.0
module load gdal/gcc/1.11
module load R/3.1.1
module load gcc/4.9.0
R

rm(list=ls())
#source('R:\\Mann Research\\IFPRI_Ethiopia_Drought_2016\\IFPRI_Ethiopia_Drought_Code\\ModisDownload.R')
source('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/SummaryFunctions.R')
# R/3.0.2 seems to work maybe


library(raster)
library(rgdal)
library(sp)
library(maptools)
#library(rts)
library(gdalUtils)
library(foreach)
library(doParallel)
library(compiler)
library(plyr)
library(foreign)

#cl <- makeCluster(32)
#registerDoParallel(cl)


# Compile Functions ---------------------------------------------------------------


functions_in = lsf.str()
lapply(1:length(functions_in), function(x){cmpfun(get(functions_in[[x]]))})  # byte code compile all functions http://adv-r.had.co.nz/Prof$


# Set up parameters -------------------------------------------------------

# Product Filters 
products =  c('MYD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1' for 250m and landcover ='MCD12Q2'
location = c(9.145000, 40.489673)  # Lat Lon of a location of interest within your tiles listed above #India c(-31.467934,-57.101319)  #
tiles =   c('h21v07','h22v07','h21v08','h22v08')   # India example c('h13v12')
dates = c('2009-01-01','2017-03-6') # example c('year-month-day',year-month-day')

version = 4 # updated land cover classes



# Extract data from subset of eas that have agss data --------------------------


  # this is a subset of data for which agss data contains relevant crop data 

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/WO Clouds Clean LC/')

  # load data stacks from both directories
  rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one
  dir1 = list.files('.','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)


  Polys_sub = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN_sub_agss_codes',
                      stringsAsFactors = F)
  Polys_sub$id = 1:dim(Polys_sub@data)[1]
  products = c('NDVI','EVI')[2]

  for(product in products){
    Poly_Veg_Ext_sub = extract_value_point_polygon(Polys_sub,
                 list(get(paste(product,'_stack_h22v08_WO_Clouds_Clean_LC',sep='')),
                 get(paste(product,'_stack_h22v07_WO_Clouds_Clean_LC',sep='')),
                 get(paste(product,'_stack_h21v08_WO_Clouds_Clean_LC',sep='')),
                 get(paste(product,'_stack_h21v07_WO_Clouds_Clean_LC',sep=''))),15)

    save(Poly_Veg_Ext_sub,
       file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/',
       product,'_Poly_Ext_sub_agss_V',version,'.RData',sep=''))
  }



# prepare other data ---------------------------------------
   # reproject transport variables
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/DistanceTransport/')
  example = proj4string(raster('../LandUseClassifications/NDVI_stack_h21v07_smooth_lc_svm_mn.tif'))
  #dist_rcap = raster('EucDist_Rcap.tif')
  #dist_rcap = projectRaster(dist_rcap, crs= crs(example), filename = './EucDist_Rcap_sin.tif',overwrite=T)
  #roadden = raster('RoadDen_5km_WLRC.tif')
  #roadden = projectRaster(roadden,crs=crs( example), filename = './RoadDen_5km_WLRC_sin.tif',overwrite=T)
  #dist_pp50k = raster('EucDist_pp50k.tif')
  #dist_pp50k = projectRaster(dist_pp50k, crs=crs(example), filename = './EucDist_pp50k_sin.tif',overwrite=T)
  # pull most agroecological area of greatest area to EAs https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
  #agro_eco = readOGR('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/Data/WLRC_Data_Original','Agroecology',
  #                    stringsAsFactors = F)
  #agro_eco =  spTransform(agro_eco, CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_def'))
  #Polys_sub = readOGR('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/Data/EnumerationAreas','EnumerationAreasSIN_sub_agss_codes',
  #                    stringsAsFactors = F)
  #Polys_sub =  spTransform(Polys_sub, CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_def')) # correct minor difference between crss
  #agro_eco_Polys_sub = intersect(agro_eco,Polys_sub) # creates multiple features for each EA with the ag zone
  # # calculate area of union eas in hectares
  #agro_eco_Polys_sub$Area_Ha = sapply(agro_eco_Polys_sub@polygons, function(x) x@Polygons[[1]]@area*0.0001)
  #agro_eco_Polys_sub_max = ddply(agro_eco_Polys_sub@data[,c('EA_cd_m','Area_Ha','Terminolog')], .(EA_cd_m), function(x) x[which.max(x$Area_Ha),]) # find poly with the max area
  #names(agro_eco_Polys_sub_max)[3]='Agr_Eco'
  ## Join data bank
  #Polys_sub@data = join(Polys_sub@data, agro_eco_Polys_sub_max[,c('EA_cd_m','Agr_Eco')] , by='EA_cd_m' )
  ## add area of ea 
  #Polys_sub$Area_Ha = sapply(Polys_sub@polygons, function(x) x@Polygons[[1]]@area*0.0001)
  #
  #writeOGR(obj=Polys_sub, dsn="R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/Data/EnumerationAreas",
  #         layer="EnumerationAreasSIN_sub_agss_codes", driver="ESRI Shapefile",overwrite=T,layer_options = "RESIZE=YES")
  
        
   


  # deal with PET  (untar bil files and place into /PET/Unzip folder)
  # get extents
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')

  # summarize mean by month and project
  #for( year in sprintf('%02d',seq(9,17))){
  # 	for( month in sprintf('%02d',seq(1,12))){
  #		flist = list.files("./PET/Unzip/", glob2rx(paste('et',year,month,'*','.bil$',sep='')),full.names = T)
  #		print(flist)
  #		stk = stack(flist)
  #		mn_stk = mean(stk,na.rm=T)
  #		proj4string(mn_stk) = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  #	 	projectRaster(mn_stk, crs=CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'), 
  #			filename = paste('./PET/PET',year,month,'mn','sin.tif',sep='_'),overwrite=T)
  #	}
  #}

  #flist = list.files("./PET/", glob2rx(paste('*','.tif$',sep='')),full.names = T)
  #year = paste('20',gsub("^.*([0-9]{2})_([0-9]{2}).*$", "\\1",flist,perl = T),sep='')  # Strip dates
  #month = gsub("^.*([0-9]{2})_([0-9]{2}).*$", "\\2",flist,perl = T)  # Strip dates
  #flist_dates = format(strptime(paste(year,month,'01',sep='_'),'%Y_%m_%d'),'%Y%j')   
  #flist = flist[order(as.numeric(paste(year,month,sep='')))]  # file list in order
  #flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order
  #PET_stack = stack(flist)
  #names(PET_stack)=flist_dates
  #save(PET_stack,file = paste('./PET/PET_stack_V',version,'.RData',sep=''))



  # deal with ETa
  # get extent
  #cl <- makeCluster(25)
  #registerDoParallel(cl)

  #setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
  #example1 = (raster('./LandUseClassifications/NDVI_stack_h21v07_smooth_lc_svm_mn.tif'))

  # get files and dates 
  #flist = list.files("./ETa Anomaly/", glob2rx(paste('*','.zip$',sep='')),full.names = T)
  #foreach(i = 1:length(flist), .inorder=F) %dopar% {unzip(flist[i], exdir ='./ETa Anomaly/')}
  #flist = list.files("./ETa Anomaly/", glob2rx(paste('*','ET.tif$',sep='')),full.names = T)
  #flist_dates = paste(gsub("^.*ma([0-9]{4}).*$", "\\1",flist,perl = T),sep='')  # Strip dates
  #flist = flist[order(flist_dates)]  # file list in order
  #flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order
  #flist_dates = paste('20',substr(flist_dates,1,2),'-',substr(flist_dates,3,4),'-01',sep='')
  #flist_dates = format(strptime(flist_dates, '%Y-%m-%d'),'%Y%j')

  # reproject to sin
  #example2 = raster(flist[1])
  #example2 = projectRaster(example2, crs=CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'))

  #foreach(layer = 1:length(flist), .inorder=F, .errorhandling ='pass',.packages='raster') %dopar% {
  #	print(layer)
  #	layer_out = raster(flist[layer])
  #	if(proj4string(layer_out)!=proj4string(example1)){
  #		layer_out = projectRaster(layer_out, example2)
  #		writeRaster(layer_out,flist[layer],overwrite=T)
  # 	}
  #	return(0)
  #}  
  #endCluster()

  # check intersection?
  #for(layer in 1:length(flist)){ print(layer)
  #      print(raster(flist[layer]))
  #      print(proj4string(raster(flist[layer]))=='+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs')
  #      print(extent(raster(flist[layer]))== extent(raster(flist[1])))
  #}

  #ETA_stack = stack(flist)
  #names(ETA_stack)=flist_dates
  #save(ETA_stack,file = paste('./ETa Anomaly/ETA_stack_V',version,'.RData',sep=''))




  #deal with soil water holding capacity  http://www.isric.org/documents/document-type/isric-report-201502-root-zone-plant-available-water-holding-capacity-sub
 
  # reproject to sin
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
  #soil = raster('./SoilWaterCapacity/af_agg_ERZD_TAWCpF23mm__M_1km.tif')
  #soil = projectRaster(soil, crs=CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'))
  #writeRaster(soil,'./SoilWaterCapacity/af_agg_ERZD_TAWCpF23mm__M_1km_sin.tif')
  soil = raster('./SoilWaterCapacity/af_agg_ERZD_TAWCpF23mm__M_1km_sin.tif')

  





# pull data to polygons --------------------------------------------------------
  # load data
  Polys_sub = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN_sub_agss_codes',
                      stringsAsFactors = F)
  Polys_sub$id = 1:dim(Polys_sub@data)[1]

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
  dist_rcap = raster('./DistanceTransport/EucDist_Rcap_sin.tif')
  roadden = raster('./DistanceTransport/RoadDen_5km_WLRC_sin.tif')
  dist_pp50k = raster('./DistanceTransport/EucDist_pp50k_sin.tif')
  elevation = raster('./SRTM/srtm_90m_sin2.tif')
  soil_TAWC = raster('./SoilWaterCapacity/af_agg_ERZD_TAWCpF23mm__M_1km_sin.tif')
  

  for(layer in c('dist_rcap','roadden','dist_pp50k','elevation','soil_TAWC')){
         values = extract_value_point_polygon(Polys_sub,get(layer),16)
         mean = do.call('rbind',lapply(values, function(x) if (!is.null(x)) colMeans(x, na.rm=TRUE) else NA ))
         Polys_sub[[layer]] = as.numeric(mean)
  }

 writeOGR(obj=Polys_sub, dsn="/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/",
 	 layer="EnumerationAreasSIN_sub_agss_codes_wdata", driver="ESRI Shapefile",overwrite=T)



# pull ETA PET data to polygons -----------------------------------
  ##load data
  #setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
  #load(paste('./PET/PET_stack_V',version,'.RData',sep=''))
  #load(paste('./ETa Anomaly/ETA_stack_V',version,'.RData',sep=''))
  #load('./Data Stacks/Rain Stacks/Rain_Stack_h21v07_h21v08_h22v07_h22v08.RData')
  ##pull to polygons 
  #Poly_PET_Ext_sub = extract_value_point_polygon(Polys_sub,PET_stack,15)
  #save(Poly_PET_Ext_sub,
  #      file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
  #      'Poly_PET_Ext_sub_V',version,'.RData',sep=''))
  #Poly_ETA_Ext_sub = extract_value_point_polygon(Polys_sub,ETA_stack,15)
  #save(Poly_ETA_Ext_sub,
  #      file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
  #      'Poly_ETA_Ext_sub_V',version,'.RData',sep=''))
  ## deal with CHIRPS PPT data downloaded and stacked w/ 1a-DownloadCHIRPSFTP_Rcurl.R
  #Poly_PPT_Ext_sub = extract_value_point_polygon(Polys_sub,rain_stack,15)
  #save(Poly_PPT_Ext_sub,
  #      file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
  #      'Poly_PPT_Ext_sub_V',version,'.RData',sep=''))
#
  #fix date NO LONGER NEEDED, intrgrated into 1a-... .R
  #Poly_PPT_Ext_sub=  lapply(1:length(Poly_PPT_Ext_sub),function(x){
  #   	date = paste('X',format(strptime( 
##	   gsub("^.*X([0-9]{4}.[0-9]{2}.[0-9]{2}).*$", "\\1", names(Poly_PPT_Ext_sub[[x]])),'%Y.%m.%d'),
#	   '%Y%j'),sep='')  
#  	names(Poly_PPT_Ext_sub[[x]])=date
#        return(Poly_PPT_Ext_sub[[x]])
#	})
 #save(Poly_PPT_Ext_sub,
 #       file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
 #       'Poly_PPT_Ext_sub_V',version,'.RData',sep=''))



  #remove x2013001 from ETA because it is missing data
  #Poly_ETA_Ext_sub = lapply(1:length(Poly_ETA_Ext_sub), function(x){
#		Poly_ETA_Ext_sub[[x]][,!(names(Poly_ETA_Ext_sub[[x]]) %in% 'X2013001')]
#	}) 
#  save(Poly_ETA_Ext_sub,
#        file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#        'Poly_ETA_Ext_sub_V',version,'.RData',sep=''))


 # # reload and summarize by NDVI timing 
 # setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
 # load(paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
 #       'Poly_PET_Ext_sub_V',version,'.RData',sep=''))
 # load(paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
 #       'Poly_ETA_Ext_sub_V',version,'.RData',sep=''))
 # load(paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
 #       'Poly_PPT_Ext_sub_V',version,'.RData',sep=''))
 # load(paste('./Outputs/NDVI_summary_V',version,sep=''))  # needed for Annual_Summary_Functions_OtherData
 # load('./Data Stacks/WO Clouds Clean LC/NDVI_stack_h21v07_WO_Clouds_Clean_LC_V4.RData') # used for spline dates
#
#
#  # Get summary statistics lists using plant harvest dates obtained from NDVI
#  extr_values = Poly_PET_Ext_sub  
#  Veg_Annual_Summary = NDVI_summary 
#  Veg_Stack = NDVI_stack_h21v07_WO_Clouds_Clean_LC # used for spline dates
#  name_prefix = 'PET'
#  Quant_percentile=0.90
#  num_workers = 10
#  spline_spar=.05 # good = 0.05
#  aggregate=T
#  return_df=T
#  PET_summary =  Annual_Summary_Functions_OtherData(extr_values, PlantHarvestTable,Veg_Stack,Veg_Annual_Summary,name_prefix,
#                                                  Quant_percentile,return_df,num_workers,aggregate,spline_spar)
#
#  extr_values= Poly_ETA_Ext_sub
#  name_prefix = 'ETA'
#  ETA_summary =  Annual_Summary_Functions_OtherData(extr_values, PlantHarvestTable,Veg_Stack,Veg_Annual_Summary,name_prefix,
#                                                  Quant_percentile,return_df,num_workers,aggregate,spline_spar)
#  save(PET_summary, file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/PET_summary_V',version,'.Rdata',sep=''))
#  save(ETA_summary, file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/ETA_summary_V',version,'.Rdata',sep=''))
#
#  extr_values= Poly_PPT_Ext_sub
#  name_prefix = 'PPT'
#  PPT_summary =  Annual_Summary_Functions_OtherData(extr_values, PlantHarvestTable, Veg_Stack,Veg_Annual_Summary,name_prefix,
#                                                  Quant_percentile,return_df,num_workers,aggregate,spline_spar)
#  save(PPT_summary, file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/PPT_summary_V',version,'.Rdata',sep=''))



# Convert data to panel format --------------------------------------------

  # Load data
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
  load(paste('./Outputs/PET_summary_V',version,'.Rdata',sep=''))
  load(paste('./Outputs/ETA_summary_V',version,'.Rdata',sep=''))
  load(paste('./Outputs/PPT_summary_V',version,'.Rdata',sep=''))
  load(paste('./Outputs/NDVI_summary_V',version,sep=''))

  Polys_sub = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN_sub_agss_codes_wdata',
                    stringsAsFactors = F)
  # calculate area of eas in hectares
  Polys_sub$Area_Ha = sapply(Polys_sub@polygons, function(x) x@Polygons[[1]]@area*0.0001)
  Polys_sub_data = Polys_sub@data
  Polys_sub_data$i = 1:dim(Polys_sub_data)[1]  # add id to join to 

  # define years to include
  year_start = 2009
  year_end   = 2016

  holder_list = list()
  for(i in 1:dim(Polys_sub_data)[1]){
	print(paste('working on row ', i))
	holder_summary = data.frame(row = seq(year_start,year_end),i=i)
	holder_summary = join(holder_summary,Polys_sub_data[i,!(names(Polys_sub_data) %in% c('Remark','UK_NAME','UK_CODE',
                'EA_ID','UK_ID','EA_CODE','W_cod_t','KK_cd_T','KK_NAME','KK_CODE'))],by=c('i'),type='left')
        # join NDVI data
	if(length(NDVI_summary[[i]])!=1 ){  # avoid missing values 
   		holder_summary = join(holder_summary,NDVI_summary[[i]],by=c('i','row'),type='left')} # join shp to vegetation data 
        # join PET data
	if(length(PET_summary[[i]])!=1){
	holder_summary = join(holder_summary,PET_summary[[i]][,!(names(PET_summary[[i]]) %in% c('PET_plant_dates',
		'PET_harvest_dates','PET_A_max_Qnt',
                'PET_A_AUC_Qnt','PET_G_mx_dates','PET_G_mx_Qnt','PET_G_AUC_Qnt','PET_T_G_Qnt'))],by=c('i','row'),type='left')}
        # join ETA data
	if(length(ETA_summary[[i]])!=1){
	holder_summary = join(holder_summary, ETA_summary[[i]][,!(names(ETA_summary[[i]]) %in% c('ETA_plant_dates',
		'ETA_harvest_dates','ETA_A_max_Qnt',
                'ETA_A_AUC_Qnt','ETA_G_mx_dates','ETA_G_mx_Qnt','ETA_G_AUC_Qnt','ETA_T_G_Qnt'))],by=c('i','row'),type='left')} 
	# join PPT data
        if(length(PPT_summary[[i]])!=1 & class(PPT_summary[[i]])[1] =='data.frame'){
        holder_summary = join(holder_summary,
	    PPT_summary[[i]][,!(names(PPT_summary[[i]]) %in% c('PPT_plant_dates','PPT_harvest_dates','PPT_A_min','PPT_A_AUC','PPT_A_Qnt',
		 'PPT_A_max_Qnt','PPT_A_AUC_Qnt','PPT_G_min','PPT_G_AUC PPT_G_Qnt',' PPT_G_mx_Qnt'))],by=c('i','row'),type='left')  }
        # rename to year and move to second column
	names(holder_summary)[names(holder_summary)=='row']='Year'
	holder_summary = arrange.vars(holder_summary, c("Year"=2))
	holder_list[[i]] = holder_summary
  }
 
  library(data.table)
  output = rbindlist(holder_list, fill=T)

  write.csv(output,paste('./Outputs/EA_NDVI_ET_panel_V',version,'.csv',sep=''))
  write.csv(output,paste('../IFPRI_Ethiopia_Drought_2016/Outputs4Pred/EA_NDVI_ET_panel_V',version,'.csv',sep=''))
  write.dta(output,paste('../IFPRI_Ethiopia_Drought_2016/Outputs4Pred/EA_NDVI_ET_panel_V',version,'.dta',sep=''))

  








