

# Run the following in bash before starting R
if [ -e $HOME/.Renviron ]; then cp $HOME/.Renviron $HOME/.Renviron.bkp; fi
if [ ! -d $HOME/.Rtmp ] ; then mkdir $HOME/.Rtmp; fi
echo "TMP='$HOME/.Rtmp'" > $HOME/.Renviron

module load gcc/4.9.2
module load proj.4/4.7
module load gdal/gcc/2.2.0
module load R/3.3.3
R

rm(list=ls())
#source('R:\\Mann Research\\IFPRI_Ethiopia_Drought_2016\\IFPRI_Ethiopia_Drought_Code\\ModisDownload.R')
#source('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/SummaryFunctions.R')
source('~/Documents/IFPRI_Ethiopia_Drought_2016/IFPRI_Ethiopia_Drought_2016/SummaryFunctions.R')
# R/3.0.2 seems to work maybe



library(data.table)
library(raster)
library(rgdal)
library(sp)
library(foreach)
library(doParallel)
library(compiler)
library(iterators)


# Compile Functions ---------------------------------------------------------------


functions_in = lsf.str()
lapply(1:length(functions_in), function(x){cmpfun(get(functions_in[[x]]))})  # byte code compile all functions http://adv-r.ha$




# Set up parameters -------------------------------------------------------


 # give path to Modis Reproduction Tool
  MRT = 'G:/Faculty/Mann/Projects/MRT/bin'

  # get list of all available modis products
  #GetProducts()

  # Product Filters
  products =  c('MYD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1' for 250m and landcover ='MCD12Q2'
  location = c(9.145000, 40.489673)  # Lat Lon of a location of interest within your tiles listed above #India c(-31.467934,-5$
  tiles =   c('h21v07','h22v07','h21v08','h22v08')   # India example c('h13v12')
  dates = c('2009-01-01','2017-03-6') # example c('year-month-day',year-month-day')

  version = 4 # updated land cover classes
  product ='NDVI'


#  DONE  Extract polygon or points data from stacks -------------------------------------

  library(data.table)
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/WO Clouds Clean LC/') # don't load smoothed...
  dir.create(file.path('../../Processed Panel/ExtractRaw/'), showWarnings=F,recursive=T) # create dir for tifs
  dir.create(file.path('/lustre/groups/manngroup/Processed Panel/ExtractRaw/'), showWarnings=F,recursive=T) # folder on high sp$


  # load data stacks from both directories
  rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one
  dir1 = list.files('.','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)

  # get polygon data
  #ogrInfo('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasUTM')
  #Polys = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasUTM')
  #Polys = spTransform(Polys, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
  #writeOGR(Polys, dsn="/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/",
  #     layer="EnumerationAreasSIN", driver="ESRI Shapefile") # this is in geographical projection
  Polys = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN',
        stringsAsFactors = F)
  Polys$id = 1:dim(Polys@data)[1]
  #head(Polys)
  #unique(Polys$R_NAME)


  # break into blocks of polygons
  block_width = 100
  nrows = dim(Polys)[1]
  nblocks <- nrows%/%block_width
  bs_rows <- seq(1,nblocks*block_width+1,block_width)
  bs_nrows <- rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
  print('Working on the following rows')
  print(paste(bs_rows))

  # use iterator package to move through rows
  inter_rows = lapply(1:length(bs_rows), function(x) seq(bs_rows[x],(bs_rows[x]+bs_nrows[x]-1)))

  # change location stating at for parallel
  start_loc = 14
  end_loc = length(inter_rows)
  inter_rows = inter_rows[start_loc:end_loc]

  # set up iterator 
  inter_rows = iter(inter_rows)

  product = c('NDVI','EVI')[1]

  out = foreach(rows= seq(1,inter_rows$length)) %do% {  
        # limit size of polys to avoid memory issues
        Polys_sub = Polys[nextElem(inter_rows),]
        # extract values croped to point or polygon
        Poly_Veg_Ext = extract_value_point_polygon(Polys_sub,
                list(get(paste(product,'_stack_h22v08_WO_Clouds_Clean_LC',sep='')),
                get(paste(product,'_stack_h22v07_WO_Clouds_Clean_LC',sep='')),
                get(paste(product,'_stack_h21v08_WO_Clouds_Clean_LC',sep='')),
                get(paste(product,'_stack_h21v07_WO_Clouds_Clean_LC',sep=''))),10)

        print(paste("saving block",start_loc))
        save(Poly_Veg_Ext ,
                 file = paste(
                    '/lustre/groups/manngroup/Processed Panel/ExtractRaw/',
                    start_loc,product,'_panel_','_ExtractRaw','.RData',sep='') )
        start_loc = start_loc + 1   # counter so that can start with later blocks
	rm(Poly_Veg_Ext)
        return(0)
  }

  # Copy files back from lustre and delete lustre
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/')
  flist = list.files("/lustre/groups/manngroup/Processed Panel/ExtractRaw/",
         glob2rx(paste('*',product,'*','.RData$',sep='')),full.names = T)
  fname = list.files("/lustre/groups/manngroup/Processed Panel/ExtractRaw/",
         glob2rx(paste('*',product,'.RData$',sep='')),full.names = F)
  file.copy(from=flist, to=paste(".",fname,sep='/'),
         overwrite = T, recursive = F, copy.mode = T)
  file.remove(flist)




  # DONE COMBINE DATA BACK FROM EXTRACT ----------------------------------------------------------------

  # Combine all extracted values into a single list 
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/')
  fname = list.files(".", glob2rx(paste('*','NDVI_panel__ExtractRaw.RData$',sep='')),full.names = F)
  nums = unlist(regmatches(fname, gregexpr("[0-9]+", fname)))
  fname = fname[order(as.numeric( nums))]  # put in order
  product = 'NDVI'

  holder =list()
  for(i in 1:length(fname)){
 	print(i)
   	load(paste('./',fname[i],sep=''))
	holder = c(holder,Poly_Veg_Ext)
	rm(Poly_Veg_Ext)
  }

  Poly_Veg_Ext = holder
  
  dir.create(file.path('../Processed Panel/ExtractRaw_Combined_AllEAs/'), showWarnings=F,recursive=T) # create dir for tifs
  save(Poly_Veg_Ext, file = paste('../Processed Panel/ExtractRaw_Combined_AllEAs/','AllEAs_',product,
										'_panel_ExtractRaw2.RData',sep='') )


  # DONE Summary Functions --------------------------------------------------------
 
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/')
  product = 'NDVI'
  load(paste('../Processed Panel/ExtractRaw_Combined_AllEAs/','AllEAs_',product,'_panel_ExtractRaw2.RData',sep='') )


  # Get planting and harvest dates
  plantharvest =   PlantHarvestDates(start_date=dates[1],end_date=dates[2],PlantingMonth=4,
                                   PlantingDay=1,HarvestMonth=1,HarvestDay=30)


  # Get summary statistics lists

  extr_values = Poly_Veg_Ext
  PlantHarvestTable = plantharvest
  Quant_percentile = 0.90
  num_workers = 1
  spline_spar = 0

  x = 1:length(extr_values)
  n = 1000
  blocks = split(x, sort(x%%n))

  NDVI_summary_all_EAs <- foreach(i = 1:length(blocks), inorder=T) %do% {
     print(i)
     Annual_Summary_Functions(extr_values[as.numeric(blocks[[i]])], PlantHarvestTable,Quant_percentile, aggregate=T,
                                         return_df=T,num_workers)  # make sure to convert block from integer to numeric for index
  }

  NDVI_summary_all_EAs = Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile, aggregate=T,
                                         return_df=T,num_workers)  # make sure to convert block from integer to numeric for index

  product = 'NDVI'
  save(NDVI_summary_all_EAs, file = paste('../Processed Panel/ExtractRaw_Combined_AllEAs/','AllEAs_',product,'_panel_summary2.RData',sep='') )







# DEAL WITH ALL OTHER DATA ######################################



# DONE pull data to polygons --------------------------------------------------------
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
  # load all EA data
  Polys_sub = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN',
                      stringsAsFactors = F)
  Polys_sub$id = 1:dim(Polys_sub@data)[1]

  # load other data
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
  dist_rcap = raster('./DistanceTransport/EucDist_Rcap_sin.tif')
  roadden = raster('./DistanceTransport/RoadDen_5km_WLRC_sin.tif')
  dist_pp50k = raster('./DistanceTransport/EucDist_pp50k_sin.tif')
  elevation = raster('./SRTM/srtm_90m_sin2.tif')
  soil_TAWC = raster('./SoilWaterCapacity/af_agg_ERZD_TAWCpF23mm__M_1km_sin.tif')

  for(layer in c('dist_rcap','roadden','dist_pp50k','elevation','soil_TAWC')){
	 print(paste('working on ',layer))
         values = extract_value_point_polygon(Polys_sub,get(layer),16)
	 save(values,file = paste('./Outputs/AllEas_values_',layer,'backup2.RData',sep=''))
	 load(paste('./Outputs/AllEas_values_',layer,'backup.RData',sep=''))
	 mean = do.call('rbind',lapply(values, function(x) if (!is.null(x)&is.data.frame(x)) colMeans((x), na.rm=TRUE) else NA ))
         Polys_sub[[layer]] = as.numeric(mean)
  }


 writeOGR(obj=Polys_sub, dsn="/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/",
         layer="AllEas_EnumerationAreasSIN_v4_wdata2", driver="ESRI Shapefile",overwrite=T)


 writeOGR(obj=Polys_sub, dsn="/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/EnumerationAreasSIN_v4_wdata",
         layer="AllEas_EnumerationAreasSIN_v4_wdata2", driver="GeoJSON",overwrite=T)



# pull ETA PET data to polygons FINISHED -----------------------------------
#  ##load data
#  version = 4
#  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')
#  ##pull to polygons
#  load(paste('./PET/PET_stack_V',version,'.RData',sep=''))
#  all_eas_PET = extract_value_point_polygon(Polys_sub,PET_stack,12)
#  save(all_eas_PET,
#  	 file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#  	 'AllEas_Poly_PET_Ext_V',version,'.RData',sep=''))
#  rm(list=c('Poly_PET_Ext_sub','PET_stack'))
#  
#
#  part1 = 1:30000
#  part2 = 30001:50000
#  part3 = 50001:length(Polys_sub)

  #load(paste('./ETa Anomaly/ETA_stack_V',version,'.RData',sep=''))
  #Poly_ETA_Ext_sub = extract_value_point_polygon(Polys_sub[part1,],ETA_stack,13)
  #save(Poly_ETA_Ext_sub,
  #	 file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
  #	 'AllEas_Poly_ETA_Ext_V',version,'part1.RData',sep=''))
  #rm(list=c('Poly_ETA_Ext_sub','ETA_stack'))
#
#  Poly_ETA_Ext_sub = extract_value_point_polygon(Polys_sub[part2,],ETA_stack,13)
#  save(Poly_ETA_Ext_sub,
#         file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_ETA_Ext_V',version,'part2.RData',sep=''))
#  rm(list=c('Poly_ETA_Ext_sub','ETA_stack'))
#
#  Poly_ETA_Ext_sub = extract_value_point_polygon(Polys_sub[part3,],ETA_stack,10)
#  save(Poly_ETA_Ext_sub,
#         file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_ETA_Ext_V',version,'part3.RData',sep=''))


  # combine back ETA parts
#  load(paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_ETA_Ext_V',version,'part1.RData',sep=''))
#  all_eas_ETA_part1 = Poly_ETA_Ext_sub
#  load(paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_ETA_Ext_V',version,'part2.RData',sep=''))
#  all_eas_ETA_part2 = Poly_ETA_Ext_sub
# load(paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_ETA_Ext_V',version,'part3.RData',sep=''))
#  all_eas_ETA_part3 = Poly_ETA_Ext_sub
#
#  all_eas_ETA = c(all_eas_ETA_part1,all_eas_ETA_part2,all_eas_ETA_part3)
#
#  save(all_eas_ETA,
#         file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_ETA_Ext_V',version,'.RData',sep=''))
#  rm(list=c('all_eas_ETA_part1','all_eas_ETA_part2','all_eas_ETA_part3'))



 ## DONE deal with CHIRPS PPT data downloaded and stacked w/ 1a-DownloadCHIRPSFTP_Rcurl.R
#  part1 = 1:30000
#  part2 = 30001:50000
#  part3 = 50001:length(Polys_sub)

#  load('./Data Stacks/Rain Stacks/Rain_Stack_h21v07_h21v08_h22v07_h22v08.RData')
#  Poly_PPT_Ext_sub = extract_value_point_polygon(Polys_sub[part1,],rain_stack,12)
#  save(Poly_PPT_Ext_sub,
#  	 file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#  	 'AllEas_Poly_PPT_Ext_V',version,'part1.RData',sep=''))
#  rm(list=c('Poly_PPT_Ext_sub','rain_stack'))

#  Poly_PPT_Ext_sub = extract_value_point_polygon(Polys_sub[part2,],rain_stack,12)
#  save(Poly_PPT_Ext_sub,
#         file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_PPT_Ext_V',version,'part2.RData',sep=''))
#  rm(list=c('Poly_PPT_Ext_sub'))

#  Poly_PPT_Ext_sub = extract_value_point_polygon(Polys_sub[part3,],rain_stack,12)
#  save(Poly_PPT_Ext_sub,
#         file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_PPT_Ext_V',version,'part3.RData',sep=''))
#  rm(list=c('Poly_PPT_Ext_sub'))



  # DONE combine back PPT parts
#  load(paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_PPT_Ext_V',version,'part1.RData',sep=''))
#  all_eas_PPT_part1 = Poly_PPT_Ext_sub
#  load(paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_PPT_Ext_V',version,'part2.RData',sep=''))
#  all_eas_PPT_part2 = Poly_PPT_Ext_sub
# load(paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_PPT_Ext_V',version,'part3.RData',sep=''))
#  all_eas_PPT_part3 = Poly_PPT_Ext_sub
#
#  all_eas_PPT = c(all_eas_PPT_part1,all_eas_PPT_part2,all_eas_PPT_part3)
#
#  save(all_eas_PPT,
#         file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/',
#         'AllEas_Poly_PPT_Ext_V',version,'.RData',sep=''))
#  rm(list=c('all_eas_PPT_part1','all_eas_PPT_part2','all_eas_PPT_part3'))



 # DONE  reload and summarize by NDVI timing
  setwd("/home/mmann1123/Documents/IFPRI_Ethiopia_Drought_2016/Data")
  load(paste('./Outputs/', 'AllEas_Poly_PPT_Ext_V',version,'.RData',sep=''))
  load(paste('./Outputs/','AllEas_Poly_ETA_Ext_V',version,'.RData',sep=''))
  load(paste('./Outputs/','AllEas_Poly_PET_Ext_V',version,'.RData',sep=''))

  load(paste('./Processed Panel/Processed Panel/ExtractRaw_Combined_AllEAs/'
	,'AllEAs_',product,'_panel_summary2.RData',sep='') )
  load('./Data Stacks/WO Clouds Clean LC/NDVI_stack_h21v07_WO_Clouds_Clean_LC_V4.RData') # used for spline dates



 # Get summary statistics lists using plant harvest dates obtained from NDVI
  Veg_Annual_Summary = NDVI_summary_all_EAs
  Veg_Stack = NDVI_stack_h21v07_WO_Clouds_Clean_LC # used for spline dates
  name_prefix = 'PET'
  extr_values = get(paste('all_eas_',name_prefix,sep=''))

  Quant_percentile=0.90
  num_workers = 10
  spline_spar=.05 # good = 0.05
  aggregate=T
  return_df=T
  all_eas_PET_summary =  Annual_Summary_Functions_OtherData(extr_values, PlantHarvestTable,Veg_Stack,Veg_Annual_Summary,name_prefix,
                                                  Quant_percentile,return_df,num_workers,aggregate,spline_spar)
  save(all_eas_PET_summary, file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/all_eas_PET_summary_V',version,'.Rdata',sep=''))

  name_prefix = 'ETA'
  extr_values = get(paste('all_eas_',name_prefix,sep=''))
  all_eas_ETA_summary =  Annual_Summary_Functions_OtherData(extr_values, PlantHarvestTable,Veg_Stack,Veg_Annual_Summary,name_prefix,
                                                  Quant_percentile,return_df,num_workers,aggregate,spline_spar)
  save(all_eas_ETA_summary, file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/all_eas_ETA_summary_V',version,'.Rdata',sep=''))


  name_prefix = 'PPT'
  extr_values = get(paste('all_eas_',name_prefix,sep=''))
  all_eas_PPT_summary =  Annual_Summary_Functions_OtherData(extr_values, PlantHarvestTable, Veg_Stack,Veg_Annual_Summary,name_prefix,
                                                  Quant_percentile,return_df,num_workers,aggregate,spline_spar)
  save(all_eas_PPT_summary, file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Outputs/all_eas_PPT_summary_V',version,'.Rdata',sep=''))






# Convert data to panel format --------------------------------------------
  version = 4
  product = 'NDVI'
  # Load data
  setwd('./Documents/IFPRI_Ethiopia_Drought_2016/Data/')
  load(paste('./Outputs/all_eas_PET_summary_V',version,'.Rdata',sep=''))
  load(paste('./Outputs/all_eas_ETA_summary_V',version,'.Rdata',sep=''))
  load(paste('./Outputs/all_eas_PPT_summary_V',version,'.Rdata',sep=''))
  load(paste('./Processed Panel/Processed Panel/ExtractRaw_Combined_AllEAs/'
        ,'AllEAs_',product,'_panel_summary2.RData',sep=''))
  all_eas_Polys = readOGR('./EnumerationAreas/','EnumerationAreasSIN', stringsAsFactors = F)


  # calculate area of eas in hectares
  all_eas_Polys$Area_Ha = sapply(all_eas_Polys@polygons, function(x) x@Polygons[[1]]@area*0.0001)
  all_eas_Polys_data = all_eas_Polys@data
  all_eas_Polys_data$i = 1:dim(all_eas_Polys_data)[1]  # add 'i' to join to

  # define years to include
  year_start = 2009
  year_end   = 2016

  holder_list = list()
  for(i in 1:dim(all_eas_Polys)[1]){
        print(paste('working on row ', i))
        holder_summary = data.frame(row = seq(year_start,year_end),i=i)
        holder_summary = join(holder_summary,all_eas_Polys_data[i,!(names(all_eas_Polys_data) %in% c('Remark','UK_NAME','UK_CODE',
                'EA_ID','UK_ID','EA_CODE','W_cod_t','KK_cd_T','KK_NAME','KK_CODE'))],by=c('i'),type='left')
        # join NDVI data
        if(length(NDVI_summary_all_EAs[[i]])!=1 ){  # avoid missing values
                holder_summary = join(holder_summary,NDVI_summary_all_EAs[[i]],by=c('i','row'),type='left')} # join shp to vegetation data
        # join PET data
        if(length(all_eas_PET_summary[[i]])!=1 & class(all_eas_PET_summary[[i]])[1]!='simpleError'){
        holder_summary = join(holder_summary,all_eas_PET_summary[[i]][,!(names(all_eas_PET_summary[[i]]) %in% c('PET_plant_dates',
                'PET_harvest_dates','PET_A_max_Qnt',
                'PET_A_AUC_Qnt','PET_G_mx_dates','PET_G_mx_Qnt','PET_G_AUC_Qnt','PET_T_G_Qnt'))],by=c('i','row'),type='left')}
        # join ETA data
        if(length(all_eas_ETA_summary[[i]])!=1){
        holder_summary = join(holder_summary, all_eas_ETA_summary[[i]][,!(names(all_eas_ETA_summary[[i]]) %in% c('ETA_plant_dates',
                'ETA_harvest_dates','ETA_A_max_Qnt',
                'ETA_A_AUC_Qnt','ETA_G_mx_dates','ETA_G_mx_Qnt','ETA_G_AUC_Qnt','ETA_T_G_Qnt'))],by=c('i','row'),type='left')}
        # join PPT data
        if(length(all_eas_PPT_summary[[i]])!=1 & class(all_eas_PPT_summary[[i]])[1] =='data.frame'){
        holder_summary = join(holder_summary,
            	all_eas_PPT_summary[[i]][,!(names(all_eas_PPT_summary[[i]]) %in% c('PPT_plant_dates','PPT_harvest_dates','PPT_A_min',
		'PPT_A_AUC','PPT_A_Qnt', 'PPT_A_max_Qnt','PPT_A_AUC_Qnt','PPT_G_min','PPT_G_AUC PPT_G_Qnt',
		' PPT_G_mx_Qnt'))],by=c('i','row'),type='left')  }
        # rename to year and move to second column
        names(holder_summary)[names(holder_summary)=='row']='Year'
        holder_summary = arrange.vars(holder_summary, c("Year"=2))
        holder_list[[i]] = holder_summary
  }

  library(data.table)
  output = rbindlist(holder_list, fill=T)

  write.csv(output,paste('./Outputs/all_eas_NDVI_ET_panel_V',version,'.csv',sep=''))
  save(output,file=paste('../IFPRI_Ethiopia_Drought_2016/Outputs4Pred/all_eas_NDVI_ET_panel_V',version,'.RData',sep=''))
  library(readstata13)
  save.dta13(output,file=paste('../IFPRI_Ethiopia_Drought_2016/Outputs4Pred/all_eas_NDVI_ET_panel_V',version,'.dta',sep=''))

