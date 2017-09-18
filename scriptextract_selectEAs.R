

rm(list=ls())
#source('R:\\Mann Research\\IFPRI_Ethiopia_Drought_2016\\IFPRI_Ethiopia_Drought_Code\\ModisDownload.R')
source('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/SummaryFunctions.R')





library(raster)
library(rgdal)
library(sp)
library(foreach)
library(doParallel)
library(compiler)


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
  dates = c('2010-01-01','2016-03-30') # example c('year-month-day',year-month-day') c('2002-07-04','2016-02-02')
  ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/6/'    # allData/6/ for evi, /51/ for landcover
  # allData/51/ for landcover DOESn't WORK jUST PULL FROM FTP


  version = 3 # updated land cover classes

# Extract polygon or points data from stacks -------------------------------------

  library(data.table)
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/WO Clouds Clean LC/') # don't load smoothed...
  dir.create(file.path('../../Processed Panel/ExtractRaw/'), showWarnings=F,recursive=T) # create dir for tifs
  dir.create(file.path('/lustre/groups/manngroup/Processed Panel/ExtractRaw/'), showWarnings=F,recursive=T) # folder on high sp$


  # load data stacks from both directories
  rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one
  dir1 = list.files('.',pattern=paste('*',version,'.RData',sep=''),full.names=T)
  lapply(dir1, load,.GlobalEnv)

  # add population and transport 
  dist_rcap = raster('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/DistanceTransport/EucDist_Rcap_sin.tif')
  roadden = raster('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/DistanceTransport/RoadDen_5km_WLRC_sin.tif')
  dist_pp50k = raster('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/DistanceTransport/EucDist_pp50k_sin.tif')



# Extract data from subset of eas that have agss data --------------------------


  # this is a subset of data for which agss data contains relevant crop data


  Polys_sub = readOGR('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/EnumerationAreas/','EnumerationAreasSIN_sub_agss_codes',
                      stringsAsFactors = F)
  Polys_sub$id = 1:dim(Polys_sub@data)[1]
  product = c('NDVI','EVI')[1]

  Poly_Veg_Ext_sub = extract_value_point_polygon(Polys_sub,
                 list(get(paste(product,'_stack_h22v08_WO_Clouds_Clean_LC',sep='')),
                 get(paste(product,'_stack_h22v07_WO_Clouds_Clean_LC',sep='')),
                 get(paste(product,'_stack_h21v08_WO_Clouds_Clean_LC',sep='')),
                 get(paste(product,'_stack_h21v07_WO_Clouds_Clean_LC',sep='')),15)

  save(Poly_Veg_Ext_sub,
       file=paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/',
       product,'_Poly_Ext_sub_agss.RData',sep=''))




