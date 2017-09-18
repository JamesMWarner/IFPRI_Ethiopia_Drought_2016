

rm(list=ls())
#source('R:\\Mann Research\\IFPRI_Ethiopia_Drought_2016\\IFPRI_Ethiopia_Drought_Code\\ModisDownload.R')
source('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/SummaryFunctions.R')





library(raster)
library(rgdal)
library(sp)
library(foreach)
library(doParallel)
library(compiler)
library(data.table)

# Compile Functions ---------------------------------------------------------------


functions_in = lsf.str()
lapply(1:length(functions_in), function(x){cmpfun(get(functions_in[[x]]))})  # byte code compile all functions http://adv-r.ha$




# Set up parameters -------------------------------------------------------



# Product Filters
products =  c('MYD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1' for 250m and landcover ='MCD12Q2'
location = c(9.145000, 40.489673)  # Lat Lon of a location of interest within your tiles listed above #India c(-31.467934,-57.101319)  #
tiles =   c('h21v07','h22v07','h21v08','h22v08')   # India example c('h13v12')
dates = c('2010-01-01','2016-03-30') # example c('year-month-day',year-month-day') c('2002-07-04','2016-02-02')
ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/6/'    # allData/6/ for evi, /51/ for landcover
# allData/51/ for landcover DOESn't WORK jUST PULL FROM FTP
#strptime(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],products[1])),'%Y%j') # get list of all available dates for products[1]
#  out_dir = 'R:\\Mann_Research\\IFPRI_Ethiopia_Drought_2016\\Data\\VegetationIndex'
#  setwd(out_dir)

version = 3    # update to 'more' landcover classification that includes more training sites and settlement land class
               # version 3 has new landcover and 2010 data




# Limit stacks to common dates -------------------------------------------


 setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/')

 # load data stacks from both directories
 versiontolook = paste('_V',version,'.RData',sep='')
 stack_types_2_load =c('EVI','NDVI','pixel_reliability')
 dir1 = list.files('./Data Stacks/Raw Stacks/',versiontolook,full.names=T)
 lapply(dir1, load,.GlobalEnv)

 # limit stacks to common elements
 for(product in stack_types_2_load ){
   for( tile in tiles){
     # find dates that exist in all datasets for current tile
     all_dates = lapply(paste(stack_types_2_load,'stack',tile,sep='_'),function(x){names(get(x))})
     # restrict to common dates
     common_dates = Reduce(intersect, all_dates)
     # subset stacks for common dates
     assign(paste(product,'_stack_',tile,sep=''),subset( get(paste(product,'_stack_',tile,sep='')),
                                                         common_dates, drop=F) )
     print('raster depth all equal')
     print( all.equal(common_dates,names(get(paste(product,'_stack_',tile,sep=''))))   )
     print(dim(get(paste(product,'_stack_',tile,sep='')))[3])
   }}



# stack smoother -----------------------------------------------------
# this stack is used for land cover classification only (bc classifier can't have NA values)


setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Raw Stacks/') # don't load smoothed...

# load data stacks from both directories
dir1 = list.files('.',versiontolook,full.names=T)
lapply(dir1, load,.GlobalEnv)


for( i in ls(pattern = "NDVI_stack*")){
  print('##############################################################')
  dir.create(file.path(getwd(), i), showWarnings = FALSE)
  print(paste('Starting processing of:',i))
  stack_in = get(i)
  stack_name = i
  dates =   as.numeric(gsub("^.*X([0-9]{7}).*$", "\\1",names(stack_in),perl = T))  # Strip dates
  pred_dates = dates
  spline_spar=0.4  # 0.4 for RF
  workers = 5
  out_dir = '/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/'
  stack_smoother(stack_in,dates,pred_dates,spline_spar,workers,stack_name,version, out_dir)
}

for( i in ls(pattern = "EVI_stack*")){
  print('##############################################################')
  dir.create(file.path(getwd(), i), showWarnings = FALSE)
  print(paste('Starting processing of:',i))
  stack_in = get(i)
  stack_name = i
  dates =   as.numeric(gsub("^.*X([0-9]{7}).*$", "\\1",names(stack_in),perl = T))  # Strip dates
  pred_dates = dates
  spline_spar=0.4  # 0.4 for RF
  workers = 5
  out_dir = '/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/'
  stack_smoother(stack_in,dates,pred_dates,spline_spar,workers,stack_name,version,out_dir)
}


# Restack Smoothed Files  ----------------------------------------------------


setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/Tifs/')  # folder where  EVI .tifs are
# create data stack for each variable and tile

# load data stacks from both directories
dir1 = list.files('.','.RData',full.names=T)
lapply(dir1, load,.GlobalEnv)

foreach(product = c('NDVI','EVI')) %do% {
  for( tile_2_process in  tiles){
    print(paste('processing',product,tile_2_process,sep=' '))
    # Set up data
    flist = list.files(".",glob2rx(paste(product,'_',tile_2_process,'*','.tif$',sep='')),
                       full.names = TRUE)
    flist_dates = gsub("^.*_X([0-9]{7}).*$", "\\1",flist,perl = T)  # Strip dates
    flist = flist[order(flist_dates)]  # file list in order
    flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order

    # stack data and save
    stacked = stack(flist)
    names(stacked) = flist_dates
    assign(paste(product,'stack',tile_2_process,'smooth',sep='_'),stacked)
    save( list=paste(product,'stack',tile_2_process,'smooth',sep='_') ,
          file = paste('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Smoothed/',
                       product,'_stack_',tile_2_process,'_smooth','.RData',sep='') )
  }}


