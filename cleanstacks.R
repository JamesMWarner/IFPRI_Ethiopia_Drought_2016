

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





# Remove low quality,CLEAN, & assign projection FROM RAW, THEN STACK ------------------------------------------------


# load data in previous section and run common dates
rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Data Stacks/Raw Stacks/') # don't load smoothed...
# load data stacks from both directories
versiontolook = paste('_V',version,'.RData',sep='')
dir1 = list.files('.',versiontolook,full.names=T)
lapply(dir1, load,.GlobalEnv)

# set up directories and names
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data//Data Stacks')
reliability_prefix = 'pixel_reliability'
dir.create(file.path('./WO Clouds/Tifs'), showWarnings=F,recursive=T)
dir.create(file.path('./WO Clouds Clean/tifs'), showWarnings=F,recursive=T)
dir.create(file.path('/lustre/groups/manngroup/WO Clouds Clean/Tifs'), showWarnings=F,recursive=T) # folder on high speed $

registerDoParallel(17)


# setup a dataframe with valid ranges and scale factors
valid = data.frame(stack='NDVI', fill= -3000,validL=-2000,validU=10000,
                   scale=0.0001,stringsAsFactors=F)
valid = rbind(valid,c('EVI',-3000,-2000,10000,0.0001))
valid



for(product in  c('EVI','NDVI')[2]){  #'EVI','NDVI'
  for( tile in tiles){
    print(paste('Working on',product,tile))
    # load quality flag
    reliability_stackvalues = get(paste(reliability_prefix,'_stack_',tile,sep=''))

    # remove clouds from produt
    data_stackvalues = get(paste(product,'_stack_',tile,sep=''))
    valid_values = valid[grep(product,valid$stack),]

    ScaleClean = function(x,y){
      x[x==as.numeric(valid_values$fill)]=NA
      x[x < as.numeric(valid_values$validL)]=NA
      x[x > as.numeric(valid_values$validU)]=NA
      #x = x * as.numeric(valid_values$scale)  # don't scale take up too much memory
      x[ y<0 | y>1 ] = NA     # remove very low quality
      x}
    # process and write to lustre
    foreach(i=(1:dim(data_stackvalues)[3]), .inorder=F,.errorhandling='remove') %dopar% {
      print(data_stackvalues[[i]])
      data_stackvalues[[i]] = ScaleClean(data_stackvalues[[i]],reliability_stackvalues[[i]])
      writeRaster(data_stackvalues[[i]],paste('/lustre/groups/manngroup/WO Clouds Clean/Tifs/',product,'_',tile,
                                              '_',names(data_stackvalues[[i]]),'_V',version,'.tif',sep=''),overwrite=T)
    }


    # Copy files back from lustre and delete lustre
    flist = list.files("/lustre/groups/manngroup/WO Clouds Clean/Tifs/",
                       glob2rx(paste(product,'_',tile,'*','_V',version,'.tif$',sep='')),full.names = T)
    fname = list.files("/lustre/groups/manngroup/WO Clouds Clean/Tifs/",
                       glob2rx(paste(product,'_',tile,'*','_V',version,'.tif$',sep='')),full.names = F)
    file.copy(from=flist, to=paste("./WO Clouds Clean/tifs",fname,sep='/'),
              overwrite = T, recursive = F, copy.mode = T)
    file.remove(flist)

    # Restack outputs
    print(paste('Restacking',product,tile,sep=' '))
    # Set up data
    flist = list.files("./WO Clouds Clean/tifs/",glob2rx(paste(product,'_',tile,'*','_V',version,'.tif$',sep='')),full.names = T)
    flist_dates = gsub("^.*_X([0-9]{7}).*$", "\\1",flist,perl = T)  # Strip dates
    flist = flist[order(flist_dates)]  # file list in order
    flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order

    # stack data and save
    stacked = stack(flist)
    names(stacked) = flist_dates
    assign(paste(product,'stack',tile,'wo_clouds_clean',sep='_'),stacked)
    save( list=paste(product,'stack',tile,'wo_clouds_clean',sep='_') ,
          file = paste('./WO Clouds Clean/',product,'_stack_',
                       tile,'_wo_clouds_clean','_V',version,'.RData',sep='') )
  }}

