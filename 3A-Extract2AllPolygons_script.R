

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



library(data.table)
library(raster)
library(rgdal)
library(sp)
library(foreach)
library(doParallel)
library(compiler)
library(iterator)

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



# Extract polygon or points data from stacks -------------------------------------

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
  Polys = Polys[!(Polys$R_NAME %in% c('SOMALI','Addis Ababa','SOMALIE')),]


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

        print(paste("saving block",rows))
        save(Poly_Veg_Ext ,
                 file = paste(
                    '/lustre/groups/manngroup/Processed Panel/ExtractRaw/',
                    rows,product,'_panel_','_ExtractRaw','.RData',sep='') )
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




  # COMBINE DATA BACK FROM EXTRACT ----------------------------------------------------------------

  # Combine all extracted values into a single list 
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/')
  fname = list.files(".", glob2rx(paste('*','NDVI_panel__ExtractRaw.RData$',sep='')),full.names = F)
  nums = unlist(regmatches(fname, gregexpr("[0-9]+", fname)))
  fname = fname[order(as.numeric( nums))]  # put in order
  
  holder =list()
  for(i in 1:length(fname)){
 	print(i)
   	load(paste('./',fname[i],sep=''))
	holder = c(holder,Poly_Veg_Ext)
	rm(Poly_Veg_Ext)
  }

  Poly_Veg_Ext = holder
  
  dir.create(file.path('../Processed Panel/ExtractRaw_Combined_AllEAs/'), showWarnings=F,recursive=T) # create dir for tifs
  save(Poly_Veg_Ext, file = paste('../Processed Panel/ExtractRaw_Combined_AllEAs/','AllEAs_',product,'_panel_ExtractRaw.RData',sep='') )


  # Summary Functions --------------------------------------------------------
 
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/Processed Panel/ExtractRaw/')
  product = 'NDVI'
  load(paste('../Processed Panel/ExtractRaw_Combined_AllEAs/','AllEAs_',product,'_panel_ExtractRaw.RData',sep='') )


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
 save(NDVI_summary_all_EAs, file = paste('../Processed Panel/ExtractRaw_Combined_AllEAs/','AllEAs_',product,'_panel_summary.RData',sep='') )



