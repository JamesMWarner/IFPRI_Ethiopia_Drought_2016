
library(RCurl)
library(doParallel)
library(raster)
library(lubridate)

out_dir = 'Z:\\Mann_Research\\IFPRI_Ethiopia_Drought_2016\\Data\\CHIRPS\\'
setwd(out_dir)
tiles = c('h21v07','h21v08','h22v07','h22v08')


# CHIRPS Rainfall Data ----------------------------------------------------

#First data downloaded from ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_dekad/tifs/
#library(R.utils)

# ungz any files
# files_gz =  list.files(path='.', pattern="*.gz", full.names=T, recursive=T)
# 
# cl <- makeCluster(4)
# registerDoParallel(cl)
# junk = foreach(file =files_gz, .inorder=F,.packages = 'R.utils') %dopar%  {	gunzip(file);return(0)}

# crop and reproject
files =  list.files(path='.', pattern="*.tif", full.names=T, recursive=T)
files = files[-c(grep('cropped',files))] # remove any already processed
 
h21v07 =raster('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/Data/VegetationIndex/MOD13Q1_2010113_h21v07.250m_16_days_EVI.tif')
h21v08 =raster('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/Data/VegetationIndex/MOD13Q1_2010113_h21v08.250m_16_days_EVI.tif')
h22v07 =raster('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/Data/VegetationIndex/MOD13Q1_2010113_h22v07.250m_16_days_EVI.tif')
h22v08 =raster('R:/Mann_Research/IFPRI_Ethiopia_Drought_2016/Data/VegetationIndex/MOD13Q1_2010113_h22v08.250m_16_days_EVI.tif')
 
outer_extent_in = union(union(extent( h21v07),extent(h21v08)),union(extent(h22v07),extent(h22v08)))

junk = foreach(file =files, .inorder=F,.packages='raster') %dopar%  {
  out_file_name = paste(basename(substr(file,1,nchar(file)-4)),'_',paste(tiles,collapse='_'),'_cropped.tif',sep='')
  print(paste(file))
  
  example = raster(file)  # read in raster
  outer_extent_out = extent(projectExtent(raster(outer_extent_in,crs=
                                               CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs')), 
                                      projection(example) ))  #project extent to match EVI data
  example =  crop(example, outer_extent_out) # crop to EVI data extent
  example[example==-9999]=NA   # remove missing data 
  # reproject raster
  projectRaster(example,crs='+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs',
                method="bilinear",  filename=out_file_name,overwrite=T) 
  return(NA)
}

# Clean up 
files =  list.files(path='.', pattern="*.tif", full.names=T, recursive=T)
files = files[-c(grep('cropped',files))] # remove any already processed
file.remove(files)


# Stack rasters & save 
setwd("/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/CHIRPS")
files = list.files(path='.', 
         pattern=paste("*_",paste(tiles,collapse='_'),"_cropped.tif",sep=''), full.names=T, recursive=T)
rain_stack = stack(files)
# strip dates
LC_dates = strptime( gsub("^.*.([0-9]{4}){1}[.]([0-9]{2})[.]([0-9]{1}).*$",replacement =  '\\1-\\2-\\3',x = files,perl = T) ,
	format='%Y-%m-%d')
#fix for dekad (-1 = 10th day 2= 20th day =3 is everything after 21)

#How does CHIRPS define a pentad?
#6 pentads = 1 calendar month. Each of first 5 pentads in a month have 5 days. The last pentad contains all the days from the 26th to the end of the month.
#How does CHIRPS define a dekad?
#A dekad = sum of 2 pentads. There are 3 dekads in a calendar month.

LC_dates$mday[LC_dates$mday==1]=10
LC_dates$mday[LC_dates$mday==2]=20
LC_dates$mday[LC_dates$mday==2]=29
names(rain_stack) = LC_dates
 

dirs = '../Data Stacks/Rain Stacks/'  
dir.create(dirs)  
save(rain_stack,file = paste(dirs,'Rain_Stack_',paste(tiles,collapse='_'),'.RData',sep=''))

