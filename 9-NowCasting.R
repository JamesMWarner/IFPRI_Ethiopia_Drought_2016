
# create prediction model that only uses variables from first 1/2 of growing season
rm(list=ls())

library(raster)
library(sp)
library(maptools)
library(ggplot2)
library(readstata13)
library(reshape)
library(rgdal)
library(plyr)
library(plm)
library(dplyr)
library(splines)
library(stargazer)
library(e1071)
library(randomForest)
library(caret)
library(doMC)
library(parallel)
library(DEoptim)
library(kernlab)
library(RANN)
library(pdp)
library(pander)

 
Home_dir = '~/Documents/IFPRI_Ethiopia_Drought_2016/IFPRI_Ethiopia_Drought_2016/'

 
# read data
version = 4  
setwd(Home_dir) 

######## MAIZE

# read data
data_in = read.dta13(paste("./Outputs4Pred/AgSS_2010_15_Compiled_panel_merged_clean_v",version,".dta",sep=''))
data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]

# remove EA with less than 4 observations
counts = as.data.frame(data_in  %>% group_by(EACODE) %>% summarise(non_na_count = sum(!is.na(MAIZEOPH_W))) %>% filter(non_na_count<4))
data_in = data_in[!(data_in$EACODE %in% counts$EACODE),]

# define as panel data
data_in_plm <- pdata.frame(data_in, index=c("EACODE","Year"),  row.names=TRUE)

# load variable selection results 
load('./Writeup/IFPRI 2016 Report Ethiopia/WriteupData/VariableSelection/vsmaize_4.RData')

form_1_maz_z_NC = paste(c(attr(vsmaize4$terms,'term.labels')[vsmaize4$varselect.pred],'lag(data_in_plm$MAIZEOPH_W,1)',
                       "factor(Z_CODE)"), collapse='+')
form_1_maz_z_NC = as.formula(paste('MAIZEOPH_W ~',form_1_maz_z_NC,sep=' '))

data_in_plm$Fert_Amt_Per_Area = data_in_plm$MAIZEFERT_CHEMICAL_AMT / (data_in_plm$MAIZEFERT_CHEMICAL_AREA+1)
 
agss_lag_maz_z_NC = MAIZEOPH_W ~ lag(data_in_plm$MAIZEOPH_W, 1)   +G_mx + G_mx_Qnt + G_AUC_leading+
  lag(MAIZEIMSEED,1) + lag(MAIZEDAMAGEAREA_P,1)  +  lag(Fert_Amt_Per_Area,1) +
  lag(MAIZEEXTAREA,1) + PPT_G_mx_Qnt+
 bs(elevation,3) + factor(Z_CODE) 
agss_lag_maz_z_NC <- plm(agss_lag_maz_z_NC, data = data_in_plm, model = "random")

# # store for text pasting 
# sum.maz.re_NC = summary(agss_lag_maz_z)
# coeff.maz.re_NC = sum.maz.re3[1]$coefficients
# sum.maz.re_NC

agss_lag_maz_z2_NC = MAIZEOPH_W ~   lag(data_in_plm$MAIZEOPH_W, 1)   +G_mx + G_mx_Qnt + G_AUC_leading+
  lag(MAIZEIMSEED,1) + lag(MAIZEDAMAGEAREA_P,1)  +  lag(Fert_Amt_Per_Area,1) +
  lag(MAIZEEXTAREA,1) + 
  bs(elevation,3) + factor(Z_CODE) 
agss_lag_maz_z2_NC <- plm(agss_lag_maz_z2_NC, data = data_in_plm, model = "random")


stargazer(agss_lag_maz_z_NC, agss_lag_maz_z2_NC, 
          title="Panel RE regression", type="text", 
          column.labels=c("Full", "Final"), 
          df=FALSE, digits=4,omit = "Z_CODE",omit.labels = 'Zone Dummy',no.space=T)














# ideally would use just: 
# load('./Documents/IFPRI_Ethiopia_Drought_2016/Data/Processed Panel/ExtractRaw/NDVI_Poly_Ext_sub_agss_V4.RData')
# load('./Documents/IFPRI_Ethiopia_Drought_2016/Data/Outputs/Poly_PPT_Ext_sub_V4.RData')


