

# Run the following in bash before starting R
if [ -e $HOME/.Renviron ]; then cp $HOME/.Renviron $HOME/.Renviron.bkp; fi
if [ ! -d $HOME/.Rtmp ] ; then mkdir $HOME/.Rtmp; fi
echo "TMP='$HOME/.Rtmp'" > $HOME/.Renviron

module load proj.4/4.8.0
module load gdal/gcc/1.11
module load R/3.3.3
module load gcc/4.9.0
R


rm(list=ls())


library(readstata13)
library(VSURF)

version = 4

#https://journal.r-project.org/archive/2015-2/genuer-poggi-tuleaumalot.pdf
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/')
data_in = read.dta13(paste("./Outputs4Pred/AgSS_2010_15_Compiled_panel_merged_clean_v",version,".dta",sep=''))
data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]

#data_in = data_in[,1:dim(data_in)[2]]
set.seed(2734, kind = "L'Ecuyer-CMRG")

# list some variables
#paste( names(data_in)[grepl('WHEAT',names(data_in))] , collapse=' + ')


# Find factor and physical  variables -------------------------------------------------------

# Find WHEAT ALL  variables -------------------------------------------------------



form4 = WHEATOPH_W ~ Year+REGIONCODE+ZONECODE+X_COORD+Y_COORD + WHEATAREA +dist_rcap+ roadden+ dist_pp50k+ elevation+
 	WHEATEXTAREA + WHEATIRRGAREA + WHEATSERRAREA + WHEATMERR1AREA + WHEATMERR2AREA + WHEATMERR3AREA + WHEATMERR4AREA +
        WHEATMERR5AREA + WHEATSEED1AREA + WHEATSEED2AREA +WHEATIMSEED + WHEATNIMSEED + WHEATDAMAGEAREA + WHEATDAMAGE_WEATHER_AREA + 
	WHEATDAMAGE_PESTS_AREA + WHEATDAMAGE_MANAGE_AREA + WHEATDAMAGE_OTHER_AREA + WHEATDAMAGE_DROUGHT_AREA + 
	WHEATDAMAGE_DROUGHT_DUM + WHEATFERT_NATURAL_AREA + WHEATFERT_CHEMICAL_AREA +  WHEATFERT_CHEMICAL_AMT+
        WHEATEXTAREA_P + WHEATIRRGAREA_P + WHEATSERRAREA_P + WHEATMERR1AREA_P + WHEATMERR2AREA_P + WHEATMERR3AREA_P +
        WHEATMERR4AREA_P +   WHEATMERR5AREA_P + WHEATSEED1AREA_P + WHEATSEED2AREA_P + WHEATIMSEED_P + WHEATNIMSEED_P +
        WHEATDAMAGEAREA_P + WHEATDAMAGE_WEATHER_AREA_P +   WHEATDAMAGE_PESTS_AREA_P + WHEATDAMAGE_MANAGE_AREA_P +
        WHEATDAMAGE_OTHER_AREA_P + WHEATDAMAGE_DROUGHT_AREA_P + WHEATFERT_NATURAL_AREA_P +   WHEATFERT_CHEMICAL_AREA_P +
        WHEATFERT_CHEMICAL_AMT_P + A_mn + A_min + A_max + A_AUC + A_Qnt + A_sd +  A_max_Qnt +  A_AUC_Qnt  +
        G_mn  + G_min + G_mx + G_AUC + G_Qnt + G_mx_Qnt + G_AUC_Qnt + G_AUC2 + G_AUC_leading + G_AUC_trailing + G_AUC_diff_mn +
        G_AUC_diff_90th+T_G_Qnt+G_sd + PET_A_mn + PET_A_min + PET_A_max + PET_A_AUC + PET_A_Qnt + PET_A_sd + PET_G_mn + 
	PET_G_min + PET_G_mx + PET_G_AUC +
        PET_G_Qnt + PET_G_AUC2 + PET_G_AUC_leading + PET_G_AUC_trailing + PET_G_AUC_diff_mn + PET_G_AUC_diff_90th + PET_G_sd+ ETA_A_mn +
        ETA_A_min + ETA_A_max + ETA_A_AUC + ETA_A_Qnt + ETA_A_sd + ETA_G_mn + ETA_G_min + ETA_G_mx + ETA_G_AUC + ETA_G_Qnt + ETA_G_AUC2 +
        ETA_G_AUC_leading + ETA_G_AUC_trailing + ETA_G_AUC_diff_mn + ETA_G_AUC_diff_90th + ETA_G_sd+ PPT_A_mn + PPT_A_max + PPT_A_sd +
        PPT_G_mn + PPT_G_mx + PPT_G_AUC + PPT_G_Qnt + PPT_G_mx_Qnt + PPT_G_AUC_Qnt + PPT_G_AUC2 + PPT_G_AUC_leading + PPT_G_AUC_trailing +
        PPT_G_AUC_diff_mn + PPT_G_AUC_diff_90th + PPT_T_G_Qnt + PPT_G_sd+soil_TAWC


set.seed(2734, kind = "L'Ecuyer-CMRG")

vswheat4 = VSURF(form4,  data= data_in, na.action=na.omit,
                parallel = T, ncores = 30 , clusterType = "FORK" )  # mtry default is # Xs / 3


save(vswheat4,file = '../Data/VariableSelection/vswheat_4.RData')




# Load variables selected ---------------------------------------------------------
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/')
load('../Data/VariableSelection/vswheat_4.RData')



# 4 - ALL variables - get names of selected variables at interpretation phase of variables selection

# interpretation phase
attr(vswheat4$terms,'term.labels')[vswheat4$varselect.interp]
# prediction phase
attr(vswheat4$terms,'term.labels')[vswheat4$varselect.pred]







# Find ALL MAIZE  variables -------------------------------------------------------



form4_maz = MAIZEOPH_W ~ Year+REGIONCODE+ZONECODE+X_COORD+Y_COORD + MAIZEAREA +dist_rcap+ roadden+ dist_pp50k+ elevation+
        MAIZEEXTAREA + MAIZEIRRGAREA + MAIZESERRAREA + MAIZEMERR1AREA + MAIZEMERR2AREA + MAIZEMERR3AREA + MAIZEMERR4AREA +
        MAIZEMERR5AREA + MAIZESEED1AREA + MAIZESEED2AREA + MAIZEIMSEED + MAIZENIMSEED + MAIZEDAMAGEAREA + MAIZEDAMAGE_WEATHER_AREA +
        MAIZEDAMAGE_PESTS_AREA + MAIZEDAMAGE_MANAGE_AREA + MAIZEDAMAGE_OTHER_AREA + MAIZEDAMAGE_DROUGHT_AREA +
        MAIZEDAMAGE_DROUGHT_DUM + MAIZEFERT_NATURAL_AREA + MAIZEFERT_CHEMICAL_AREA +  MAIZEFERT_CHEMICAL_AMT+
        MAIZEEXTAREA_P + MAIZEIRRGAREA_P + MAIZESERRAREA_P + MAIZEMERR1AREA_P + MAIZEMERR2AREA_P + MAIZEMERR3AREA_P +
        MAIZEMERR4AREA_P + MAIZEMERR5AREA_P + MAIZESEED1AREA_P + MAIZESEED2AREA_P + MAIZEIMSEED_P + MAIZENIMSEED_P +
        MAIZEDAMAGEAREA_P + MAIZEDAMAGE_WEATHER_AREA_P + MAIZEDAMAGE_PESTS_AREA_P + MAIZEDAMAGE_MANAGE_AREA_P +
        MAIZEDAMAGE_OTHER_AREA_P + MAIZEDAMAGE_DROUGHT_AREA_P + MAIZEFERT_NATURAL_AREA_P + MAIZEFERT_CHEMICAL_AREA_P +
        MAIZEFERT_CHEMICAL_AMT_P + A_mn + A_min + A_max + A_AUC + A_Qnt + A_sd +  A_max_Qnt +  A_AUC_Qnt  +
        G_mn  + G_min + G_mx + G_AUC + G_Qnt + G_mx_Qnt + G_AUC_Qnt + G_AUC2 + G_AUC_leading + G_AUC_trailing + G_AUC_diff_mn +
        G_AUC_diff_90th+T_G_Qnt+G_sd + PET_A_mn + PET_A_min + PET_A_max + PET_A_AUC + PET_A_Qnt + PET_A_sd + PET_G_mn +
        PET_G_min + PET_G_mx + PET_G_AUC +
        PET_G_Qnt + PET_G_AUC2 + PET_G_AUC_leading + PET_G_AUC_trailing + PET_G_AUC_diff_mn + PET_G_AUC_diff_90th + PET_G_sd+ ETA_A_mn +
        ETA_A_min + ETA_A_max + ETA_A_AUC + ETA_A_Qnt + ETA_A_sd + ETA_G_mn + ETA_G_min + ETA_G_mx + ETA_G_AUC + ETA_G_Qnt + ETA_G_AUC2 +
        ETA_G_AUC_leading + ETA_G_AUC_trailing + ETA_G_AUC_diff_mn + ETA_G_AUC_diff_90th + ETA_G_sd+ PPT_A_mn + PPT_A_max + PPT_A_sd +
        PPT_G_mn + PPT_G_mx + PPT_G_AUC + PPT_G_Qnt + PPT_G_mx_Qnt + PPT_G_AUC_Qnt + PPT_G_AUC2 + PPT_G_AUC_leading + PPT_G_AUC_trailing +
        PPT_G_AUC_diff_mn + PPT_G_AUC_diff_90th + PPT_T_G_Qnt + PPT_G_sd+soil_TAWC


set.seed(2734, kind = "L'Ecuyer-CMRG")

vsmaize4 = VSURF(form4_maz,  data= data_in, na.action=na.omit,
                parallel = T, ncores = 15 , clusterType = "FORK" )  # mtry default is # Xs / 3

save(vsmaize4,file = '../Data/VariableSelection/vsmaize_4.RData')



# Find ALL BARLEY variables -------------------------------------------------------



form4_bar = BARLEYOPH_W ~ Year+REGIONCODE+ZONECODE+X_COORD+Y_COORD + BARLEYAREA +dist_rcap+ roadden+ dist_pp50k+ elevation+
        BARLEYEXTAREA + BARLEYIRRGAREA + BARLEYSERRAREA + BARLEYMERR1AREA + BARLEYMERR2AREA + BARLEYMERR3AREA + BARLEYMERR4AREA +
        BARLEYMERR5AREA + BARLEYSEED1AREA + BARLEYSEED2AREA + BARLEYIMSEED + BARLEYNIMSEED + BARLEYDAMAGEAREA + BARLEYDAMAGE_WEATHER_AREA +
        BARLEYDAMAGE_PESTS_AREA + BARLEYDAMAGE_MANAGE_AREA + BARLEYDAMAGE_OTHER_AREA + BARLEYDAMAGE_DROUGHT_AREA +
        BARLEYDAMAGE_DROUGHT_DUM + BARLEYFERT_NATURAL_AREA + BARLEYFERT_CHEMICAL_AREA +  BARLEYFERT_CHEMICAL_AMT+
        BARLEYEXTAREA_P + BARLEYIRRGAREA_P + BARLEYSERRAREA_P + BARLEYMERR1AREA_P + BARLEYMERR2AREA_P + BARLEYMERR3AREA_P +
        BARLEYMERR4AREA_P + BARLEYMERR5AREA_P + BARLEYSEED1AREA_P + BARLEYSEED2AREA_P + BARLEYIMSEED_P + BARLEYNIMSEED_P +
        BARLEYDAMAGEAREA_P + BARLEYDAMAGE_WEATHER_AREA_P + BARLEYDAMAGE_PESTS_AREA_P + BARLEYDAMAGE_MANAGE_AREA_P +
        BARLEYDAMAGE_OTHER_AREA_P + BARLEYDAMAGE_DROUGHT_AREA_P + BARLEYFERT_NATURAL_AREA_P + BARLEYFERT_CHEMICAL_AREA_P +
        BARLEYFERT_CHEMICAL_AMT_P + A_mn + A_min + A_max + A_AUC + A_Qnt + A_sd +  A_max_Qnt +  A_AUC_Qnt  +
        G_mn  + G_min + G_mx + G_AUC + G_Qnt + G_mx_Qnt + G_AUC_Qnt + G_AUC2 + G_AUC_leading + G_AUC_trailing + G_AUC_diff_mn +
        G_AUC_diff_90th+T_G_Qnt+G_sd + PET_A_mn + PET_A_min + PET_A_max + PET_A_AUC + PET_A_Qnt + PET_A_sd + PET_G_mn +
        PET_G_min + PET_G_mx + PET_G_AUC +
        PET_G_Qnt + PET_G_AUC2 + PET_G_AUC_leading + PET_G_AUC_trailing + PET_G_AUC_diff_mn + PET_G_AUC_diff_90th + PET_G_sd+ ETA_A_mn +
        ETA_A_min + ETA_A_max + ETA_A_AUC + ETA_A_Qnt + ETA_A_sd + ETA_G_mn + ETA_G_min + ETA_G_mx + ETA_G_AUC + ETA_G_Qnt + ETA_G_AUC2 +
        ETA_G_AUC_leading + ETA_G_AUC_trailing + ETA_G_AUC_diff_mn + ETA_G_AUC_diff_90th + ETA_G_sd+ PPT_A_mn + PPT_A_max + PPT_A_sd +
        PPT_G_mn + PPT_G_mx + PPT_G_AUC + PPT_G_Qnt + PPT_G_mx_Qnt + PPT_G_AUC_Qnt + PPT_G_AUC2 + PPT_G_AUC_leading + PPT_G_AUC_trailing +
        PPT_G_AUC_diff_mn + PPT_G_AUC_diff_90th + PPT_T_G_Qnt + PPT_G_sd+soil_TAWC


set.seed(2734, kind = "L'Ecuyer-CMRG")

vsbarley4 = VSURF(form4_bar,  data= data_in, na.action=na.omit,
                parallel = T, ncores = 15 , clusterType = "FORK" )  # mtry default is # Xs / 3

save(vsbarley4,file = '../Data/VariableSelection/vsbarley_4.RData')




# Find ALL TEFF variables -------------------------------------------------------



form4_tef = TEFFOPH_W ~ Year+REGIONCODE+ZONECODE+X_COORD+Y_COORD + TEFFAREA +dist_rcap+ roadden+ dist_pp50k+ elevation+
        TEFFEXTAREA + TEFFIRRGAREA + TEFFSERRAREA + TEFFMERR1AREA + TEFFMERR2AREA + TEFFMERR3AREA + TEFFMERR4AREA +
        TEFFMERR5AREA + TEFFSEED1AREA + TEFFSEED2AREA + TEFFIMSEED + TEFFNIMSEED + TEFFDAMAGEAREA + TEFFDAMAGE_WEATHER_AREA +
        TEFFDAMAGE_PESTS_AREA + TEFFDAMAGE_MANAGE_AREA + TEFFDAMAGE_OTHER_AREA + TEFFDAMAGE_DROUGHT_AREA +
        TEFFDAMAGE_DROUGHT_DUM + TEFFFERT_NATURAL_AREA + TEFFFERT_CHEMICAL_AREA +  TEFFFERT_CHEMICAL_AMT+
        TEFFEXTAREA_P + TEFFIRRGAREA_P + TEFFSERRAREA_P + TEFFMERR1AREA_P + TEFFMERR2AREA_P + TEFFMERR3AREA_P +
        TEFFMERR4AREA_P + TEFFMERR5AREA_P + TEFFSEED1AREA_P + TEFFSEED2AREA_P + TEFFIMSEED_P + TEFFNIMSEED_P +
        TEFFDAMAGEAREA_P + TEFFDAMAGE_WEATHER_AREA_P + TEFFDAMAGE_PESTS_AREA_P + TEFFDAMAGE_MANAGE_AREA_P +
        TEFFDAMAGE_OTHER_AREA_P + TEFFDAMAGE_DROUGHT_AREA_P + TEFFFERT_NATURAL_AREA_P + TEFFFERT_CHEMICAL_AREA_P +
        TEFFFERT_CHEMICAL_AMT_P + A_mn + A_min + A_max + A_AUC + A_Qnt + A_sd +  A_max_Qnt +  A_AUC_Qnt  +
        G_mn  + G_min + G_mx + G_AUC + G_Qnt + G_mx_Qnt + G_AUC_Qnt + G_AUC2 + G_AUC_leading + G_AUC_trailing + G_AUC_diff_mn +
        G_AUC_diff_90th+T_G_Qnt+G_sd + PET_A_mn + PET_A_min + PET_A_max + PET_A_AUC + PET_A_Qnt + PET_A_sd + PET_G_mn +
        PET_G_min + PET_G_mx + PET_G_AUC +
        PET_G_Qnt + PET_G_AUC2 + PET_G_AUC_leading + PET_G_AUC_trailing + PET_G_AUC_diff_mn + PET_G_AUC_diff_90th + PET_G_sd+ ETA_A_mn +
        ETA_A_min + ETA_A_max + ETA_A_AUC + ETA_A_Qnt + ETA_A_sd + ETA_G_mn + ETA_G_min + ETA_G_mx + ETA_G_AUC + ETA_G_Qnt + ETA_G_AUC2 +
        ETA_G_AUC_leading + ETA_G_AUC_trailing + ETA_G_AUC_diff_mn + ETA_G_AUC_diff_90th + ETA_G_sd+ PPT_A_mn + PPT_A_max + PPT_A_sd +
        PPT_G_mn + PPT_G_mx + PPT_G_AUC + PPT_G_Qnt + PPT_G_mx_Qnt + PPT_G_AUC_Qnt + PPT_G_AUC2 + PPT_G_AUC_leading + PPT_G_AUC_trailing +
        PPT_G_AUC_diff_mn + PPT_G_AUC_diff_90th + PPT_T_G_Qnt + PPT_G_sd+soil_TAWC


set.seed(2734, kind = "L'Ecuyer-CMRG")

vsteff4 = VSURF(form4_tef,  data= data_in, na.action=na.omit,
                parallel = T, ncores = 15 , clusterType = "FORK" )  # mtry default is # Xs / 3

save(vsteff4,file = '../Data/VariableSelection/vsteff_4.RData')





# Find ALL SORGHUM variables -------------------------------------------------------



form4_tef = SORGHUMOPH_W ~ Year+REGIONCODE+ZONECODE+X_COORD+Y_COORD + SORGHUMAREA +dist_rcap+ roadden+ dist_pp50k+ elevation+
        SORGHUMEXTAREA + SORGHUMIRRGAREA + SORGHUMSERRAREA + SORGHUMMERR1AREA + SORGHUMMERR2AREA + SORGHUMMERR3AREA + SORGHUMMERR4AREA +
        SORGHUMMERR5AREA + SORGHUMSEED1AREA + SORGHUMSEED2AREA + SORGHUMIMSEED + SORGHUMNIMSEED + SORGHUMDAMAGEAREA + SORGHUMDAMAGE_WEATHER_AREA +
        SORGHUMDAMAGE_PESTS_AREA + SORGHUMDAMAGE_MANAGE_AREA + SORGHUMDAMAGE_OTHER_AREA + SORGHUMDAMAGE_DROUGHT_AREA +
        SORGHUMDAMAGE_DROUGHT_DUM + SORGHUMFERT_NATURAL_AREA + SORGHUMFERT_CHEMICAL_AREA +  SORGHUMFERT_CHEMICAL_AMT+
        SORGHUMEXTAREA_P + SORGHUMIRRGAREA_P + SORGHUMSERRAREA_P + SORGHUMMERR1AREA_P + SORGHUMMERR2AREA_P + SORGHUMMERR3AREA_P +
        SORGHUMMERR4AREA_P + SORGHUMMERR5AREA_P + SORGHUMSEED1AREA_P + SORGHUMSEED2AREA_P + SORGHUMIMSEED_P + SORGHUMNIMSEED_P +
        SORGHUMDAMAGEAREA_P + SORGHUMDAMAGE_WEATHER_AREA_P + SORGHUMDAMAGE_PESTS_AREA_P + SORGHUMDAMAGE_MANAGE_AREA_P +
        SORGHUMDAMAGE_OTHER_AREA_P + SORGHUMDAMAGE_DROUGHT_AREA_P + SORGHUMFERT_NATURAL_AREA_P + SORGHUMFERT_CHEMICAL_AREA_P +
        SORGHUMFERT_CHEMICAL_AMT_P + A_mn + A_min + A_max + A_AUC + A_Qnt + A_sd +  A_max_Qnt +  A_AUC_Qnt  +
        G_mn  + G_min + G_mx + G_AUC + G_Qnt + G_mx_Qnt + G_AUC_Qnt + G_AUC2 + G_AUC_leading + G_AUC_trailing + G_AUC_diff_mn +
        G_AUC_diff_90th+T_G_Qnt+G_sd + PET_A_mn + PET_A_min + PET_A_max + PET_A_AUC + PET_A_Qnt + PET_A_sd + PET_G_mn +
        PET_G_min + PET_G_mx + PET_G_AUC +
        PET_G_Qnt + PET_G_AUC2 + PET_G_AUC_leading + PET_G_AUC_trailing + PET_G_AUC_diff_mn + PET_G_AUC_diff_90th + PET_G_sd+ ETA_A_mn +
        ETA_A_min + ETA_A_max + ETA_A_AUC + ETA_A_Qnt + ETA_A_sd + ETA_G_mn + ETA_G_min + ETA_G_mx + ETA_G_AUC + ETA_G_Qnt + ETA_G_AUC2 +
        ETA_G_AUC_leading + ETA_G_AUC_trailing + ETA_G_AUC_diff_mn + ETA_G_AUC_diff_90th + ETA_G_sd+ PPT_A_mn + PPT_A_max + PPT_A_sd +
        PPT_G_mn + PPT_G_mx + PPT_G_AUC + PPT_G_Qnt + PPT_G_mx_Qnt + PPT_G_AUC_Qnt + PPT_G_AUC2 + PPT_G_AUC_leading + PPT_G_AUC_trailing +
        PPT_G_AUC_diff_mn + PPT_G_AUC_diff_90th + PPT_T_G_Qnt + PPT_G_sd+soil_TAWC


set.seed(2734, kind = "L'Ecuyer-CMRG")

vssorghum4 = VSURF(form4_sor,  data= data_in, na.action=na.omit,
                parallel = T, ncores = 15 , clusterType = "FORK" )  # mtry default is # Xs / 3

save(vssorghum4,file = '../Data/VariableSelection/vssorghum_4.RData')











#### Panel Regression ----------------------------------------------------

  # read data
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/')
  data_in = read.dta13(paste("./Outputs4Pred/AgSS_2010_15_Compiled_panel_merged_clean_v",version,".dta",sep=''))
  data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]

  # remove EA with less than 4 observations
  library(dplyr)
  counts = as.data.frame(data_in  %>% group_by(EACODE) %>% summarise(non_na_count = sum(!is.na(MAIZEOPH_W))) %>% filter(non_na_count<4))
  data_in = data_in[!(data_in$EACODE %in% counts$EACODE),]
  
  # define as panel data
  library(plm)
  data_in_plm <- pdata.frame(data_in, index=c("EACODE","Year"),  row.names=TRUE)
  


 ######## MAIZE

 # read data
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/')
  data_in = read.dta13(paste("./Outputs4Pred/AgSS_2010_15_Compiled_panel_merged_clean_v",version,".dta",sep=''))
  data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]

  # remove EA with less than 4 observations
  library(dplyr)
  counts = as.data.frame(data_in  %>% group_by(EACODE) %>% summarise(non_na_count = sum(!is.na(MAIZEOPH_W))) %>% filter(non_na_count<4))
  data_in = data_in[!(data_in$EACODE %in% counts$EACODE),]

  # define as panel data
  library(plm)
  data_in_plm <- pdata.frame(data_in, index=c("EACODE","Year"),  row.names=TRUE)



  load('../Data/VariableSelection/vsmaize_4.RData')
  form_1_maz = paste(attr(vsmaize4$terms,'term.labels')[vsmaize4$varselect.pred], collapse='+')
  form_1_maz = as.formula(paste('MAIZEOPH_W ~',form_1_maz,sep=' '))

  form_1_maz_w = paste(c(attr(vsmaize4$terms,'term.labels')[vsmaize4$varselect.pred],'lag(data_in_plm$MAIZEOPH_W,1)',
                "factor(W_CODE)"), collapse='+')
  form_1_maz_w = as.formula(paste('MAIZEOPH_W ~',form_1_maz_w,sep=' '))

  form_1_maz_z = paste(c(attr(vsmaize4$terms,'term.labels')[vsmaize4$varselect.pred],'lag(data_in_plm$MAIZEOPH_W,1)',
                "factor(Z_CODE)"), collapse='+')
  form_1_maz_z = as.formula(paste('MAIZEOPH_W ~',form_1_maz_z,sep=' '))

  maz.re <- plm(form_1_maz_z, data = data_in_plm, model = "random")
  summary(maz.re)


  
 ######## WHEAT

 # read data
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/')
  data_in = read.dta13(paste("./Outputs4Pred/AgSS_2010_15_Compiled_panel_merged_clean_v",version,".dta",sep=''))
  data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]

  # remove EA with less than 4 observations
  library(dplyr)
  counts = as.data.frame(data_in  %>% group_by(EACODE) %>% summarise(non_na_count = sum(!is.na(WHEATOPH_W))) %>% filter(non_na_count<4))
  data_in = data_in[!(data_in$EACODE %in% counts$EACODE),]

  # define as panel data
  library(plm)
  data_in_plm <- pdata.frame(data_in, index=c("EACODE","Year"),  row.names=TRUE)


  load('../Data/VariableSelection/vswheat_4.RData')
  form_1_wht = paste(attr(vswheat4$terms,'term.labels')[vswheat4$varselect.pred], collapse='+')
  form_1_wht = as.formula(paste('WHEATOPH_W ~',form_1_wht,sep=' '))

  form_1_wht_w = paste(c(attr(vswheat4$terms,'term.labels')[vswheat4$varselect.pred],'lag(data_in_plm$WHEATOPH_W,1)',
                "factor(W_CODE)"), collapse='+')
  form_1_wht_w = as.formula(paste('WHEATOPH_W ~',form_1_wht_w,sep=' '))

  form_1_wht_z = paste(c(attr(vswheat4$terms,'term.labels')[vswheat4$varselect.pred],'lag(data_in_plm$WHEATOPH_W,1)',
                "factor(Z_CODE)"), collapse='+')
  form_1_wht_z = as.formula(paste('WHEATOPH_W ~',form_1_wht_z,sep=' '))

  wht.re <- plm(form_1_wht_z, data = data_in_plm, model = "random")
  summary(wht.re)
  
  



 ######### BARLEY

 # read data
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/')
  data_in = read.dta13(paste("./Outputs4Pred/AgSS_2010_15_Compiled_panel_merged_clean_v",version,".dta",sep=''))
  data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]

  # remove EA with less than 4 observations
  library(dplyr)
  counts = as.data.frame(data_in  %>% group_by(EACODE) %>% summarise(non_na_count = sum(!is.na(BARLEYOPH_W))) %>% filter(non_na_count<4))
  data_in = data_in[!(data_in$EACODE %in% counts$EACODE),]

  # define as panel data
  library(plm)
  data_in_plm <- pdata.frame(data_in, index=c("EACODE","Year"),  row.names=TRUE)

  load('../Data/VariableSelection/vsbarley_4.RData')
  form_1_bar = paste(attr(vsbarley4$terms,'term.labels')[vsbarley4$varselect.pred], collapse='+')
  form_1_bar = as.formula(paste('BARLEYOPH_W ~',form_1_bar,sep=' '))

  form_1_bar_w = paste(c(attr(vsbarley4$terms,'term.labels')[vsbarley4$varselect.pred],'lag(data_in_plm$BARLEYOPH_W,1)',
		"factor(W_CODE)"), collapse='+')
  form_1_bar_w = as.formula(paste('BARLEYOPH_W ~',form_1_bar_w,sep=' '))

  form_1_bar_z = paste(c(attr(vsbarley4$terms,'term.labels')[vsbarley4$varselect.pred],'lag(data_in_plm$BARLEYOPH_W,1)',
		"factor(Z_CODE)"), collapse='+')
  form_1_bar_z = as.formula(paste('BARLEYOPH_W ~',form_1_bar_z,sep=' '))

  bar.re <- plm(form_1_bar_z, data = data_in_plm, model = "random")
  summary(bar.re)



 # formula TEFF
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/')
  data_in = read.dta13(paste("./Outputs4Pred/AgSS_2010_15_Compiled_panel_merged_clean_v",version,".dta",sep=''))
  data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]

  # remove EA with less than 4 observations
  library(dplyr)
  counts = as.data.frame(data_in  %>% group_by(EACODE) %>% summarise(non_na_count = sum(!is.na(TEFFOPH_W))) %>% filter(non_na_count<4))
  data_in = data_in[!(data_in$EACODE %in% counts$EACODE),]

  # define as panel data
  library(plm)
  data_in_plm <- pdata.frame(data_in, index=c("EACODE","Year"),  row.names=TRUE)

  load('../Data/VariableSelection/vsteff_4.RData')
  form_1_tef = paste(attr(vsteff4$terms,'term.labels')[vsteff4$varselect.pred], collapse='+')
  form_1_tef = as.formula(paste('TEFFOPH_W ~',form_1_tef,sep=' '))

  form_1_tef_w = paste(c(attr(vsteff4$terms,'term.labels')[vsteff4$varselect.pred],'lag(data_in_plm$TEFFOPH_W,1)',
                "factor(W_CODE)"), collapse='+')
  form_1_tef_w = as.formula(paste('TEFFOPH_W ~',form_1_tef_w,sep=' '))

  form_1_tef_z = paste(c(attr(vsteff4$terms,'term.labels')[vsteff4$varselect.pred],'lag(data_in_plm$TEFFOPH_W,1)',
                "factor(Z_CODE)"), collapse='+')
  form_1_tef_z = as.formula(paste('TEFFOPH_W ~',form_1_tef_z,sep=' '))

  tef.re <- plm(form_1_tef_z, data = data_in_plm, model = "random")
  summary(tef.re)




 # formula SORGHUM
  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/')
  data_in = read.dta13(paste("./Outputs4Pred/AgSS_2010_15_Compiled_panel_merged_clean_v",version,".dta",sep=''))
  data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]

  # remove EA with less than 4 observations
  library(dplyr)
  counts = as.data.frame(data_in  %>% group_by(EACODE) %>% summarise(non_na_count = sum(!is.na(SORGHUMOPH_W))) %>% filter(non_na_count<4))
  data_in = data_in[!(data_in$EACODE %in% counts$EACODE),]

  # define as panel data
  library(plm)
  data_in_plm <- pdata.frame(data_in, index=c("EACODE","Year"),  row.names=TRUE)

  load('../Data/VariableSelection/vssorghum_4.RData')
  form_1_sor = paste(attr(vssorghum4$terms,'term.labels')[vssorghum4$varselect.pred], collapse='+')
  form_1_sor = as.formula(paste('SORGHUMOPH_W ~',form_1_sor,sep=' '))

  form_1_sor_w = paste(c(attr(vssorghumf4$terms,'term.labels')[vssorghum4$varselect.pred],'lag(data_in_plm$SORGHUMOPH_W,1)',
               	"factor(W_CODE)"), collapse='+')
  form_1_sor_w = as.formula(paste('SORGHUMOPH_W ~',form_1_sor_w,sep=' '))

  form_1_sor_z = paste(c(attr(vssorghum4$terms,'term.labels')[vssorghum4$varselect.pred],'lag(data_in_plm$SORGHUMOPH_W,1)',
                "factor(Z_CODE)"), collapse='+')
  form_1_sor_z = as.formula(paste('SORGHUMOPH_W ~',form_1_sor_z,sep=' '))

  sor.re <- plm(form_1_sor_z, data = data_in_plm, model = "random")
  summary(sor.re)








  form_EA_sor = as.formula(paste('SORGHUMOPH_W ~',form_EA_1,sep=' '))
  training_EA_sor = na.omit(model.frame(form_EA_sor,data_in))

  form_sor = as.formula(paste('SORGHUMOPH_W ~',form_1,sep=' '))
  training_sor = na.omit(model.frame(form_sor,data_in))

  sor.re <- plm(form_sor, data = data_in_plm, model = "random")
  summary(sor.re)
  summary(sor.re$residuals)





# Machine Learning Alternatives -------------------------------------------



  library(REEMtree)
  library(readstata13)
  library(e1071)
  library(randomForest)


  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/Outputs4Pred/')

  # load variable selections
  load('../Data/VariableSelection/vsmaize_4.RData')
  load('../Data/VariableSelection/vsbarley_4.RData')
  load('../Data/VariableSelection/vswheat_4.RData')





#### RE-EM Tree RE Trees

  reem1 = REEMtree(form_EA, data=training_EA, random=~1|EACODE|Year)
  plot(reem1)
  reem1


#### Support Vector Regression
  source( '../mctune.R')


  ### WHEAT 
  # read data
  data_in = read.dta13("./AgSS_2010_15_Compiled_panel_merged_clean_v4.dta")
  data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]
  # remove less than 4 obs 
  counts = as.data.frame(data_in  %>% group_by(EACODE) %>% summarise(non_na_count = sum(!is.na(WHEATOPH_W))) %>% filter(non_na_count<4))
  data_in = data_in[!(data_in$EACODE %in% counts$EACODE),]
  # formula
  form_1_wht_z = paste(c(attr(vswheat4$terms,'term.labels')[vswheat4$varselect.pred],'1'), collapse='+')
  form_1_wht_z = as.formula(paste('WHEATOPH_W ~',form_1_wht_z,sep=' '))


  # Problem not spliting by groups... look at gkf.split for scikitlearn
  svm_model1 <- svm(form_1_wht_z,data_in)
  summary(svm_model1)
  summary((predict(svm_model1)- training$WHEATOPH_W))

  # Tuned regression
  svr_tuned <- tune(svm, form_1_wht_z,  data = data_in,
                     ranges = list(epsilon = seq(0,1,0.1), cost = seq(0.1,1,.25))) 
  svr_tuned$best.model
  print(svr_tuned)
  plot(svr_tuned)
  save(svr_tuned, file = '../Data/Models/svr_tuned_wht.RData')
  load('./svr_tuned.RData')

  predict(svr_tuned$best.model)
  summary(svr_tuned$best.model$residuals)



#### Random Forest 
  # http://www.bios.unc.edu/~dzeng/BIOS740/randomforest.pdf

  ### WHEAT
  # read data
  data_in = read.dta13("./AgSS_2010_15_Compiled_panel_merged_clean_v4.dta")
  data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]
  # remove less than 4 obs
  counts = as.data.frame(data_in  %>% group_by(EACODE) %>% summarise(non_na_count = sum(!is.na(WHEATOPH_W))) %>% filter(non_na_count<4))
  data_in = data_in[!(data_in$EACODE %in% counts$EACODE),]
  # formula
  form_1_wht_z = paste(c(attr(vswheat4$terms,'term.labels')[vswheat4$varselect.pred],
                "Z_CODE"), collapse='+')
  form_1_wht_z = as.formula(paste('WHEATOPH_W ~',form_1_wht_z,sep=' '))

  # impute missing values
  set.seed(222)
  data_in.imputed <- rfImpute(form_1_wht_z, data_in[!is.na(data_in$WHEATOPH_W),])
  save(data_in.imputed,file = '../Data/Models/data_in_wht.imputed.RData')

  # estimate forest
  #names(data_in.imputed)[names(data_in.imputed)=='factor(Z_CODE)'] = 'Z_CODE'
  set.seed(1341)
  wht.rf <- randomForest(form_1_wht_z, data_in.imputed)
  varImpPlot(wht.rf)
  save(wht.rf,file = '../Data/Models/wht.rf.RData')



## CTREES 

  # Conditional Inference Tree for Kyphosis
  library(party)
  ctree_wht <- ctree(form_1_wht_z, data=data_in.imputed)
  plot(ctree_wht, main="Conditional Inference Tree for Kyphosis")


  summary((predict(ctree_wht,data_in.imputed)- data_in.imputed$WHEATOPH_W))



#### RPART - regresion trees (tunable with e1071)
  library(rpart)
  
  rpart_reg <- rpart(form_1_wht_z, data_in)
  rpart_reg
  summary((predict(rpart_reg,data_in)- data_in$WHEATOPH_W))
  
  set.seed(10)
  svm_tuned<-mctune(confusionmatrizes=T,
                    mc.control=list(mc.cores=1, mc.preschedule=F), method=svr,
                    ranges=list(type='C',kernel='radial',gamma=3^(-10:-1),cost=3^(-8:8)),
                    train.x=form,data = training,validation.x=NULL,  validation.y=NULL,
                    tunecontrol=tune.control(sampling='cross',cross=3,performances=T,nrepeat=5,best.model=T,))
  
  save(svm_tuned,
       file = '../Data/Models/svm_model_tuned_mnsdmx_newtrain_more.RData')
  load('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/svm_model_tuned_mnsdmx_newtrain_more.RData')
  
  svm_tuned$best.model
  table(predict(svm_tuned$best.model), NDVI_smooth$Class[rowSums(is.na( NDVI_smooth)) ==0])  # table with missing data removed
  plot(svm_tuned)




http://www.statmethods.net/advstats/cart.html
# Regression Tree Example
library(rpart)

# grow tree 
fit <- rpart(Mileage~Price + Country + Reliability + Type, 
   method="anova", data=cu.summary)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
  	main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
  	title = "Regression Tree for Mileage ")

click to view

# prune the tree 
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
  	main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree2.ps", 
  	title = "Pruned Regression Tree for Mileage")


