

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

form0 = WHEATOPH_W ~ Year+REGIONCODE+ZONECODE+X_COORD+Y_COORD + WHEATAREA +dist_rcap+ roadden+ dist_pp50k+ elevation

vswheat0 = VSURF(form0,  data= data_in, na.action=na.omit,
                parallel = T, ncores = 40 , clusterType = "FORK" )  # mtry default is # Xs / 3


save(vswheat0,file = '../Data/VariableSelection/vswheat_0.RData')





# Find other wheat variables -------------------------------------------------------

form1 = WHEATOPH_W ~ WHEATEXTAREA + WHEATIRRGAREA + WHEATSERRAREA + WHEATMERR1AREA + WHEATMERR2AREA + WHEATMERR3AREA + WHEATMERR4AREA +
                WHEATMERR5AREA + WHEATSEED1AREA + WHEATSEED2AREA +WHEATIMSEED +
                  WHEATNIMSEED + WHEATDAMAGEAREA + WHEATDAMAGE_WEATHER_AREA + WHEATDAMAGE_PESTS_AREA + WHEATDAMAGE_MANAGE_AREA +  
		  WHEATDAMAGE_OTHER_AREA +
                  WHEATDAMAGE_DROUGHT_AREA + WHEATDAMAGE_DROUGHT_DUM + WHEATFERT_NATURAL_AREA + WHEATFERT_CHEMICAL_AREA + 
		WHEATFERT_CHEMICAL_AMT+
                 WHEATEXTAREA_P + WHEATIRRGAREA_P + WHEATSERRAREA_P + WHEATMERR1AREA_P + WHEATMERR2AREA_P + WHEATMERR3AREA_P +
                WHEATMERR4AREA_P +   WHEATMERR5AREA_P + WHEATSEED1AREA_P + WHEATSEED2AREA_P + WHEATIMSEED_P + WHEATNIMSEED_P +
                WHEATDAMAGEAREA_P + WHEATDAMAGE_WEATHER_AREA_P +   WHEATDAMAGE_PESTS_AREA_P + WHEATDAMAGE_MANAGE_AREA_P +
                WHEATDAMAGE_OTHER_AREA_P + WHEATDAMAGE_DROUGHT_AREA_P + WHEATFERT_NATURAL_AREA_P +   WHEATFERT_CHEMICAL_AREA_P +
                WHEATFERT_CHEMICAL_AMT_P

set.seed(2734, kind = "L'Ecuyer-CMRG")

vswheat1 = VSURF(form1,  data= data_in, na.action=na.omit,
                parallel = T, ncores = 40 , clusterType = "FORK" )  # mtry default is # Xs / 3


save(vswheat1,file = '../Data/VariableSelection/vswheat_1.RData')



# Find other A_ G_  NDVI  variables -------------------------------------------------------

form2 = WHEATOPH_W ~  A_mn + A_min + A_max + A_AUC + A_Qnt + A_sd +  A_max_Qnt +  A_AUC_Qnt  +
          G_mn  + G_min + G_mx + G_AUC + G_Qnt + G_mx_Qnt + G_AUC_Qnt + G_AUC2 + G_AUC_leading + G_AUC_trailing + G_AUC_diff_mn +
        G_AUC_diff_90th+T_G_Qnt+G_sd

set.seed(2734, kind = "L'Ecuyer-CMRG")

vswheat2 = VSURF(form2,  data= data_in, na.action=na.omit,
                parallel = T, ncores = 30 , clusterType = "FORK" )  # mtry default is # Xs / 3


save(vswheat2,file = '../Data/VariableSelection/vswheat_2.RData')



# Find other PET AET PPT  variables -------------------------------------------------------

paste( names(data_in)[grepl('PPT',names(data_in))] , collapse=' + ')

form3 = WHEATOPH_W ~ PET_A_mn + PET_A_min + PET_A_max + PET_A_AUC + PET_A_Qnt + PET_A_sd + PET_G_mn + PET_G_min + PET_G_mx + PET_G_AUC +
        PET_G_Qnt + PET_G_AUC2 + PET_G_AUC_leading + PET_G_AUC_trailing + PET_G_AUC_diff_mn + PET_G_AUC_diff_90th + PET_G_sd+ ETA_A_mn +
        ETA_A_min + ETA_A_max + ETA_A_AUC + ETA_A_Qnt + ETA_A_sd + ETA_G_mn + ETA_G_min + ETA_G_mx + ETA_G_AUC + ETA_G_Qnt + ETA_G_AUC2 +
        ETA_G_AUC_leading + ETA_G_AUC_trailing + ETA_G_AUC_diff_mn + ETA_G_AUC_diff_90th + ETA_G_sd+ PPT_A_mn + PPT_A_max + PPT_A_sd + 
	PPT_G_mn + PPT_G_mx + PPT_G_AUC + PPT_G_Qnt + PPT_G_mx_Qnt + PPT_G_AUC_Qnt + PPT_G_AUC2 + PPT_G_AUC_leading + PPT_G_AUC_trailing +
	PPT_G_AUC_diff_mn + PPT_G_AUC_diff_90th + PPT_T_G_Qnt + PPT_G_sd


set.seed(2734, kind = "L'Ecuyer-CMRG")

vswheat3 = VSURF(form3,  data= data_in, na.action=na.omit,
                parallel = T, ncores = 30 , clusterType = "FORK" )  # mtry default is # Xs / 3


save(vswheat3,file = '../Data/VariableSelection/vswheat_3.RData')



# Find ALL  variables -------------------------------------------------------



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
        PPT_G_AUC_diff_mn + PPT_G_AUC_diff_90th + PPT_T_G_Qnt + PPT_G_sd


set.seed(2734, kind = "L'Ecuyer-CMRG")

vswheat4 = VSURF(form4,  data= data_in, na.action=na.omit,
                parallel = T, ncores = 30 , clusterType = "FORK" )  # mtry default is # Xs / 3


save(vswheat4,file = '../Data/VariableSelection/vswheat_4.RData')




# Load variables selected ---------------------------------------------------------
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/')
load('../Data/VariableSelection/vswheat_0.RData')
load('../Data/VariableSelection/vswheat_1.RData')
load('../Data/VariableSelection/vswheat_2.RData')
load('../Data/VariableSelection/vswheat_3.RData')
load('../Data/VariableSelection/vswheat_4.RData')



# 4 - ALL variables - get names of selected variables at interpretation phase of variables selection

attr(vswheat4$terms,'term.labels')[vswheat4$varselect.interp]

 [1] "WHEATFERT_CHEMICAL_AMT"     "WHEATDAMAGEAREA_P"
 [3] "WHEATFERT_CHEMICAL_AREA"    "WHEATNIMSEED"
 [5] "G_mx"                       "A_max"
 [7] "WHEATFERT_CHEMICAL_AMT_P"   "Y_COORD"
 [9] "WHEATAREA"                  "A_Qnt"
[11] "WHEATDAMAGE_WEATHER_AREA_P" "G_Qnt"
[13] "WHEATSEED2AREA"             "Year"
[15] "WHEATNIMSEED_P"             "X_COORD"
[17] "PPT_G_Qnt"                  "PPT_G_sd"
[19] "PPT_G_mx_Qnt"               "PPT_A_max"
[21] "PPT_G_mx"                   "WHEATFERT_CHEMICAL_AREA_P"
[23] "PPT_G_AUC_Qnt"              "T_G_Qnt"
[25] "A_max_Qnt"                  "PPT_T_G_Qnt"
[27] "PPT_A_sd"                   "G_mx_Qnt"
[29] "PPT_G_AUC2"                 "PPT_G_AUC"
[31] "WHEATEXTAREA"               "WHEATDAMAGEAREA"
[33] "ZONECODE"                   "elevation"
[35] "G_sd"                       "G_AUC_Qnt"
[37] "G_mn"                       "A_sd"
[39] "PET_G_sd"


# prediction phase
attr(vswheat4$terms,'term.labels')[vswheat4$varselect.pred]

 [1] "WHEATFERT_CHEMICAL_AMT"   "WHEATDAMAGEAREA_P"
 [3] "WHEATFERT_CHEMICAL_AREA"  "WHEATNIMSEED"
 [5] "G_mx"                     "WHEATFERT_CHEMICAL_AMT_P"
 [7] "Y_COORD"                  "WHEATAREA"
 [9] "Year"                     "WHEATNIMSEED_P"
[11] "X_COORD"                  "PPT_G_Qnt"
[13] "PPT_G_mx_Qnt"             "PPT_G_AUC_Qnt"
[15] "T_G_Qnt"                  "A_max_Qnt"
[17] "PPT_T_G_Qnt"              "ZONECODE"
[19] "elevation"                "G_AUC_Qnt"
[21] "PET_G_sd"





# get names of selected variables at interpretation phase of variables selection

c(attr(vswheat0$terms,'term.labels')[vswheat0$varselect.interp],
attr(vswheat1$terms,'term.labels')[vswheat1$varselect.interp],
attr(vswheat2$terms,'term.labels')[vswheat2$varselect.interp],
attr(vswheat3$terms,'term.labels')[vswheat3$varselect.interp])


 [1] "Y_COORD"                    "X_COORD"
 [3] "ZONECODE"                   "WHEATAREA"
 [5] "elevation"                  "dist_rcap"
 [7] "dist_pp50k"                 "Year"
 [9] "WHEATNIMSEED"               "WHEATFERT_CHEMICAL_AMT"
[11] "WHEATSEED2AREA"             "WHEATDAMAGEAREA_P"
[13] "WHEATFERT_CHEMICAL_AREA"    "WHEATFERT_CHEMICAL_AMT_P"
[15] "WHEATNIMSEED_P"             "WHEATSERRAREA"
[17] "WHEATDAMAGE_WEATHER_AREA_P" "WHEATDAMAGEAREA"
[19] "WHEATMERR4AREA"             "WHEATFERT_CHEMICAL_AREA_P"
[21] "WHEATDAMAGE_WEATHER_AREA"   "WHEATEXTAREA"
[23] "WHEATMERR1AREA"             "WHEATFERT_NATURAL_AREA"
[25] "WHEATFERT_NATURAL_AREA_P"   "WHEATIMSEED"
[27] "WHEATSERRAREA_P"            "WHEATSEED1AREA"
[29] "WHEATEXTAREA_P"             "WHEATIMSEED_P"
[31] "WHEATDAMAGE_DROUGHT_AREA_P" "WHEATMERR1AREA_P"
[33] "WHEATSEED1AREA_P"           "WHEATMERR4AREA_P"
[35] "G_mx"                       "A_max"
[37] "A_Qnt"                      "G_Qnt"
[39] "G_AUC2"                     "G_AUC"
[41] "G_mn"                       "G_AUC_Qnt"
[43] "A_AUC"                      "A_mn"
[45] "A_AUC_Qnt"                  "G_min"
[47] "A_max_Qnt"                  "T_G_Qnt"
[49] "G_mx_Qnt"                   "PPT_G_AUC_Qnt"
[51] "PPT_G_AUC2"                 "PPT_G_Qnt"
[53] "PPT_G_AUC"                  "PPT_G_mx_Qnt"
[55] "PPT_A_max"                  "PPT_G_mx"
[57] "PET_G_sd"                   "PPT_A_mn"
[59] "PPT_G_sd"                   "PPT_T_G_Qnt"
[61] "PET_G_AUC_trailing"         "PET_A_sd"
[63] "PPT_G_mn"                   "PPT_A_sd"
[65] "ETA_G_Qnt"                  "PET_G_AUC2"
[67] "PET_G_AUC"                  "PET_G_Qnt"
[69] "PET_G_mn"                   "PPT_G_AUC_diff_mn"
[71] "ETA_G_mn"                   "ETA_A_Qnt"
[73] "ETA_A_max"                  "PPT_G_AUC_leading"
[75] "PET_G_AUC_diff_mn"          "ETA_A_AUC"
[77] "ETA_A_sd"                   "PET_A_mn"
[79] "PET_A_AUC"                  "ETA_G_mx"
[81] "PET_G_min"                  "PET_A_min"





# get names of selected variables at interpretation phase of variables selection

c(attr(vswheat0$terms,'term.labels')[vswheat0$varselect.pred],
attr(vswheat1$terms,'term.labels')[vswheat1$varselect.pred],
attr(vswheat2$terms,'term.labels')[vswheat2$varselect.pred],
attr(vswheat3$terms,'term.labels')[vswheat3$varselect.pred])


 [1] "Y_COORD"                 "X_COORD"
 [3] "ZONECODE"                "Year"
 [5] "WHEATNIMSEED"            "WHEATFERT_CHEMICAL_AMT"
 [7] "WHEATSEED2AREA"          "WHEATDAMAGEAREA_P"
 [9] "WHEATFERT_CHEMICAL_AREA" "G_mx"
[11] "A_Qnt"                   "G_Qnt"
[13] "G_AUC2"                  "G_mn"
[15] "G_AUC_Qnt"               "A_AUC"
[17] "A_mn"                    "A_AUC_Qnt"
[19] "G_min"                   "A_max_Qnt"
[21] "T_G_Qnt"                 "G_mx_Qnt"
[23] "PPT_G_AUC_Qnt"           "PPT_G_mx_Qnt"
[25] "PPT_A_max"               "PET_G_sd"
[27] "PPT_T_G_Qnt"             "PET_G_Qnt"
[29] "PET_G_min"



# Machine Learning Alternatives -------------------------------------------

  
  
  library(REEMtree)
  library(readstata13)
  library(e1071) 

  setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/Outputs4Pred/')
  
  
  load('../../Data/VariableSelection/vswheat_0.RData')
  load('../../Data/VariableSelection/vswheat_1.RData')
  load('../../Data/VariableSelection/vswheat_2.RData')
  load('../../Data/VariableSelection/vswheat_3.RData')
  
  
  
  data_in = read.dta13("./AgSS_2010_15_Compiled_panel_merged_clean_v4.dta")
  data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]
  
  form_EA = paste(paste(
    c(attr(vswheat0$terms,'term.labels')[vswheat0$varselect.interp],
      attr(vswheat1$terms,'term.labels')[vswheat1$varselect.interp],
      attr(vswheat2$terms,'term.labels')[vswheat2$varselect.interp],
      attr(vswheat3$terms,'term.labels')[vswheat3$varselect.interp]), collapse='+'),'+EACODE')
  
  form = paste( c(attr(vswheat0$terms,'term.labels')[vswheat0$varselect.interp],
      attr(vswheat1$terms,'term.labels')[vswheat1$varselect.interp],
      attr(vswheat2$terms,'term.labels')[vswheat2$varselect.interp],
      attr(vswheat3$terms,'term.labels')[vswheat3$varselect.interp]), collapse='+')
  
  # formula  
  form_EA = as.formula(paste('WHEATOPH_W ~',form_EA,sep=' '))
  training_EA = na.omit(model.frame(form_EA,data_in))
  
  form = as.formula(paste('WHEATOPH_W ~',form,sep=' '))
  training = na.omit(model.frame(form,data_in))


#### RE-EM Tree RE Trees 
  
  reem1 = REEMtree(form_EA, data=training_EA, random=~1|EACODE|Year)
  plot(reem1)
  reem1


#### Support Vector Regression 
  
  source( '../mctune.R')
  
  # Problem not spliting by groups... look at gkf.split for scikitlearn 
  svm_model1 <- svm(form,data_in)
  summary((predict(svm_model1)- training$WHEATOPH_W))
  
  # Tuned regression 
  svr_tuned <- tune(svm, form,  data = data_in,
                     ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
  print(svr_tuned)
  plot(svr_tuned)
  save(svr_tuned, file = './svr_tuned.RData')

  summary((predict(svr_tuned$best.model)- training$WHEATOPH_W))
 
#### Panel Regression 
  library(plm)
  data_in_plm <- pdata.frame(data_in, index=c("EACODE","Year"),  row.names=TRUE)
  
  wht.re <- plm(form, data = data_in_plm, model = "random")
  summary(wht.re)
  summary(wht.re$residuals)
  
  
  
  
  
#### RPART - regresion trees (tunable with e1071)
  library(rpart)
  
  rpart_reg <- rpart(form, data=training)
  rpart_reg
  summary((predict(rpart_reg)- training$WHEATOPH_W))
  
  set.seed(10)
  svm_tuned<-mctune(confusionmatrizes=T,
                    mc.control=list(mc.cores=1, mc.preschedule=F), method=svr,
                    ranges=list(type='C',kernel='radial',gamma=3^(-10:-1),cost=3^(-8:8)),
                    train.x=form,data = training,validation.x=NULL,  validation.y=NULL,
                    tunecontrol=tune.control(sampling='cross',cross=3,performances=T,nrepeat=5,best.model=T,))
  
  save(svm_tuned,
       file = '/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/svm_model_tuned_mnsdmx_newtrain_more.RData')
  load('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/Data/LandUseClassifications/svm_model_tuned_mnsdmx_newtrain_more.RData')
  
  svm_tuned$best.model
  table(predict(svm_tuned$best.model), NDVI_smooth$Class[rowSums(is.na( NDVI_smooth)) ==0])  # table with missing data removed
  plot(svm_tuned)

