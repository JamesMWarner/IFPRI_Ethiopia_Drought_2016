

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
#https://journal.r-project.org/archive/2015-2/genuer-poggi-tuleaumalot.pdf
setwd('/groups/manngroup/IFPRI_Ethiopia_Dought_2016/IFPRI_Ethiopia_Drought_2016/')
data_in = read.dta13("./Outputs4Pred/AgSS_2010_15_Compiled_panel_merged_clean_PCA_v3.dta")

data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]
#data_in = data_in[,1:dim(data_in)[2]]
set.seed(2734, kind = "L'Ecuyer-CMRG")

paste( names(data_in)[grepl('WHEAT',names(data_in))] , collapse=' + ')






# Find other wheat variables -------------------------------------------------------



form0 = WHEATOPH_W ~ Year+REGIONCODE+ZONECODE+X_COORD+Y_COORD + WHEATAREA

#vswheat0 = VSURF(form0,  data= data_in, na.action=na.omit,
#                parallel = T, ncores = 40 , clusterType = "FORK" )  # mtry default is # Xs / 3


#save(vswheat0,file = '../Data/VariableSelection/vswheat_0.RData')





# Find other wheat variables -------------------------------------------------------

form1 = WHEATOPH_W ~ WHEATEXTAREA + WHEATIRRGAREA + WHEATSERRAREA + WHEATMERR1AREA + WHEATMERR2AREA + WHEATMERR3AREA + WHEATMERR4AREA +
                WHEATMERR5AREA + WHEATSEED1AREA + WHEATSEED2AREA +WHEATIMSEED +
                  WHEATNIMSEED + WHEATDAMAGEAREA + WHEATDAMAGE_WEATHER_AREA + WHEATDAMAGE_PESTS_AREA + WHEATDAMAGE_MANAGE_AREA +  WHEATDAMAGE_OT$
                  WHEATDAMAGE_DROUGHT_AREA + WHEATDAMAGE_DROUGHT_DUM + WHEATFERT_NATURAL_AREA + WHEATFERT_CHEMICAL_AREA + WHEATFERT_CHEMICAL_AMT$
                 WHEATEXTAREA_P + WHEATIRRGAREA_P + WHEATSERRAREA_P + WHEATMERR1AREA_P + WHEATMERR2AREA_P + WHEATMERR3AREA_P +
                WHEATMERR4AREA_P +   WHEATMERR5AREA_P + WHEATSEED1AREA_P + WHEATSEED2AREA_P + WHEATIMSEED_P + WHEATNIMSEED_P +
                WHEATDAMAGEAREA_P + WHEATDAMAGE_WEATHER_AREA_P +   WHEATDAMAGE_PESTS_AREA_P + WHEATDAMAGE_MANAGE_AREA_P +
                WHEATDAMAGE_OTHER_AREA_P + WHEATDAMAGE_DROUGHT_AREA_P + WHEATFERT_NATURAL_AREA_P +   WHEATFERT_CHEMICAL_AREA_P +
                WHEATFERT_CHEMICAL_AMT_P

#set.seed(2734, kind = "L'Ecuyer-CMRG")
#
#vswheat1 = VSURF(form1,  data= data_in, na.action=na.omit,
#                parallel = T, ncores = 40 , clusterType = "FORK" )  # mtry default is # Xs / 3
#
#
#save(vswheat1,file = '../Data/VariableSelection/vswheat_1.RData')



# Find other A_ G_  NDVI  variables -------------------------------------------------------


form2 = WHEATOPH_W ~  A_mn + A_min + A_max + A_AUC + A_Qnt + A_sd +  A_max_Qnt +  A_AUC_Qnt  +
          G_mn  + G_min + G_mx + G_AUC + G_Qnt + G_mx_Qnt + G_AUC_Qnt + G_AUC2 + G_AUC_leading + G_AUC_trailing + G_AUC_diff_mn +
        G_AUC_diff_90th+T_G_Qnt+G_sd

set.seed(2734, kind = "L'Ecuyer-CMRG")

#vswheat2 = VSURF(form2,  data= data_in, na.action=na.omit,
#                parallel = T, ncores = 30 , clusterType = "FORK" )  # mtry default is # Xs / 3
#
#
#save(vswheat2,file = '../Data/VariableSelection/vswheat_2.RData')




# Find other PET AET  variables -------------------------------------------------------

paste( names(data_in)[grepl('AET',names(data_in))] , collapse=' + ')

form3 = WHEATOPH_W ~ PET_A_mn + PET_A_min + PET_A_max + PET_A_AUC + PET_A_Qnt + PET_A_sd + PET_G_mn + PET_G_min + PET_G_mx + PET_G_AUC +
        PET_G_Qnt + PET_G_AUC2 + PET_G_AUC_leading + PET_G_AUC_trailing + PET_G_AUC_diff_mn + PET_G_AUC_diff_90th + PET_G_sd+ ETA_A_mn +
        ETA_A_min + ETA_A_max + ETA_A_AUC + ETA_A_Qnt + ETA_A_sd + ETA_G_mn + ETA_G_min + ETA_G_mx + ETA_G_AUC + ETA_G_Qnt + ETA_G_AUC2 +
        ETA_G_AUC_leading + ETA_G_AUC_trailing + ETA_G_AUC_diff_mn + ETA_G_AUC_diff_90th + ETA_G_sd


set.seed(2734, kind = "L'Ecuyer-CMRG")

#vswheat3 = VSURF(form3,  data= data_in, na.action=na.omit,
#                parallel = T, ncores = 30 , clusterType = "FORK" )  # mtry default is # Xs / 3
#
#
#save(vswheat3,file = '../Data/VariableSelection/vswheat_3.RData')




# Load variables selected ---------------------------------------------------------

load('../Data/VariableSelection/vswheat_0.RData')
load('../Data/VariableSelection/vswheat_1.RData')
load('../Data/VariableSelection/vswheat_2.RData')
load('../Data/VariableSelection/vswheat_3.RData')


# get names of selected variables at interpretation phase of variables selection

c(attr(vswheat0$terms,'term.labels')[vswheat0$varselect.interp],
attr(vswheat1$terms,'term.labels')[vswheat1$varselect.interp],
attr(vswheat2$terms,'term.labels')[vswheat2$varselect.interp],
attr(vswheat3$terms,'term.labels')[vswheat3$varselect.interp])


# get names of selected variables at interpretation phase of variables selection

c(attr(vswheat0$terms,'term.labels')[vswheat0$varselect.pred],
attr(vswheat1$terms,'term.labels')[vswheat1$varselect.pred],
attr(vswheat2$terms,'term.labels')[vswheat2$varselect.pred],
attr(vswheat3$terms,'term.labels')[vswheat3$varselect.pred])





