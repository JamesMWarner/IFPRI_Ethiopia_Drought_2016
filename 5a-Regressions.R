

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


 [1] "Y_COORD"                    "X_COORD"
 [3] "ZONECODE"                   "WHEATAREA"
 [5] "REGIONCODE"                 "Year"
 [7] "WHEATNIMSEED"               "WHEATFERT_CHEMICAL_AMT"
 [9] "WHEATSEED2AREA"             "WHEATNIMSEED_P"
[11] "WHEATFERT_CHEMICAL_AREA"    "WHEATDAMAGEAREA_P"
[13] "WHEATFERT_CHEMICAL_AMT_P"   "WHEATSERRAREA"
[15] "WHEATDAMAGEAREA"            "WHEATMERR4AREA"
[17] "WHEATFERT_CHEMICAL_AREA_P"  "WHEATDAMAGE_WEATHER_AREA_P"
[19] "WHEATMERR1AREA"             "WHEATEXTAREA"
[21] "WHEATDAMAGE_WEATHER_AREA"   "WHEATFERT_NATURAL_AREA"
[23] "WHEATIMSEED"                "WHEATFERT_NATURAL_AREA_P"
[25] "WHEATSEED1AREA"             "WHEATIMSEED_P"
[27] "WHEATSERRAREA_P"            "WHEATSEED2AREA_P"
[29] "WHEATSEED1AREA_P"           "WHEATMERR4AREA_P"
[31] "WHEATDAMAGE_PESTS_AREA"     "WHEATEXTAREA_P"
[33] "WHEATMERR1AREA_P"           "WHEATDAMAGE_DROUGHT_AREA_P"
[35] "WHEATMERR5AREA"             "WHEATDAMAGE_DROUGHT_AREA"
[37] "WHEATMERR5AREA_P"           "G_AUC_Qnt"
[39] "G_mx"                       "A_max"
[41] "A_Qnt"                      "A_max_Qnt"
[43] "A_mn"                       "G_AUC"
[45] "G_AUC2"                     "G_Qnt"
[47] "A_AUC"                      "G_mn"
[49] "G_mx_Qnt"                   "T_G_Qnt"
[51] "G_min"                      "A_AUC_Qnt"
[53] "ETA_G_AUC_diff_mn"          "PET_G_mn"
[55] "PET_G_sd"                   "PET_A_max"
[57] "PET_A_sd"                   "PET_G_AUC_trailing"
[59] "ETA_G_AUC_diff_90th"        "ETA_G_mx"
[61] "ETA_G_Qnt"                  "PET_A_AUC"
[63] "ETA_G_sd"                   "PET_A_mn"
[65] "ETA_A_sd"                   "ETA_G_AUC2"





# get names of selected variables at interpretation phase of variables selection

c(attr(vswheat0$terms,'term.labels')[vswheat0$varselect.pred],
attr(vswheat1$terms,'term.labels')[vswheat1$varselect.pred],
attr(vswheat2$terms,'term.labels')[vswheat2$varselect.pred],
attr(vswheat3$terms,'term.labels')[vswheat3$varselect.pred])


 [1] "WHEATNIMSEED"               "WHEATFERT_CHEMICAL_AMT"
 [3] "WHEATSEED2AREA"             "WHEATNIMSEED_P"
 [5] "WHEATFERT_CHEMICAL_AREA"    "WHEATDAMAGEAREA_P"
 [7] "WHEATFERT_CHEMICAL_AMT_P"   "WHEATSERRAREA"
 [9] "WHEATEXTAREA"               "WHEATFERT_NATURAL_AREA"
[11] "WHEATIMSEED"                "WHEATMERR4AREA_P"
[13] "WHEATDAMAGE_PESTS_AREA"     "WHEATEXTAREA_P"
[15] "WHEATMERR1AREA_P"           "WHEATDAMAGE_DROUGHT_AREA_P"
[17] "WHEATMERR5AREA_P"           "G_AUC_Qnt"
[19] "A_max_Qnt"                  "A_mn"
[21] "G_AUC"                      "G_mx_Qnt"
[23] "T_G_Qnt"                    "A_AUC_Qnt"
[25] "ETA_G_AUC_diff_mn"          "PET_G_mn"
[27] "PET_G_sd"                   "PET_A_max"
[29] "PET_A_sd"



