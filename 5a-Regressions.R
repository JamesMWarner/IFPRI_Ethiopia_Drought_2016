
library(readstata13)
library(VSURF)
#https://journal.r-project.org/archive/2015-2/genuer-poggi-tuleaumalot.pdf

data_in = read.dta13("C:\\Users\\mmann\\Dropbox\\Ethiopia_Drought\\Drought_Study\\AgSS_Data1_2010_16_Cleaned\\AgSS_2010_15_Compiled_panel_merged_clean_PCA_v3.dta")
data_in = data_in[,!(names(data_in) %in% c("_merge") ) ]
#data_in = data_in[,1:dim(data_in)[2]]
set.seed(2734, kind = "L'Ecuyer-CMRG")

paste( names(data_in)[grepl('WHEAT',names(data_in))] , collapse=' + ')


vswheat = VSURF(WHEATOPH_W ~ Year+REGIONCODE+ZONECODE+X_COORD+Y_COORD+WHEAT + WHEATAREA + WHEATOUTPUT + WHEATEXTAREA + WHEATIRRGAREA + WHEATSERRAREA + 
                  WHEATMERR1AREA + WHEATMERR2AREA + WHEATMERR3AREA + WHEATMERR4AREA + WHEATMERR5AREA + WHEATSEED1AREA + WHEATSEED2AREA + WHEATIMSEED + 
                  WHEATNIMSEED + WHEATDAMAGEAREA + WHEATDAMAGE_WEATHER_AREA + WHEATDAMAGE_PESTS_AREA + WHEATDAMAGE_MANAGE_AREA + WHEATDAMAGE_OTHER_AREA + 
                  WHEATDAMAGE_DROUGHT_AREA + WHEATDAMAGE_DROUGHT_DUM + WHEATFERT_NATURAL_AREA + WHEATFERT_CHEMICAL_AREA + WHEATFERT_CHEMICAL_AMT + 
                  WHEATEXTAREA_P + WHEATIRRGAREA_P + WHEATSERRAREA_P + WHEATMERR1AREA_P + WHEATMERR2AREA_P + WHEATMERR3AREA_P + WHEATMERR4AREA_P + 
                  WHEATMERR5AREA_P + WHEATSEED1AREA_P + WHEATSEED2AREA_P + WHEATIMSEED_P + WHEATNIMSEED_P + WHEATDAMAGEAREA_P + WHEATDAMAGE_WEATHER_AREA_P + 
                  WHEATDAMAGE_PESTS_AREA_P + WHEATDAMAGE_MANAGE_AREA_P + WHEATDAMAGE_OTHER_AREA_P + WHEATDAMAGE_DROUGHT_AREA_P + WHEATFERT_NATURAL_AREA_P + 
                  WHEATFERT_CHEMICAL_AREA_P + WHEATFERT_CHEMICAL_AMT_P, data= data_in, na.action=na.omit,parallel = T, ncores = 4 ,mtry = round(dim(data_in)[2]/3) )  # mtry default is # Xs / 3



