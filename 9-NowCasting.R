
# create prediction model that only uses variables from first 1/2 of growing season
rm(list=ls())

library(ggplot2)
library(reshape)
library(readstata13)
library(plm)
library(dplyr)
library(splines)
library(stargazer)
library(splines)
library(caret)
library(DEoptim)
library(kernlab)
library(RANN)
library(pdp)
library(prediction)
library(doMC)
library(doParallel)
library(mlbench)
library(randomForest)
 
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
  data_in_plm$Fert_Amt_Per_Area = data_in_plm$MAIZEFERT_CHEMICAL_AMT / (data_in_plm$MAIZEFERT_CHEMICAL_AREA+1)
  
  # add lag variables for agss
  data_in_plm$MAIZEOPH_W_lag  = lag(data_in_plm$MAIZEOPH_W, 1)
  data_in_plm$MAIZEIMSEED_lag = lag(data_in_plm$MAIZEIMSEED,1) 
  data_in_plm$MAIZEDAMAGEAREA_P_lag = lag(data_in_plm$MAIZEDAMAGEAREA_P,1)
  data_in_plm$Fert_Amt_Per_Area_lag = lag(data_in_plm$Fert_Amt_Per_Area,1) 
  data_in_plm$MAIZEEXTAREA_lag    = lag(data_in_plm$MAIZEEXTAREA,1)
  
  # remove 2015 for prediction & convert back to data.frames
  data_in_plm_pred = data_in_plm[data_in_plm$Year==2015,]
  data_in_plm = data_in_plm[data_in_plm$Year!=2015,]
  

  # Impute values & convert to data.frame
  set.seed(123)
  #data_in_plm_pred = as.data.frame(predict(preProcess(data_in_plm_pred, method = "medianImpute",k=25),data_in_plm_pred))
  data_in_plm  = as.data.frame(predict(preProcess(data_in_plm, method = "medianImpute",k=25),data_in_plm))
 
  #set up seeds for multicore
  set.seed(123)
  seeds <- vector(mode = "list", length = 16)
  for(i in 1:length(seeds)) seeds[[i]] <- sample.int(1000, length(seeds)-1);seeds[[length(seeds)]]=sample.int(1000, 1)
  
  #set up longitudinal data groups  index = https://topepo.github.io/caret/data-splitting.html
  groups = groupKFold(data_in_plm$EACODE, k = 40)
  
  form1_mz =  as.numeric(MAIZEOPH_W) ~ MAIZEOPH_W_lag +MAIZEIMSEED_lag + MAIZEDAMAGEAREA_P_lag + Fert_Amt_Per_Area_lag +
    MAIZEEXTAREA_lag + G_mx_dates +G_mx + G_mx_Qnt + G_AUC_leading+ 
    PPT_G_AUC_leading +  PPT_G_mx_Qnt+ PPT_G_mx+ elevation + factor(Z_CODE) 
  
  # #train random forest on grouped data https://topepo.github.io/caret/model-training-and-tuning.html
  # registerDoMC(cores = 5)
  # maz.rf_nc<-train(form1_mz,data=data_in_plm,method="rf", 
  #                trControl=trainControl(method="cv",number=3, seeds=seeds,index = groups), #number iterations+1
  #                prox=T,allowParallel=T,tuneGrid = expand.grid(mtry=seq(5,12,1)))
  # save(maz.rf_nc,file = './Outputs4Pred/maz.rf_nowcast.RData')
  load('./Outputs4Pred/maz.rf_nowcast.RData')
  
  registerDoMC(cores = 5)
  maz.rf_nc_oob<-train(form1_mz,data=data_in_plm,method="rf",
                 trControl=trainControl(method="oob", seeds=seeds,index = groups), #number iterations+1
                 prox=T,allowParallel=T,tuneGrid = expand.grid(mtry=seq(6,12,1)))
  save(maz.rf_nc_oob,file = './Outputs4Pred/maz.rf_nowcast_oob.RData')
  load('./Outputs4Pred/maz.rf_nowcast.RData')
  
  # maz.cb_nc<-train(form1_mz,data=data_in_plm,method="cubist",
  #                trControl=trainControl(method="cv",number=3, seeds=seeds,index = groups), #number iterations+1
  #                prox=T,allowParallel=T ,tuneGrid = expand.grid(.committees = seq(10, 100, 10), .neighbors = 0))
  # save(maz.cb_nc,file = './Outputs4Pred/maz.cb_nowcast.RData')
  load('./Outputs4Pred/maz.cb_nowcast.RData')
  
  plot(maz.rf_nc)
  print(maz.rf_nc)
  varImp(maz.rf_nc)
  
  
  form1_mz =  as.numeric(MAIZEOPH_W) ~ MAIZEOPH_W_lag +MAIZEIMSEED_lag + MAIZEDAMAGEAREA_P_lag + Fert_Amt_Per_Area_lag +
    MAIZEEXTAREA_lag + G_mx_dates +G_mx + G_mx_Qnt + G_AUC_leading+ 
    PPT_G_AUC_leading +  PPT_G_mx_Qnt+ PPT_G_mx+ elevation + Z_CODE 
  
  
  varImpPlot(randomForest(form1_mz,data=data_in_plm,mtry=8),main='')
  
    
  
  # save(maz.rf,file = './Outputs4Pred/maz.rf.MAIZEDAMAGEAREA_P>.3_zone.RData')
  # load('./Outputs4Pred/maz.rf.MAIZEDAMAGEAREA_P<.3_zone.RData')
  data_in_plm_pred_mf = na.omit(data_in_plm_pred[,names(data_in_plm_pred) %in% all.vars(form1_mz)])
  data_in_plm_pred_mf$pred = predict(maz.rf_nc_oob,data_in_plm_pred_mf)
  data_in_plm_pred_mf$error = data_in_plm_pred_mf$MAIZEOPH_W-data_in_plm_pred_mf$pred 
  data_in_plm_pred_mf$p_error = (data_in_plm_pred_mf$MAIZEOPH_W-data_in_plm_pred_mf$pred)/data_in_plm_pred_mf$MAIZEOPH_W *100
  
  ggplot(data = data_in_plm_pred_mf, aes(x=p_error))+geom_histogram(bins = 300)+ xlab('Percentage Error')+
      coord_cartesian(xlim=c(-100, 100))
   
  
  # get accuracy measures
  cm_maz = confusionMatrix(data =  pp.test$pred, reference = (pp.test$MAIZEDAMAGEAREA_P<0.3), mode = "prec_recall")
  
  
  
  # Compare Random forest and SVM
  # maz.svm_nc<-train(form1_mz,data=data_in_plm,method="svmRadial",metric ='RMSE',
  #                trControl=trainControl(method="cv",number=3, seeds=seeds,index = groups), #number iterations+1
  #                prox=T,allowParallel=T )
  # save(maz.svm_nc,file = './Outputs4Pred/maz.svm_nowcast.RData')
  # 
  # 
  results <- resamples(list(randomforest=maz.rf_nc, rf_oob = maz.rf_nc_oob ,Cubist= maz.cb_nc))  #https://machinelearningmastery.com/compare-models-and-select-the-best-using-the-caret-r-package/
  # summarize the distributions
  summary(results)
  # boxplots of results
  bwplot(results)
 
  # 




# DONT USE PANEL TOO HARD TO DO OUt OF SAMPLE PREDICTIONS

 # load variable selection results
 # load('./Writeup/IFPRI 2016 Report Ethiopia/WriteupData/VariableSelection/vsmaize_4.RData')
 # 
 # form_1_maz_z_NC = paste(c(attr(vsmaize4$terms,'term.labels')[vsmaize4$varselect.pred],'lag(data_in_plm$MAIZEOPH_W,1)',
 #                        "factor(Z_CODE)"), collapse='+')
 # form_1_maz_z_NC = as.formula(paste('MAIZEOPH_W ~',form_1_maz_z_NC,sep=' '))
#
#
# agss_lag_maz_z_NC = MAIZEOPH_W ~ lag(data_in_plm$MAIZEOPH_W, 1)   +G_mx + G_mx_Qnt + G_AUC_leading+
#   lag(MAIZEIMSEED,1) + lag(MAIZEDAMAGEAREA_P,1)  +  lag(Fert_Amt_Per_Area,1) +
#   lag(MAIZEEXTAREA,1) + PPT_G_mx_Qnt+
#  bs(elevation,3) + factor(Z_CODE)
# agss_lag_maz_z_NC <- plm(agss_lag_maz_z_NC, data = data_in_plm, model = "random")
#
# # # store for text pasting
# # sum.maz.re_NC = summary(agss_lag_maz_z)
# # coeff.maz.re_NC = sum.maz.re3[1]$coefficients
# # sum.maz.re_NC
#
# agss_lag_maz_z2_NC = MAIZEOPH_W ~   lag(data_in_plm$MAIZEOPH_W, 1)   +G_mx + G_mx_Qnt + G_AUC_leading+
#   lag(MAIZEIMSEED,1) + lag(MAIZEDAMAGEAREA_P,1)  +  lag(Fert_Amt_Per_Area,1) +
#   lag(MAIZEEXTAREA,1) +
#   bs(elevation,3) + factor(Z_CODE)
# agss_lag_maz_z2_NC <- plm(agss_lag_maz_z2_NC, data = data_in_plm, model = "random")
#
#
# stargazer(agss_lag_maz_z_NC, agss_lag_maz_z2_NC,
#           title="Panel RE regression", type="text",
#           column.labels=c("Full", "Final"),
#           df=FALSE, digits=4,omit = "Z_CODE",omit.labels = 'Zone Dummy',no.space=T)
#
#
# prediction(agss_lag_maz_z2_NC,data = data_in_plm_pred)











# ideally would reestimate all stats from: 
# load('./Documents/IFPRI_Ethiopia_Drought_2016/Data/Processed Panel/ExtractRaw/NDVI_Poly_Ext_sub_agss_V4.RData')
# load('./Documents/IFPRI_Ethiopia_Drought_2016/Data/Outputs/Poly_PPT_Ext_sub_V4.RData')


