require(plyr)         
require(ggplot2)      
require(scales)       
require(gtable)       
require(sqldf)
require(Metrics)
require(plotly)
require(data.table)
require(gridExtra)
require(ggrepel)

# ----------------------------------------------------------------------------------------------------------------------------
# Other functions
# ----------------------------------------------------------------------------------------------------------------------------
nvl <- function(x,y) {ifelse(is.na(x),y,x)}
h <- function(x){pmin(0,x)}

source('/home/garciaj/Car_NB_valuation/Market_models/clean_functions.R')
source('/home/garciaj/Home/Conversion_model_propDB/201802/UDF_list.R')

# ----------------------------------------------------------------------------------------------------------------------------
## Load Hive Data
# ----------------------------------------------------------------------------------------------------------------------------
load("/home/garciaj/Car_NB_valuation/Market_models/d_split_enriched.RData")

d = b_propDB

colnames(d)

# ----------------------------------------------------------------------------------------------------------------------------
# Column types and cleaning
# ----------------------------------------------------------------------------------------------------------------------------
d = function_clean_market_compete(d, with_propDB = TRUE)

# ----------------------------------------------------------------------------------------------------------------------------
# New columns - database factors
# ----------------------------------------------------------------------------------------------------------------------------
d$Day_Of_Birth = as.integer(format(d$Cust_DOB_1,format="%d"))
d$Perilv3 = ifelse(d$Day_Of_Birth>=30 & d$Pol_Dt_Quote >= '2017-09-28' 
                   & d$Pol_Dt_Incept >= '2017-10-05',1,
                   ifelse(d$Day_Of_Birth>=28 & d$Pol_Dt_Quote >= '2017-10-23' 
                          & d$Pol_Dt_Incept >= '2017-10-27',1,
                          ifelse(d$Day_Of_Birth>=22 & d$Pol_Dt_Quote >= '2018-01-04' 
                                 & d$Pol_Dt_Incept >= '2018-01-08',1,
                                 ifelse(d$Pol_Dt_Quote >= '2018-02-06' 
                                        & d$Pol_Dt_Incept >= '2018-02-10',1,0))))

d$DD_Flag = ifelse(d$Bhvr_DD_DUQ == "Y", 1, 0)

# ----------------------------------------------------------------------------------------------------------------------------
# New columns - database factors against propDB factors
# ----------------------------------------------------------------------------------------------------------------------------

d$ratio_yearregistration_propdb = d$postcode_avg_yearofregistration/d$Veh_Reg_Yr

d$diff_actual_diesel_propdb = ifelse(d$Veh_Fuel == "Diesel",1,0) - d$fueltype_diesel_proportion
d$diff_actual_eletric_propdb = ifelse(d$Veh_Fuel == "Electric",1,0) - d$fueltype_electric_proportion
d$diff_actual_petrol_propdb = ifelse(d$Veh_Fuel == "Petrol",1,0) - d$fueltype_petrol_proportion

d$diff_dd_propDB_dd_no = d$DD_Flag - d$dd_duq_annual_proportion
d$diff_dd_propDB_dd_yes = d$DD_Flag - d$dd_duq_monthly_proportion

d$ratio_creditscore_actual_propDB = d$postcode_avg_creditscore/d$DE_Credit_Score

# ----------------------------------------------------------------------------------------------------------------------------
# Subset
# ----------------------------------------------------------------------------------------------------------------------------
d <- d[!is.na(d$Prem_Comm),]
d <- d[which(d$Prem_Gross<10000),]
d <- d[which(!is.na(d$Annual_Premium_Top5)),]
d <- d[which(!is.na(d$Annual_Premium_6to10)),]
d <- d[which(d$Bhvr_Days_Seen_Quote>=0),]
d <- d[which(d$Perilv3==1),]

# ----------------------------------------------------------------------------------------------------------------------------
# Response
# ----------------------------------------------------------------------------------------------------------------------------
# plot(density((d$Direct_Premium/d$Annual_Premium_Top5)))

# We decide to transform the response variable with a log transformation to make it more normally distributed.
# We need to ensure the models capture this transformed response, AND, the original response variable
d$RESPONSE = log(d$Direct_Premium/d$Annual_Premium_Top5)
summary(d$RESPONSE)

plot(density(log(d$Direct_Premium/d$Annual_Premium_Top5)))


na_count_d_extract = sapply(d, function(y) sum(length(which(is.na(y)))))
na_count_d_extract = data.frame(na_count_d_extract)
na_count_d_extract

d = na.omit(d)

# ----------------------------------------------------------------------------------------------------------------------------
# Preparing data for h2o
# ----------------------------------------------------------------------------------------------------------------------------
d1 = function_data_frame_for_h2o(d)
head(d1)

# After having created multiple models, we keep only the 20 most important variables
final_cols = c('log_Prem_Net','Cust_Age_R','Veh_Capacity','Cust_Age_1','DE_Credit_Score','Veh_Drv_Restrict'
               ,'Cust_Lic_Len_R','log_postcode_avg_annual_premium_top5','Area_Grp_Adv_New','log_distance_to_booths'
               ,'Pol_NCD_Yrs','postcode_avg_premier_ratio_top5','log_Veh_Own_Yrs','Cust_Lic_Len_1','DE_ID_Score'
               ,'diff_dd_propDB_dd_yes','Pol_CUE_Score','Veh_Age_Yrs','log_postcode_avg_annual_premium_6to10'
               ,'Clm_Num_L5Y','log_postcode_avg_annual_premium_top10','Veh_Own_Mnths','Veh_Trans','ratio_creditscore_actual_propDB'
               ,'log_Veh_Mile_Total','Pol_Dt_Quote','RESPONSE')
d1 = d1[,final_cols]



# ----------------------------------------------------------------------------------------------------------------------------
# Train/Test
# ----------------------------------------------------------------------------------------------------------------------------
max(d1$Pol_Dt_Quote)
min(d1$Pol_Dt_Quote)

b = subset(d, Pol_Dt_Quote >= '2018-03-01' & Pol_Dt_Quote < '2018-03-20') # 2M rows
t = subset(d, Pol_Dt_Quote >= '2018-03-20' & Pol_Dt_Quote < '2018-04-02') # 931k rows
v = d[which(difftime(max(d$Pol_Dt_Quote),d$Pol_Dt_Quote,units="days")<10),] # 345k rows

b1 = subset(d1, Pol_Dt_Quote >= '2018-03-01' & Pol_Dt_Quote < '2018-03-20') # 2M rows
#b1$Pol_Dt_Quote = NULL
t1 = subset(d1, Pol_Dt_Quote >= '2018-03-20' & Pol_Dt_Quote < '2018-04-02') # 931k rows
#t1$Pol_Dt_Quote = NULL
v1 = d1[which(difftime(max(d1$Pol_Dt_Quote),d1$Pol_Dt_Quote,units="days")<10),] # 345k rows
#v1$Pol_Dt_Quote = NULL

# ----------------------------------------------------------------------------------------------------------------------------
# Loading previous market model
# ----------------------------------------------------------------------------------------------------------------------------
# load('/home/garciaj/Car_NB_valuation/Market_models/Conversion/4.6.0/compete_v4_6_0.R')
# coef(compete_v4_6_0)
# cat(gsub("+\n",")+\n",gsub("\\n","+\n(",format(compete_v4_6_0, style="pmax")), "\n"))

for(i in 1:4){
  
  if(i == 1){
    attach(b)
    print('Calculating predictions of previous mars model for train dataset')
  }
  
  if(i == 2){
    attach(t)    
    print('Calculating predictions of previous mars model for test dataset')
  }
  
  if(i == 3){
    attach(v)    
    print('Calculating predictions of previous mars model for validation dataset')
  }
  
  if(i == 4){
    attach(d)    
    print('Calculating predictions of previous mars model for full dataset')
  }
  
  mars_pred=1.151686+
    (-0.154147*ifelse(Veh_Use_Class=="02",1,0))+
    (+0.06200969*ifelse(Veh_Drv_Restrict=="2",1,0))+
    (+0.08524436*ifelse(Veh_Drv_Restrict=="W",1,0))+
    (-1.176136*pmax(0,2.975836-log10(1+Prem_Net)))+
    (+1.617636*pmax(0,log10(1+Prem_Net)-2.975836))+
    (-0.06131305*pmax(0,pmin(Cust_Age_R,80)-24))+
    (-0.08027337*pmax(0,38-pmin(Cust_Age_R,80)))+
    (+0.06746436*pmax(0,pmin(Cust_Age_R,80)-38))+
    (-0.02035517*pmax(0,7-pmin(Cust_Lic_Len_1,20)))+
    (-0.09692891*pmax(0,pmin(Cust_Lic_Len_1,20)-7))+
    (-0.09798229*pmax(0,1-Pol_NCD_Yrs))+
    (+0.00939054*pmax(0,Pol_NCD_Yrs-1))+
    (+0.06389001*pmax(0,I(DE_Credit_Score/10)-3.4))+
    (+0.05743311*pmax(0,8.7-I(DE_Credit_Score/10)))+
    (-0.04719136*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (-0.01850971*pmax(0,I(DE_Credit_Score/10)-10.2))+
    (+0.01985356*pmax(0,I(DE_Credit_Score/10)-11.7))+
    (-0.02478914*pmax(0,I(DE_Credit_Score/10)-12.9))+
    (+0.01893885*pmax(0,I(DE_Credit_Score/10)-13.9))+
    (-0.002901697*pmax(0,Area_Grp_Adv_New-92))+
    (-0.002719129*pmax(0,197-Area_Grp_Adv_New))+
    (+0.002016306*pmax(0,Area_Grp_Adv_New-197))+
    (-0.0318807*pmax(0,2-pmin(Veh_Own_Yrs,10)))+
    (+0.01112769*pmax(0,pmin(Veh_Own_Yrs,10)-2))+
    (+0.01070325*pmax(0,3.822822-log10(1+pmin(Veh_Mile_Total,30000))))+
    (-0.3015776*pmax(0,log10(1+pmin(Veh_Mile_Total,30000))-3.822822))+
    (+0.2325008*pmax(0,2.143-I(Veh_Capacity/1000)))+
    (-0.1297379*pmax(0,I(Veh_Capacity/1000)-2.143))+
    (+0.05016052*pmax(0,ifelse(Pol_Homeowner=="Y",1,0)-0))+
    (+0.05557634*pmax(0,1-Conv_Num_L5Y))+
    (+0.003920731*pmax(0,Conv_Num_L5Y-1))+
    (-0.0001835082*pmax(0,200-Pol_XS_Vol))+
    (-8.371153e-06*pmax(0,Pol_XS_Vol-200))+
    (-0.1366319*pmax(0,2.975836-log10(1+Prem_Net))*ifelse(Veh_Drv_Restrict=="1",1,0))+
    (-0.005198148*pmax(0,38-pmin(Cust_Age_R,80))*ifelse(Veh_Drv_Restrict=="5",1,0))+
    (+0.00678667*pmax(0,Pol_NCD_Yrs-1)*ifelse(Veh_Drv_Restrict=="1",1,0))+
    (-0.04939582*pmax(0,1-Pol_NCD_Yrs)*DD_Flag)+
    (-0.0002093603*pmax(0,Area_Grp_Adv_New-197)*DD_Flag)+
    (+0.001326509*pmax(0,2.975836-log10(1+Prem_Net))*pmax(0,pmin(Cust_Age_R,80)-21))+
    (+0.06224157*pmax(0,2.975836-log10(1+Prem_Net))*pmax(0,21-pmin(Cust_Age_R,80)))+
    (-0.01987927*pmax(0,log10(1+Prem_Net)-2.975836)*pmax(0,pmin(Cust_Lic_Len_1,20)-2))+
    (-0.09831142*pmax(0,log10(1+Prem_Net)-2.975836)*pmax(0,2-pmin(Cust_Lic_Len_1,20)))+
    (-0.0008113278*pmax(0,1-pmin(Bhvr_Days_Quote_Incept,30))*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (-0.0004723445*pmax(0,2-pmin(Bhvr_Days_Quote_Incept,30))*pmax(0,I(DE_Credit_Score/10)-3.4))+
    (-5.559973e-05*pmax(0,pmin(Bhvr_Days_Quote_Incept,30)-2)*pmax(0,I(DE_Credit_Score/10)-3.4))+
    (-2.692479e-06*pmax(0,pmin(Bhvr_Days_Seen_Incept,30)-3)*pmax(0,I(DE_Credit_Score/10)-3.4))+
    (+0.001846486*pmax(0,25-pmin(Cust_Age_1,80))*pmax(0,38-pmin(Cust_Age_R,80)))+
    (-0.0001782432*pmax(0,pmin(Cust_Age_1,80)-25)*pmax(0,38-pmin(Cust_Age_R,80)))+
    (+0.0001807098*pmax(0,66-pmin(Cust_Age_1,80))*pmax(0,pmin(Cust_Age_R,80)-38))+
    (-0.0004309611*pmax(0,pmin(Cust_Age_1,80)-66)*pmax(0,pmin(Cust_Age_R,80)-38))+
    (-8.476501e-05*pmax(0,72-pmin(Cust_Age_1,80))*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (-0.000544303*pmax(0,pmin(Cust_Age_1,80)-72)*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (+0.08210665*pmax(0,21-pmin(Cust_Age_1,80))*pmax(0,2.143-I(Veh_Capacity/1000)))+
    (-0.001372517*pmax(0,pmin(Cust_Age_1,80)-21)*pmax(0,2.143-I(Veh_Capacity/1000)))+
    (+0.009954947*pmax(0,pmin(Cust_Age_R,80)-20)*pmax(0,pmin(Cust_Lic_Len_1,20)-7))+
    (+0.00769883*pmax(0,30-pmin(Cust_Age_R,80))*pmax(0,pmin(Cust_Lic_Len_1,20)-7))+
    (-0.01003456*pmax(0,pmin(Cust_Age_R,80)-30)*pmax(0,pmin(Cust_Lic_Len_1,20)-7))+
    (+0.0006290613*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,pmin(Cust_Lic_Len_1,20)-2))+
    (-0.002123041*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,2-pmin(Cust_Lic_Len_1,20)))+
    (-5.410686e-05*pmax(0,25-pmin(Cust_Age_R,80))*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (-0.0001172066*pmax(0,pmin(Cust_Age_R,80)-25)*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (+5.629959e-05*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,Veh_Grp_Adv_New-4))+
    (+0.001038376*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,4-Veh_Grp_Adv_New))+
    (+0.0004812979*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,Clm_Num_F_L5Y-1))+
    (+0.005522522*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,1-Clm_Num_F_L5Y))+
    (-0.007001311*pmax(0,1-Pol_NCD_Yrs)*pmax(0,pmin(Veh_Age_Yrs,15)-1))+
    (+0.09284484*pmax(0,1-Pol_NCD_Yrs)*pmax(0,1-pmin(Veh_Age_Yrs,15)))+
    (+8.82108e-05*pmax(0,Pol_NCD_Yrs-1)*pmax(0,pmin(Veh_Age_Yrs,15)-8))+
    (-0.0002153296*pmax(0,Pol_NCD_Yrs-1)*pmax(0,8-pmin(Veh_Age_Yrs,15)))+
    (-0.01965309*pmax(0,1-Pol_NCD_Yrs)*pmax(0,Clm_Num_F_L5Y-1))+
    (+0.08237553*pmax(0,1-Pol_NCD_Yrs)*pmax(0,1-Clm_Num_F_L5Y))+
    (-0.000147407*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,Veh_Grp_Adv_New-94))+
    (+3.655883e-05*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,94-Veh_Grp_Adv_New))+
    (+0.0005176078*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,pmin(Veh_Age_Yrs,15)-7))+
    (+0.0001530631*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,7-pmin(Veh_Age_Yrs,15)))+
    (-0.006771993*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,log10(1+pmin(Veh_Value,50000))-4.088136))+
    (+8.826158e-05*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,4.088136-log10(1+pmin(Veh_Value,50000))))+
    (+0.004683997*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,I(Veh_Capacity/1000)-2.993))+
    (-0.002043025*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,2.993-I(Veh_Capacity/1000)))+
    (+6.326333e-05*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,Veh_Num_Oth_Veh-2))+
    (-0.0009096281*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,2-Veh_Num_Oth_Veh))+
    (+6.239045e-06*pmax(0,2-pmin(Veh_Own_Yrs,10))*pmax(0,Pol_XS_Vol-200))+
    (-1.149244e-05*pmax(0,2-pmin(Veh_Own_Yrs,10))*pmax(0,200-Pol_XS_Vol))+
    (+0.0004431438*pmax(0,25-pmin(Cust_Age_1,80))*pmax(0,38-pmin(Cust_Age_R,80))*ifelse(Veh_Use_Class=="04",1,0))+
    (-3.096723e-05*pmax(0,72-pmin(Cust_Age_1,80))*pmax(0,I(DE_Credit_Score/10)-8.7)*ifelse(Veh_Loc_Night=="1",1,0))+
    (-0.0001237739*pmax(0,2.188338-log10(1+Prem_Net))*pmax(0,pmin(Bhvr_Days_Quote_Incept,30)-1)*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (+5.281658e-05*pmax(0,log10(1+Prem_Net)-2.188338)*pmax(0,pmin(Bhvr_Days_Quote_Incept,30)-1)*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (+0.0003725745*pmax(0,2.250103-log10(1+Prem_Net))*pmax(0,72-pmin(Cust_Age_1,80))*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (+3.77926e-05*pmax(0,log10(1+Prem_Net)-2.250103)*pmax(0,72-pmin(Cust_Age_1,80))*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (+0.03728539*pmax(0,2.975836-log10(1+Prem_Net))*pmax(0,21-pmin(Cust_Age_R,80))*pmax(0,Pol_NCD_Yrs-1))+
    (+0.4813318*pmax(0,2.975836-log10(1+Prem_Net))*pmax(0,21-pmin(Cust_Age_R,80))*pmax(0,1-Pol_NCD_Yrs))+
    (+0.0007455432*pmax(0,2.420912-log10(1+Prem_Net))*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,Veh_Grp_Adv_New-4))+
    (-0.0009245639*pmax(0,log10(1+Prem_Net)-2.420912)*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,Veh_Grp_Adv_New-4))+
    (+0.0009423346*pmax(0,log10(1+Prem_Net)-2.476266)*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,Veh_Grp_Adv_New-4))+
    (+0.01112636*pmax(0,3.402421-log10(1+Prem_Net))*pmax(0,1-Pol_NCD_Yrs)*pmax(0,pmin(Veh_Age_Yrs,15)-1))+
    (+0.004238479*pmax(0,log10(1+Prem_Net)-3.402421)*pmax(0,1-Pol_NCD_Yrs)*pmax(0,pmin(Veh_Age_Yrs,15)-1))+
    (+5.153924e-06*pmax(0,pmin(Bhvr_Days_Quote_Incept,30)-1)*pmax(0,pmin(Cust_Lic_Len_1,20)-1)*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (-4.741614e-05*pmax(0,pmin(Bhvr_Days_Quote_Incept,30)-1)*pmax(0,1-pmin(Cust_Lic_Len_1,20))*pmax(0,I(DE_Credit_Score/10)-8.7))+
    (-3.356429e-08*pmax(0,pmin(Bhvr_Days_Quote_Incept,30)-1)*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,Area_Grp_Adv_New-126))+
    (+6.158787e-07*pmax(0,pmin(Bhvr_Days_Quote_Incept,30)-1)*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,126-Area_Grp_Adv_New))+
    (-9.0151e-06*pmax(0,pmin(Bhvr_Days_Quote_Incept,30)-1)*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,I(Veh_Capacity/1000)-0.499))+
    (-0.0007902333*pmax(0,pmin(Bhvr_Days_Quote_Incept,30)-1)*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,0.499-I(Veh_Capacity/1000)))+
    (+6.495998e-07*pmax(0,2-pmin(Bhvr_Days_Seen_Incept,30))*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,94-Veh_Grp_Adv_New))+
    (-2.143741e-07*pmax(0,pmin(Bhvr_Days_Seen_Incept,30)-2)*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,94-Veh_Grp_Adv_New))+
    (-0.0007499159*pmax(0,25-pmin(Cust_Age_1,80))*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,pmin(Veh_Age_Yrs,15)-14))+
    (-3.845879e-05*pmax(0,25-pmin(Cust_Age_1,80))*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,14-pmin(Veh_Age_Yrs,15)))+
    (+0.002939319*pmax(0,21-pmin(Cust_Age_1,80))*pmax(0,1-Pol_NCD_Yrs)*pmax(0,pmin(Veh_Age_Yrs,15)-1))+
    (-2.800656e-05*pmax(0,pmin(Cust_Age_1,80)-21)*pmax(0,1-Pol_NCD_Yrs)*pmax(0,pmin(Veh_Age_Yrs,15)-1))+
    (-6.634096e-07*pmax(0,72-pmin(Cust_Age_1,80))*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,Area_Grp_Adv_New-169))+
    (+6.33398e-07*pmax(0,72-pmin(Cust_Age_1,80))*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,Area_Grp_Adv_New-196))+
    (-3.364169e-05*pmax(0,20-pmin(Cust_Age_1,80))*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,94-Veh_Grp_Adv_New))+
    (-8.137229e-08*pmax(0,pmin(Cust_Age_1,80)-20)*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,94-Veh_Grp_Adv_New))+
    (+6.894471e-08*pmax(0,72-pmin(Cust_Age_1,80))*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,Pol_XS_Vol-50))+
    (+1.349646e-07*pmax(0,72-pmin(Cust_Age_1,80))*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,50-Pol_XS_Vol))+
    (+0.004606539*pmax(0,pmin(Cust_Age_R,80)-20)*pmax(0,pmin(Cust_Lic_Len_1,20)-7)*pmax(0,log10(1+pmin(Veh_Value,50000))-4.598791))+
    (-1.104029e-05*pmax(0,19-pmin(Cust_Age_R,80))*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,94-Veh_Grp_Adv_New))+
    (+2.384596e-07*pmax(0,pmin(Cust_Age_R,80)-19)*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,94-Veh_Grp_Adv_New))+
    (-0.001277893*pmax(0,25-pmin(Cust_Age_R,80))*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,I(Veh_Capacity/1000)-1.297))+
    (-0.001203751*pmax(0,25-pmin(Cust_Age_R,80))*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,1.297-I(Veh_Capacity/1000)))+
    (-1.403285e-07*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,Area_Grp_Adv_New-178)*pmax(0,Veh_Grp_Adv_New-4))+
    (-2.189661e-07*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,178-Area_Grp_Adv_New)*pmax(0,Veh_Grp_Adv_New-4))+
    (-3.933333e-06*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,Veh_Grp_Adv_New-4)*pmax(0,pmin(Veh_Age_Yrs,15)-1))+
    (+4.941466e-05*pmax(0,38-pmin(Cust_Age_R,80))*pmax(0,Veh_Grp_Adv_New-4)*pmax(0,1-pmin(Veh_Age_Yrs,15)))+
    (-2.110467e-05*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,pmin(Veh_Age_Yrs,15)-7)*pmax(0,pmin(Veh_Own_Yrs,10)-1))+
    (-0.0002758704*pmax(0,I(DE_Credit_Score/10)-8.7)*pmax(0,pmin(Veh_Age_Yrs,15)-7)*pmax(0,1-pmin(Veh_Own_Yrs,10)))
  
  if(i == 1){
    b1_mars = mars_pred
    head(b1_mars)
    detach(b)    
  }
  
  if(i == 2){
    t1_mars = mars_pred
    head(t1_mars)
    detach(t)    
  }  
  
  if(i == 3){
    v1_mars = mars_pred
    head(v1_mars)
    detach(v)    
  }  
  
  if(i == 4){
    d1_mars = mars_pred
    head(d1_mars)
    detach(d)    
  } 
}

head(b1_mars)
head(t1_mars)
head(v1_mars)

mse(b$RESPONSE,b1_mars) #0.04168323
mse(t$RESPONSE,t1_mars) #0.04562511
mse(v$RESPONSE,v1_mars) #0.0485194

nrow(d[which(is.na(b1_mars)),])
nrow(d[which(is.na(t1_mars)),])
nrow(d[which(is.na(v1_mars)),])

# ----------------------------------------------------------------------------------------------------------------------------
# Preparing data for h2o
# ----------------------------------------------------------------------------------------------------------------------------

require(h2o)
h2o.init()

d_with_propDB_h2o.hex = as.h2o(d1)
b_with_propDB_h2o.hex = as.h2o(b1)
t_with_propDB_h2o.hex = as.h2o(t1)
v_with_propDB_h2o.hex = as.h2o(v1)


# ----------------------------------------------------------------------------------------------------------------------------
# Preparing data for h2o
# ----------------------------------------------------------------------------------------------------------------------------

hyper_params = list(max_depth = c(4)
                    , ntrees = c(600,700)
                    , min_rows = c(1000)
)

y_col = grep("RESPONSE", colnames(b_with_propDB_h2o.hex))
x_col = seq(1,ncol(b_with_propDB_h2o.hex),1)
x_col = x_col[!x_col == y_col] 

grid_title = "jpg_grid_gbm_market_compete_with_propDB_20_most_imp_vars"

gbm_grid_20_most_imp_vars = h2o.grid(
  
  ## hyper parameters
  hyper_params = hyper_params
  
  ## which algorithm to run
  , algorithm = "gbm"
  
  ## identifier for the grid, to later retrieve it
  , grid_id = grid_title
  
  ## standard model parameters
  , x = x_col
  , y = y_col
  , training_frame = b_with_propDB_h2o.hex
  
  , nfolds = 3
  
  ## fix a random number generator seed for reproducibility
  , seed = 1234)

sorted_Grid_with_propDB = h2o.getGrid(grid_title, sort_by = "MSE", decreasing = FALSE)
sorted_Grid_with_propDB

# ------------------------------------------------------------------------------------------------------
# Picking the model
# ------------------------------------------------------------------------------------------------------
params_model = h2o.getModel(sorted_Grid_with_propDB@model_ids[[which(sorted_Grid_with_propDB@model_ids == "jpg_grid_gbm_market_compete_with_propDB_20_most_imp_vars_model_15")]])

# -------------------------------------------------------------------------------------------------------------------------
# SAVING THE MODEL
# -------------------------------------------------------------------------------------------------------------------------
path = '/home/garciaj/Car_NB_valuation/Market_models/201805_GBM_final_methods'
h2o.saveModel(params_model, path) 

gbm_model1 = h2o.loadModel(paste0(path,"/jpg_grid_gbm_market_compete_with_propDB_20_most_imp_vars_model_5"))
h2o.performance(gbm_model1)

# -------------------------------------------------------------------------------------------------------------------------
# Variable importance
# -------------------------------------------------------------------------------------------------------------------------
var_importance_df = as.data.frame(h2o.varimp(gbm_model1))
first_20_vars = var_importance_df[1:25,]
first_20_vars

# -------------------------------------------------------------------------------------------------------------------------
# Predictions - train
# -------------------------------------------------------------------------------------------------------------------------
predict_gbm_model_b = as.data.frame(h2o.predict(gbm_model1, newdata = b_with_propDB_h2o.hex, type = "response"))
head(predict_gbm_model_b[,1])

df_b = data.frame(actual = b$RESPONSE
                  , old_model = b1_mars
                  , new_model = predict_gbm_model_b[,1]
)

# Differences between both models
df_b$old_model_vs_actual = df_b$actual-df_b$old_model
df_b$new_model_vs_actual = df_b$actual-df_b$new_model

head(df_b)

sum(abs(df_b$old_model_vs_actual)) #314005.5
sum(abs(df_b$new_model_vs_actual)) #291072.7
(sum(abs(df_b$old_model_vs_actual)) - sum(abs(df_b$new_model_vs_actual)))/nrow(df_b) #0.01119881

# Plots
plot(density(df_b$actual))
lines(density(df_b$old_model), col = "red")
lines(density(df_b$new_model), col = "blue")


# -------------------------------------------------------------------------------------------------------------------------
# Predictions - test
# -------------------------------------------------------------------------------------------------------------------------
predict_gbm_model_t = as.data.frame(h2o.predict(gbm_model1, newdata = t_with_propDB_h2o.hex, type = "response"))
head(predict_gbm_model_t[,1])

df_t = data.frame(actual = t$RESPONSE
                  , old_model = t1_mars
                  , new_model = predict_gbm_model_t[,1]
)

# Differences between both models
df_t$old_model_vs_actual = df_t$actual-df_t$old_model
df_t$new_model_vs_actual = df_t$actual-df_t$new_model

head(df_t)

sum(abs(df_t$old_model_vs_actual)) #149838.2
sum(abs(df_t$new_model_vs_actual)) #143751.3
(sum(abs(df_t$old_model_vs_actual)) - sum(abs(df_t$new_model_vs_actual)))/nrow(df_t) #0.006536707

# Plots
plot(density(df_t$actual))
lines(density(df_t$old_model), col = "red")
lines(density(df_t$new_model), col = "blue")

# -------------------------------------------------------------------------------------------------------------------------
# Predictions - validation
# -------------------------------------------------------------------------------------------------------------------------
predict_gbm_model_v = as.data.frame(h2o.predict(gbm_model1, newdata = v_with_propDB_h2o.hex, type = "response"))
head(predict_gbm_model_v[,1])

df_v = data.frame(actual = v$RESPONSE
                  , old_model = v1_mars
                  , new_model = predict_gbm_model_v[,1]
)

# Differences between both models
df_v$old_model_vs_actual = df_v$actual-df_v$old_model
df_v$new_model_vs_actual = df_v$actual-df_v$new_model

head(df_v)

sum(abs(df_v$old_model_vs_actual)) #57342.19
sum(abs(df_v$new_model_vs_actual)) #55276.81
(sum(abs(df_v$old_model_vs_actual)) - sum(abs(df_v$new_model_vs_actual)))/nrow(df_v) #0.005998765

# Plots
plot(density(df_v$actual))
lines(density(df_v$old_model), col = "red")
lines(density(df_v$new_model), col = "blue")


# # -------------------------------------------------------------------------------------------------------------------------
# # Trying to improve predictions by smoothing the errors
# # -------------------------------------------------------------------------------------------------------------------------
# 
# # Create a model to fit the error of the predictions
# # ---------------------------------------------------
# df_b_to_improve = data.frame(actual = b$RESPONSE
#                              , actual_rounded = round_any(b$RESPONSE, 0.01)
#                              , prediction = predict_gbm_model_b[,1]
#                              , error = b$RESPONSE - predict_gbm_model_b[,1]
# )
# head(df_b_to_improve)
# 
# gen = df_b_to_improve %>% group_by(actual_rounded) %>% summarise(avg_error=mean(error),std_err=std.error(error),n=n()) %>% arrange(actual_rounded)
# gen$plus <- gen$avg_error+1.96*gen$std_err
# gen$minus <- gen$avg_error-1.96*gen$std_err
# head(gen)
# 
# g = ggplot(gen,aes(x=actual_rounded,y=avg_error))+geom_line()+geom_ribbon(aes(ymin=minus,ymax=plus), alpha = 0.7)
# g = g + ggtitle("GBM error")+ylab("Prediction error")+xlab("Prediction value")#+coord_cartesian(ylim=c(-600,600))
# g
# 
# error_x <- gen$actual_rounded
# error_y <- gen$avg_error
# n <- gen$n
# 
# plot(density(gen$avg_error))
# plot(x = error_x, y = error_y)
# 
# error_spread <- glm(error_y ~ error_x +
#                       I(error_x^2) +
#                       I(error_x^3) 
#                     , weights = n, family = gaussian())
# summary(error_spread)
# coef(error_spread)
# 
# error_preds = -0.07502785 + (0.39209499*error_x) + (-0.25422801*(error_x^2)) + (0.14811157 *(error_x^3)) 
# xmin = -0.5
# xmax = 1.5
# ymin = -0.5
# ymax = 0.5
# plot(x=error_x, y=error_y, xlim = c(xmin,xmax), ylim = c(ymin,ymax))
# lines(x=error_x, y=error_preds, col=2, xlim = c(xmin,xmax), ylim = c(ymin,ymax))
# lines(x=error_x ,y = error_y-error_preds,col=5)
# abline(h=0,col=1)
# 
# # Input the predictions to the model where the model says errors
# # ---------------------------------------------------------------
# head(df_b)
# 
# df_b$new_model_corrected = df_b$new_model + (-0.07502785 + (0.39209499*df_b$new_model) + (-0.25422801*(df_b$new_model^2)) + (0.14811157 *(df_b$new_model^3))) 
# df_b$new_model_corrected_vs_actual = df_b$actual-df_b$new_model_corrected
# df_b$old_model_vs_new_model_corrected = abs(df_b$old_model_vs_actual) - abs(df_b$new_model_corrected_vs_actual)
# 
# head(df_b)
# 
# mean(abs(df_b$old_model_vs_new_model))
# mean(abs(df_b$old_model_vs_new_model_corrected))
# 
# # Plots
# plot(density(df_b$actual), main = "Actual density of transformed market competitiveness: \n log(Direct Premium / Avg Top 5 Market Premiums)")
# lines(density(df_b$old_model), col = "red")
# lines(density(df_b$new_model), col = "blue")
# lines(density(df_b$new_model_corrected), col = "green")
# legend("topright", legend=c("Actual","MARs model", "GBM without correction", "GBM with correction")
#        , col=c("black","red","blue","green"),
#        lty=c(1,1,1,1), lwd=c(1,1,1,1), cex=0.9)
# 
# # RMSE
# # rmse(df_b$actual, df_b$old_model)
# # rmse(df_b$actual, df_b$new_model)
# # rmse(df_b$actual, df_b$new_model_corrected)

# -------------------------------------------------------------------------------------------------------------------------
# Comparison between datasets and models
# -------------------------------------------------------------------------------------------------------------------------
R2 = function(actual,predict){1 - (sum((actual-predict)^2)/sum((actual-mean(actual))^2))}

SumAbsErrors = function(actual,predict){sum(abs(actual-predict))}

# ============================================================================================
# Retransforming back to the original variable
# ============================================================================================

# Between datasets within model
# MARs
rmse(exp(b$RESPONSE), exp(b1_mars)) #0.2991173
rmse(exp(t$RESPONSE), exp(t1_mars)) #0.3212333
rmse(exp(v$RESPONSE), exp(v1_mars)) #0.3304931

R2(exp(b$RESPONSE), exp(b1_mars)) #0.6055581
R2(exp(t$RESPONSE), exp(t1_mars)) #0.5891065
R2(exp(v$RESPONSE), exp(v1_mars)) #0.5724674

SumAbsErrors(exp(b$RESPONSE), exp(b1_mars))/nrow(b) # 0.200733
SumAbsErrors(exp(t$RESPONSE), exp(t1_mars))/nrow(t) # 0.2169292
SumAbsErrors(exp(v$RESPONSE), exp(v1_mars))/nrow(v) # 0.226033

# GBM
rmse(exp(b$RESPONSE), exp(predict_gbm_model_b[,1])) #0.2754726 
rmse(exp(t$RESPONSE), exp(predict_gbm_model_t[,1])) #0.30832
rmse(exp(v$RESPONSE), exp(predict_gbm_model_v[,1])) #0.3172962

R2(exp(b$RESPONSE), exp(predict_gbm_model_b[,1])) #0.6654783
R2(exp(t$RESPONSE), exp(predict_gbm_model_t[,1])) #0.6214754
R2(exp(v$RESPONSE), exp(predict_gbm_model_v[,1])) #0.6058989

SumAbsErrors(exp(b$RESPONSE), exp(predict_gbm_model_b[,1]))/nrow(b) # 0.1857232
SumAbsErrors(exp(t$RESPONSE), exp(predict_gbm_model_t[,1]))/nrow(t) # 0.2081943
SumAbsErrors(exp(v$RESPONSE), exp(predict_gbm_model_v[,1]))/nrow(v) # 0.2181618

# ----------------------------------------------------------------------
# Plot density
# ----------------------------------------------------------------------
plot(density(exp(b$RESPONSE)))
lines(density(exp(b1_mars)), col = "red")
lines(density(exp(predict_gbm_model_b[,1])), col = "blue")
legend("topright", legend = c("Actual","MARs","GBM without PropDB")
       , col = c("black","red","blue")
       , lty = c(1,1,1))


# ============================================================================================
# Load model without PropDB
# ============================================================================================
gbm_model2 = h2o.loadModel(paste0(path,"/jpg_grid_gbm_market_compete_without_propDB_20_most_important_vars_model_12"))
h2o.performance(gbm_model2)

predict_gbm_model_v2 = as.data.frame(h2o.predict(gbm_model2, newdata = v_with_propDB_h2o.hex, type = "response"))
head(predict_gbm_model_v[,1])



# ============================================================================================
# Actuals vs predicted
# ============================================================================================
min(b$Pol_Dt_Quote)
max(b$Pol_Dt_Quote)
min(t$Pol_Dt_Quote)
max(t$Pol_Dt_Quote)
min(v$Pol_Dt_Quote)
max(v$Pol_Dt_Quote)

length(fr)
fr = c(b$Pol_Dt_Quote,t$Pol_Dt_Quote,v$Pol_Dt_Quote)
f0 = c(exp(df_b$actual),exp(df_t$actual),exp(df_v$actual))
f1 = c(exp(b1_mars),exp(t1_mars),exp(v1_mars))
f2 = c(exp(predict_gbm_model_b[,1]),exp(predict_gbm_model_t[,1]),exp(predict_gbm_model_v[,1]))

ap = aggregate(f0, by=list(fr), FUN=mean)
ap = subset(ap, Group.1 != "2018-04-11")
ap_1 = aggregate(f1, by=list(fr), FUN=mean)
ap_1 = subset(ap_1, Group.1 != "2018-04-11")
ap_2 = aggregate(f2, by=list(fr), FUN=mean)
ap_2 = subset(ap_2, Group.1 != "2018-04-11")
head(ap_1)

g = ggplot(data = ap, aes(x = Group.1, y = x)) + geom_point(aes(col = "red"),show.legend = TRUE) + geom_line(col = "red")
g = g + geom_point(data = ap_1, aes(x = Group.1, y = x, col = "blue"), show.legend = TRUE) + geom_line(data = ap_1, aes(x = Group.1, y = x), col = "blue")
g = g + geom_point(data = ap_2, aes(x = Group.1, y = x, col = "green"), show.legend = TRUE) + geom_line(data = ap_2, aes(x = Group.1, y = x), col = "green")
g = g + theme_bw()
g = g + xlab("Pol Dt Incept") + ylab("Average compete ratio (direct price / avg top 5 market price)")
g = g + scale_color_manual("Legend", values = c(red = "red", blue = "blue", green = "green")
                           , labels = c("MARS","GBM with PropDB","Actuals"))
g = g + geom_vline(xintercept=as.numeric(as.Date(max(b$Pol_Dt_Quote))), col = "grey")
g = g + geom_vline(xintercept=as.numeric(as.Date(max(t$Pol_Dt_Quote))), col = "grey")
g = g + theme(axis.text.x = element_text(angle=90))
g

p = ggplot(data=b, aes(x=Pol_Dt_Quote)) + geom_bar()
p = p + geom_bar(data=t, aes(x=Pol_Dt_Quote))
p = p + geom_bar(data=v, aes(x=Pol_Dt_Quote))
p = p + theme_bw()
p



# ============================================================================================
# Actuals vs predicted
# ============================================================================================

predict_gbm_model_d = as.data.frame(h2o.predict(gbm_model1, newdata = d_with_propDB_h2o.hex, type = "response"))
head(predict_gbm_model_d[,1])
head(d1_mars)
head(d$RESPONSE)

length(predict_gbm_model_d[,1])
length(d1_mars)
length(d$RESPONSE)

f0 = round(d$Prem_Net,-1)
f0 = d$Cust_Age_R
f0 = round(d$Veh_Capacity,-1)
f0 = d$Veh_Drv_Restrict
f0 = d$Cust_Lic_Len_R
f0 = round(d$DE_Credit_Score,-1)
f0 = round(d1$log_postcode_avg_annual_premium_top5,2)
f0 = round(d1$log_distance_to_booths,1)
f0 = round(d$postcode_avg_premier_ratio_top5,2)


# Actual vs. Predicted
ap1 <- aggregate(d$RESPONSE, by=list(f0), FUN=mean)
ap2 <- aggregate(d1_mars, by=list(f0), FUN=mean)
ap3 <- aggregate(predict_gbm_model_d[,1], by=list(f0), FUN=mean)
plot(ap1, ylab="Response", xlab="Credit score")
#plot(ap1, ylab="Response", xlab="Log Postcode average of annual premium top 5",xlim=c(5,8.5))
lines(ap2,col=2)
lines(ap3,col=3)
legend("topright", legend=c("Actual","MARS","GBM with PropDB"), col=c("black","red","green"),
       lty=c(1,1,1), lwd=c(1,1,1), cex=0.9)






# ============================================================================================
# function to see GBM
# ============================================================================================

grid_search_view_function = function(grid_name
                                     , grid_id
                                     , test_df
                                     , val_df){
  
  # Substring function
  substrRight = function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  print(grid_name)
  
  # Setting up an error data frame to hold values to plot
  error_df = data.frame(model_name = character()
                        , model_number = numeric()
                        , dataset = character()
                        , mse = numeric()
                        , rmse = numeric()
                        , ntrees = numeric()
                        , max_depth = numeric()
                        , learn_rate = numeric()
                        , min_rows = numeric())
  
  # Loop through models in the grid
  for(i in 1:length(grid_name@model_ids)){
    
    print('-----------------------------------------')
    print(grid_name@model_ids[[i]])
    print('-----------------------------------------')
    print('')
    
    # Getting the model
    print('Fetching the model')
    modelx = h2o.getModel(grid_name@model_ids[[i]])
    
    # Calculating performance of the model with new test data
    print('Calculating test performance')
    test_perf = h2o.performance(model = modelx
                                , newdata = test_df)
    
    print('Calculating validation performance')
    val_perf = h2o.performance(model = modelx
                               , newdata = val_df)
    
    
    
    print("Calculating metrics for the training dataset")
    error_df = rbind(error_df
                     ,data.frame(
                       model_name = grid_name@model_ids[[i]]
                       , model_number = gsub(grid_id, "", grid_name@model_ids[[i]])
                       , dataset = "train"
                       , mse = modelx@model$training_metrics@metrics$MSE
                       , rmse = sqrt(modelx@model$training_metrics@metrics$MSE)
                       , ntrees = as.numeric(modelx@model$model_summary$number_of_trees)
                       , max_depth = as.factor(as.numeric(modelx@model$model_summary$max_depth))
                       , learn_rate = as.numeric(modelx@allparameters$learn_rate)
                       , min_rows = as.numeric(modelx@allparameters$min_rows)
                     )
    )
    
    print("Calculating metrics for the testing dataset")
    error_df = rbind(error_df
                     ,data.frame(
                       model_name = grid_name@model_ids[[i]]
                       , model_number = gsub(grid_id, "", grid_name@model_ids[[i]])
                       , dataset = "test"
                       , mse = test_perf@metrics$MSE
                       , rmse = sqrt(test_perf@metrics$MSE)
                       , ntrees = as.numeric(modelx@model$model_summary$number_of_trees)
                       , max_depth = as.factor(as.numeric(modelx@model$model_summary$max_depth))
                       , learn_rate = as.numeric(modelx@allparameters$learn_rate)
                       , min_rows = as.numeric(modelx@allparameters$min_rows)
                     )
    )
    
    print("Calculating metrics for the validation dataset")
    error_df = rbind(error_df
                     ,data.frame(
                       model_name = grid_name@model_ids[[i]]
                       , model_number = gsub(grid_id, "", grid_name@model_ids[[i]])
                       , dataset = "val"
                       , mse = val_perf@metrics$MSE
                       , rmse = sqrt(val_perf@metrics$MSE)
                       , ntrees = as.numeric(modelx@model$model_summary$number_of_trees)
                       , max_depth = as.factor(as.numeric(modelx@model$model_summary$max_depth))
                       , learn_rate = as.numeric(modelx@allparameters$learn_rate)
                       , min_rows = as.numeric(modelx@allparameters$min_rows)
                     )
    )
    
  }
  
  return(error_df)
  
}

rrr = grid_search_view_function(grid_name = sorted_Grid_with_propDB
                                , grid_id = grid_title
                                , test_df = t_with_propDB_h2o.hex
                                , val_df = v_with_propDB_h2o.hex)

p = ggplot(rrr)
p = p + geom_point(data = rrr, aes(x = min_rows, y = mse, color = max_depth, shape = dataset, label = model_number), size = 3)
p = p + geom_text_repel(aes(x = min_rows, y = mse, label = model_number))
p = p + facet_grid(.~ntrees)
p = p + theme(legend.position = "bottom")
p
