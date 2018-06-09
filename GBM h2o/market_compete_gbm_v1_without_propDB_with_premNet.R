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

# ----------------------------------------------------------------------------------------------------------------------------
## Load Hive Data
# ----------------------------------------------------------------------------------------------------------------------------
load("/home/garciaj/Car_NB_valuation/Market_models/d_split_enriched.RData")

d = b_propDB

colnames(d)

# ----------------------------------------------------------------------------------------------------------------------------
# Column types and cleaning
# ----------------------------------------------------------------------------------------------------------------------------
d = function_clean_market_compete(d, with_propDB = FALSE)
head(d)

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
d$RESPONSE = log(d$Direct_Premium/d$Annual_Premium_Top5)
summary(d$RESPONSE)

na_count_d_extract = sapply(d, function(y) sum(length(which(is.na(y)))))
na_count_d_extract = data.frame(na_count_d_extract)
na_count_d_extract

d = na.omit(d)

# ----------------------------------------------------------------------------------------------------------------------------
# Preparing data for h2o
# ----------------------------------------------------------------------------------------------------------------------------
d1 = function_data_frame_for_h2o_nopropDB(d)
head(d1)

# After having created multiple models, we keep only the 20 most important variables
final_cols = c('log_Prem_Net','Cust_Age_R','Area_Grp_Adv_New'
               ,'DE_Credit_Score','Veh_Capacity','Cust_Age_1'
               ,'Veh_Drv_Restrict','Cust_Lic_Len_R','Pol_NCD_Yrs'
               ,'Cust_Lic_Len_1','log_Veh_Own_Yrs','DE_ID_Score'
               ,'Clm_Num_L5Y','Veh_Age_Yrs','Pol_CUE_Score'
               ,'DD_Flag','Veh_Own_Mnths','log_Veh_Mile_Total'
               ,'Veh_Use_Class','Veh_Fuel','Pol_Dt_Quote','RESPONSE')

d1 = d1[,final_cols]


# ----------------------------------------------------------------------------------------------------------------------------
# Train/Test
# ----------------------------------------------------------------------------------------------------------------------------
max(d1$Pol_Dt_Quote)
min(d1$Pol_Dt_Quote)

b = subset(d, Pol_Dt_Quote >= '2018-03-01' & Pol_Dt_Quote < '2018-03-20') # 2M rows
t = subset(d, Pol_Dt_Quote >= '2018-03-20' & Pol_Dt_Quote < '2018-04-02') # 931k rows
v = d[which(difftime(max(d$Pol_Dt_Quote),d$Pol_Dt_Quote,units="days")<10),] # 345k rows

b1_without_propDB = subset(d1, Pol_Dt_Quote >= '2018-03-01' & Pol_Dt_Quote < '2018-03-20') # 2M rows
b1_without_propDB$Pol_Dt_Quote = NULL
t1_without_propDB = subset(d1, Pol_Dt_Quote >= '2018-03-20' & Pol_Dt_Quote < '2018-04-02') # 931k rows
t1_without_propDB$Pol_Dt_Quote = NULL
v1_without_propDB = d1[which(difftime(max(d1$Pol_Dt_Quote),d1$Pol_Dt_Quote,units="days")<10),] # 345k rows
v1_without_propDB$Pol_Dt_Quote = NULL

# ----------------------------------------------------------------------------------------------------------------------------
# Loading previous market model
# ----------------------------------------------------------------------------------------------------------------------------
# load('/home/garciaj/Car_NB_valuation/Market_models/Conversion/4.6.0/compete_v4_6_0.R')
# coef(compete_v4_6_0)
# cat(gsub("+\n",")+\n",gsub("\\n","+\n(",format(compete_v4_6_0, style="pmax")), "\n"))

for(i in 1:3){
  
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
}

head(b1_mars)
head(t1_mars)
head(v1_mars)

mse(b$RESPONSE,b1_mars) #0.0417048
mse(t$RESPONSE,t1_mars) #0.0456481
mse(v$RESPONSE,v1_mars) #0.04858334

# ----------------------------------------------------------------------------------------------------------------------------
# Preparing data for h2o
# ----------------------------------------------------------------------------------------------------------------------------

require(h2o)
h2o.init()

b_without_propDB_h2o.hex = as.h2o(b1_without_propDB)
t_without_propDB_h2o.hex = as.h2o(t1_without_propDB)
v_without_propDB_h2o.hex = as.h2o(v1_without_propDB)

# ----------------------------------------------------------------------------------------------------------------------------
# Preparing data for h2o
# ----------------------------------------------------------------------------------------------------------------------------

hyper_params = list(max_depth = c(4,5)
                    , ntrees = c(450)
                    , min_rows = c(1000)
                    )
  
y_col = grep("RESPONSE", colnames(b_without_propDB_h2o.hex))
x_col = seq(1,ncol(b_without_propDB_h2o.hex),1)
x_col = x_col[!x_col == y_col] 
  
grid_title = "jpg_grid_gbm_market_compete_without_propDB_20_most_important_vars"
    
gbm_grid_without_propDB_20_most_imp_vars = h2o.grid(
                                  ## hyper parameters
                                  hyper_params = hyper_params
                                  
                                  ## which algorithm to run
                                  , algorithm = "gbm"
                                  
                                  ## identifier for the grid, to later retrieve it
                                  , grid_id = grid_title
                                  
                                  ## standard model parameters
                                  , x = x_col
                                  , y = y_col
                                  , training_frame = b_without_propDB_h2o.hex
                                  #, validation_frame = t_without_propDB_h2o.hex
                                  
                                  , nfolds = 3
                                  
                                  ## fix a random number generator seed for reproducibility
                                  , seed = 1234)
  
sorted_Grid_without_propDB = h2o.getGrid(grid_title, sort_by = "MSE", decreasing = FALSE)
sorted_Grid_without_propDB

# ------------------------------------------------------------------------------------------------------
# Picking the model
# ------------------------------------------------------------------------------------------------------
params_model = h2o.getModel(sorted_Grid_without_propDB@model_ids[[which(sorted_Grid_without_propDB@model_ids == "jpg_grid_gbm_market_compete_without_propDB_20_most_important_vars_model_12")]])

# -------------------------------------------------------------------------------------------------------------------------
# SAVING THE MODEL
# -------------------------------------------------------------------------------------------------------------------------
path = '/home/garciaj/Car_NB_valuation/Market_models/201805_GBM_final_methods'
h2o.saveModel(params_model, path) 

gbm_model1 = h2o.loadModel(paste0(path,"/jpg_grid_gbm_market_compete_without_propDB_20_most_important_vars_model_12"))
h2o.performance(gbm_model1)

# -------------------------------------------------------------------------------------------------------------------------
# Variable importance
# -------------------------------------------------------------------------------------------------------------------------
var_importance_df = as.data.frame(h2o.varimp(gbm_model1))
first_20_vars = var_importance_df[1:20,]
first_20_vars

# -------------------------------------------------------------------------------------------------------------------------
# Predictions - train
# -------------------------------------------------------------------------------------------------------------------------
predict_gbm_model_b = as.data.frame(h2o.predict(gbm_model1, newdata = b_without_propDB_h2o.hex, type = "response"))
head(predict_gbm_model_b[,1])

df_b = data.frame(actual = b$RESPONSE
                  , old_model = b1_mars
                  , new_model = predict_gbm_model_b[,1]
)

# Differences between both models
df_b$old_model_vs_actual = df_b$actual-df_b$old_model
df_b$new_model_vs_actual = df_b$actual-df_b$new_model
df_b$old_model_vs_new_model = abs(df_b$old_model_vs_actual) - abs(df_b$new_model_vs_actual)

head(df_b)

mean(df_b$old_model_vs_new_model)

sum(abs(df_b$old_model_vs_actual)) #314237.4
sum(abs(df_b$old_model_vs_actual))/nrow(df_b) #0.1533821

sum(abs(df_b$new_model_vs_actual)) #305662.1
sum(abs(df_b$new_model_vs_actual))/nrow(df_b) #0.1491964

(sum(abs(df_b$old_model_vs_actual)) - sum(abs(df_b$new_model_vs_actual)))/nrow(df_b) # 0.004218545

# Plots
plot(density(df_b$actual))
lines(density(df_b$old_model), col = "red")
lines(density(df_b$new_model), col = "blue")


# -------------------------------------------------------------------------------------------------------------------------
# Predictions - test
# -------------------------------------------------------------------------------------------------------------------------
predict_gbm_model_t = as.data.frame(h2o.predict(gbm_model1, newdata = t_without_propDB_h2o.hex, type = "response"))
head(predict_gbm_model_t[,1])

df_t = data.frame(actual = t$RESPONSE
                  , old_model = t1_mars
                  , new_model = predict_gbm_model_t[,1]
)

# Differences between both models
df_t$old_model_vs_actual = df_t$actual-df_t$old_model
df_t$new_model_vs_actual = df_t$actual-df_t$new_model
df_t$old_model_vs_new_model = abs(df_t$old_model_vs_actual) - abs(df_t$new_model_vs_actual)

head(df_t)

sum(abs(df_t$old_model_vs_actual)) #149958.6
sum(abs(df_t$old_model_vs_actual))/nrow(df_t) #0.1609

sum(abs(df_t$new_model_vs_actual)) #147754.8
sum(abs(df_t$new_model_vs_actual))/nrow(df_t) #0.1580174

(sum(abs(df_t$old_model_vs_actual)) - sum(abs(df_t$new_model_vs_actual)))/nrow(df_t) #0.002939072


# Plots
plot(density(df_t$actual))
lines(density(df_t$old_model), col = "red")
lines(density(df_t$new_model), col = "blue")

# -------------------------------------------------------------------------------------------------------------------------
# Predictions - validation
# -------------------------------------------------------------------------------------------------------------------------
predict_gbm_model_v = as.data.frame(h2o.predict(gbm_model1, newdata = v_without_propDB_h2o.hex, type = "response"))
head(predict_gbm_model_v[,1])

df_v = data.frame(actual = v$RESPONSE
                  , old_model = v1_mars
                  , new_model = predict_gbm_model_v[,1]
)

# Differences between both models
df_v$old_model_vs_actual = df_v$actual-df_v$old_model
df_v$new_model_vs_actual = df_v$actual-df_v$new_model
df_v$old_model_vs_new_model = abs(df_v$old_model_vs_actual) - abs(df_v$new_model_vs_actual)

head(df_v)

mean(df_v$old_model_vs_new_model)

sum(abs(df_v$old_model_vs_actual)) #57405.79
sum(abs(df_v$old_model_vs_actual))/nrow(df_v) #0.1666

sum(abs(df_v$new_model_vs_actual)) #56298.51
sum(abs(df_v$new_model_vs_actual))/nrow(df_v) #0.1634224

(sum(abs(df_v$old_model_vs_actual)) - sum(abs(df_v$new_model_vs_actual)))/nrow(df_v) # 0.0003875846


# Plots
plot(density(df_v$actual))
lines(density(df_v$old_model), col = "red")
lines(density(df_v$new_model), col = "blue")


# -------------------------------------------------------------------------------------------------------------------------
# Comparison between datasets and models
# -------------------------------------------------------------------------------------------------------------------------
R2 = function(actual,predict){1 - (sum((actual-predict )^2)/sum((actual-mean(actual))^2))}

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
rmse(exp(b$RESPONSE), exp(predict_gbm_model_b[,1])) #0.2899575 
rmse(exp(t$RESPONSE), exp(predict_gbm_model_t[,1])) #0.3151903
rmse(exp(v$RESPONSE), exp(predict_gbm_model_v[,1])) #0.3228465

R2(exp(b$RESPONSE), exp(predict_gbm_model_b[,1])) #0.6293461
R2(exp(t$RESPONSE), exp(predict_gbm_model_t[,1])) #0.6044203
R2(exp(v$RESPONSE), exp(predict_gbm_model_v[,1])) #0.5920221

SumAbsErrors(exp(b$RESPONSE), exp(predict_gbm_model_b[,1]))/nrow(b) # 0.1948935
SumAbsErrors(exp(t$RESPONSE), exp(predict_gbm_model_t[,1]))/nrow(t) # 0.2128757
SumAbsErrors(exp(v$RESPONSE), exp(predict_gbm_model_v[,1]))/nrow(v) # 0.2215092


# Plotting
plot(density(exp(b$RESPONSE)))
lines(density(exp(b1_mars)), col = "red")
lines(density(exp(predict_gbm_model_b[,1])), col = "blue")
legend("topright", legend = c("Actual","MARs","GBM with PropDB")
       , col = c("black","red","blue")
       , lty = c(1,1,1))


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

rrr = grid_search_view_function(grid_name = sorted_Grid_without_propDB
                                , grid_id = grid_title #"jpg_grid_gbm_market_compete_with_propDB"
                                , test_df = t_without_propDB_h2o.hex
                                , val_df = v_without_propDB_h2o.hex)

p = ggplot(rrr)
p = p + geom_point(data = rrr, aes(x = learn_rate, y = mse, color = max_depth, shape = dataset, label = model_number), size = 3)
p = p + geom_text_repel(aes(x = learn_rate, y = mse, label = model_number))
p = p + facet_grid(.~ntrees)
p = p + theme(legend.position = "bottom")
p
