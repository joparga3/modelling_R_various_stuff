# sqldf("SELECT PREM_TEST, AVG(NETPREMIUM), AVG(GROSSPREMIUM), AVG(SALE) FROM d WHERE INSURER = 'Advantage' GROUP BY PREM_TEST")
# 
# d$PREM_TEST
# d$NETPREMIUM
# d$GROSSPREMIUM

##############################################################################################################################
#
#  LOAD REQUIRED PACKAGES
#
##############################################################################################################################

require(RODBC)
require(earth)
require(glmnet)
require(lme4)
require(ROCR)
require(lattice)
require(caret)
require(data.table)

source('/home/garciaj/Home/Conversion_model/201802/UDF_list.R')

checkPolyFit <- function(x,y,dof = 2, dps)
{
  fit_lin = lm(y~x)
  fit_poly = lm(y~poly(x,dof,raw = T))
  
  plot(aggregate(y,by=list(round(x, dps)),FUN=mean))
  lines(aggregate(fit_lin$fitted.values,by=list(round(x, dps)),FUN=mean), col = "blue")
  lines(aggregate(fit_poly$fitted.values,by=list(round(x, dps)),FUN=mean), col = "red")
  
}

checkHingeFit <- function(x,y,hp, dps)
{
  fit_l = lm(y~h(x-hp) + h(hp-x))
  fit_r = lm(y~h(x-hp) + h(hp-x) + ifelse(x<hp,1,0))
  plot(aggregate(y,by=list(round(x, dps)),FUN=mean))
  lines(aggregate(fit_l$fitted.values,by=list(round(x, dps)),FUN=mean), col = "blue")
  lines(aggregate(fit_r$fitted.values,by=list(round(x, dps)),FUN=mean), col = "red")
  
}

checkInteractionFit <- function(x1, x2,y, dps)
{
  fit_l = lm(y~x1+x2)
  fit_r = lm(y~I(x1*x2))
  plot(aggregate(y,by=list(round(x1*x2, dps)),FUN=mean))
  lines(aggregate(fit_l$fitted.values,by=list(round(x1*x2, dps)),FUN=mean), col = "blue")
  lines(aggregate(fit_r$fitted.values,by=list(round(x1*x2, dps)),FUN=mean), col = "red")
  
}

h = function(x){pmax(x,0)}


# Define custom link function
# Symmetric - nu alters thickness of both tails
StudentT <- function(nu) {
  qqt <- function(p, nu)
    sign(p-0.5)*sqrt(qf(1-2*pmin(p,1-p), 1, nu))
  linkfun <- function(mu) qqt(mu,nu)
  linkinv <- function(eta) {
    thresh <- -qqt(.Machine$double.eps,nu)
    eta <- pmin(thresh, pmax(eta, -thresh))
    pt(eta, nu)
  }
  mu.eta <- function(eta)
    pmax(dt(eta, nu), .Machine$double.eps)
  valideta <- function(eta) TRUE
  name <- "StudentT"
  structure(list(linkfun=linkfun,
                 linkinv=linkinv,
                 mu.eta=mu.eta,
                 valideta=valideta,
                 name=name),
            class = "link-glm")
}

# Asymmetric -  phi alters thickness of one tail
ao2 <- function(phi, verbose = FALSE)
{
  ## parameter processing
  if(phi == 1) {
    rval <- make.link("logit")
    rval$name <- "ao2"
    return(rval)
  }
  if(phi == 0) {
    rval <- make.link("cloglog")
    rval$name <- "ao2"
    return(rval)
  }
  
  linkfun <- function(mu) log(((1 - mu)^(-phi) - 1)/phi)
  
  linkinv <- function(eta){
    if(phi < 0) {
      etastar <- pmin(eta, log(-1/phi) - .Machine$double.eps)
      if(verbose && !isTRUE(all.equal(as.vector(eta), as.vector(etastar))))
        warning("truncation in inverse link function")
      eta <- etastar
    }
    1- (1 + phi * exp(eta))^(-1/phi)
  }
  
  mu.eta <- function(eta) exp(eta) * (1 + phi * exp(eta))^(-(1 + phi)/phi)
  
  valideta <- function(eta) {
    if(verbose && !all(phi * exp(eta) > -1)) warning("some of the current etas are out of range")
    TRUE
  }
  
  name <- "ao2" ## "Aranda-Ordaz asymmetric"
  
  ## return link-glm object
  structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta,
                 valideta = valideta, name = name), class = "link-glm")
}



##############################################################################################################################
#
#  LOAD DATA
#
##############################################################################################################################

d_raw = fread('/home/garciaj/Home/Conversion_model/201804/Home_Quote_EXTRACT_20180101_20180409.txt', sep = '\t')

d = d_raw
d = as.data.frame(d)
d = d[,sort(colnames(d))]
head(d)

# Table of conversion
table(d$SALE)

# 0         1 
# 1,898,281   23,409 
# (23409/1898281)*100 # --> 1.23%

##############################################################################################################################
#
#  PREPROCESSING
#
##############################################################################################################################

# ----------------------------------------------------------------------------------------------------------------------------
# NAs and columns to omit
# ----------------------------------------------------------------------------------------------------------------------------

# COLUMN NAMES
sort(colnames(d))
#  "AGE"                        "AGGCOUNT"                   "ALARM"                      "ANCILLARY1"                 "ANCILLARY2"                
# [6] "ANCILLARY3"                 "ANCILLARYINCOME"            "ANCILLARYPREMIUM"           "BANKRUPTCY_IVA_PRESENT"     "BRAND"                     
# [11] "BUILD_YEAR"                 "BUILDINGS_ACC_DAMAGE"       "BUILDINGS_EXCESS"           "BUILDINGS_VALUE"            "BUILDINGS_YEARS_NC"        
# [16] "BUILDINGSQUOTECOUNT"        "BUSINESS_USE"               "CASH_INSURED_VALUE"         "CCJ_PRESENT"                "CLM_BUILDINGS_NUM_L1Y"     
# [21] "CLM_BUILDINGS_NUM_L5Y"      "CLM_CONTENTS_NUM_L1Y"       "CLM_CONTENTS_NUM_L5Y"       "CLM_NUM_L1Y"                "CLM_NUM_L5Y"               
# [26] "COMBINEDQUOTECOUNT"         "COMPLETIONCHANNELCODE"      "CONTENTS_ACC_DAMAGE"        "CONTENTS_EXCESS"            "CONTENTS_VALUE"            
# [31] "CONTENTS_YEARS_NC"          "CONTENTSQUOTECOUNT"         "CREDIT_CARD_INSURED_VALUE"  "CREDITSCORE"                "CUST_NUM_CHILDREN"         
# [36] "CUST_NUM_NON_REL"           "CUST_NUM_OTH_REL"           "CUST_NUM_PARENTS"           "CUST_SMOKER"                "DATEOFBIRTH"               
# [41] "DAY_OF_BIRTH"               "DAYSTOINCEPTION"            "EMPLOYERSBUSINESS"          "EMPLOYMENTTYPE"             "FEE"                       
# [46] "FIRSTSEENDATE"              "FLAT_ROOF_PERCENTAGE"       "GROSSPREMIUM"               "INSTALMENTSREQUESTEDIND"    "INSURANCEREFUSED"          
# [51] "INTERESTINCOME"             "LISTED"                     "MARITALSTATUS"              "MAX_SPEC_VALUE"             "MAX_SPEC_VALUE_AWAY"       
# [56] "MAX_SPEC_VALUE_HOME"        "MINIMUM_LOCKS"              "NEAR_WATER"                 "NEIGHBOUR_WATCH"            "NETPREMIUM"                
# [61] "NUM_ADULT_OCCUPANTS"        "NUM_CHILD_OCCUPANTS"        "NUMBER_BATHROOMS"           "NUMBER_OF_BEDROOMS"         "NUMBER_OF_ROOMS"           
# [66] "OCCUPANCY"                  "OCCUPATION_GROUP"           "OCCUPATIONCODE"             "ORIGINALCHANNELCODE"        "OWNEDMONTHS"               
# [71] "OWNEDYEARS"                 "OWNERSHIP_TYPE"             "PAYMENTTYPE"                "POL_COVER"                  "POL_DT_INCEPT"             
# [76] "POL_DT_QUOTE"               "POL_HOMEOWNER"              "POL_PRO_NUM"                "POL_SCHEME"                 "PREM_ANC_TOTAL"            
# [81] "PREM_TEST"                  "PREVIOUS_BUILDINGS_INSURER" "PREVIOUS_BUILDINGS_POLICY"  "PREVIOUS_CONTENTS_INSURER"  "PREVIOUS_CONTENTS_POLICY"  
# [86] "PROPERTY_TYPE"              "PURCHASEDATE"               "QUOTECOUNT"                 "QUOTEREFERENCE"             "REQUESTID"                 
# [91] "RISK_AREA"                  "RISK_CODE"                  "SALE"                       "SB__"                       "SEENTOCURRENT"             
# [96] "SEENTOINCEPTION"            "SMOKE_DETECTORS"            "TOTAL_SPEC_VALUE"           "TOTAL_SPEC_VALUE_AWAY"      "TOTAL_SPEC_VALUE_HOME"     
# [101] "TRANSFERBRAND"              "TRANSFERID"                 "TRANSFERRED"                "UNSPECIFIED_ITEMS_VALUE"    "YEARS_RESIDENCY"  

for(i in sort(colnames(d))){print(i)}

# Eliminate variables we dont/cant use
d$AGGCOUNT = NULL
d$ANCILLARY1  = NULL              
d$ANCILLARY2 = NULL                
d$ANCILLARY3  = NULL
d$ANCILLARYINCOME = NULL
d$ANCILLARYPREMIUM = NULL
d$BANKRUPTCY_IVA_PRESENT = NULL
d$BUILDINGS_VALUE = NULL
d$BUILDINGSQUOTECOUNT = NULL
d$CASH_INSURED_VALUE = NULL
d$CCJ_PRESENT = NULL
d$COMBINEDQUOTECOUNT = NULL
d$COMPLETIONCHANNELCODE = NULL
d$CONTENTSQUOTECOUNT = NULL
d$CREDIT_CARD_INSURED_VALUE = NULL
d$DAY_OF_BIRTH = NULL
d$EMPLOYERSBUSINESS = NULL
d$EMPLOYMENTTYPE = NULL
d$FEE = NULL
d$FIRSTSEENDATE = NULL
d$INSURANCEREFUSED = NULL
d$INTERESTINCOME = NULL
d$OCCUPATION_GROUP = NULL
d$OCCUPATIONCODE = NULL
d$OWNEDMONTHS = NULL
d$OWNEDYEARS = NULL
d$PAYMENTTYPE = NULL
d$POL_HOMEOWNER = NULL
d$POL_PRO_NUM = NULL
d$PREM_ANC_TOTAL = NULL
d$PREVIOUS_BUILDINGS_INSURER = NULL
d$PREVIOUS_BUILDINGS_POLICY = NULL
d$PREVIOUS_CONTENTS_INSURER = NULL
d$PREVIOUS_CONTENTS_POLICY = NULL
d$PURCHASEDATE = NULL
d$QUOTECOUNT = NULL
d$QUOTEREFERENCE = NULL
d$REQUESTID = NULL
d$RISK_CODE = NULL
d$SB__ = NULL
d$TRANSFERBRAND = NULL
d$TRANSFERID = NULL
d$TRANSFERRED = NULL

# Checking for NAs
na_count_d_extract = sapply(d, function(y) sum(length(which(is.na(y)))))
na_count_d_extract = data.frame(na_count_d_extract)
na_count_d_extract

d$CREDITSCORE = ifelse(is.na(d$CREDITSCORE),0,d$CREDITSCORE)
d$CREDITSCORE = ifelse(d$CREDITSCORE == 9999,0,d$CREDITSCORE)

# Checking how many sales were many with the 2k creditscores that were NAs --> given that only 1 sale was made, we are confident that
# we can delete these rows. Alternatively, we could have created an interaction flag for these 2k records, but given time constraints
# we are happy with na.omit()
table(subset(d, is.na(CREDITSCORE) == TRUE)$SALE)
d = na.omit(d)

# ----------------------------------------------------------------------------------------------------------------------------
# Transformations
# ----------------------------------------------------------------------------------------------------------------------------

d$Contents_ncd_corrected = ifelse(d$ORIGINALCHANNELCODE=='Money Supermarket',
                                  ifelse(d$CONTENTS_YEARS_NC > 5
                                         , 5
                                         , d$CONTENTS_YEARS_NC)
                                  , d$CONTENTS_YEARS_NC)

d$Buildings_ncd_corrected = ifelse(d$ORIGINALCHANNELCODE=='Money Supermarket',
                                   ifelse(d$BUILDINGS_YEARS_NC>5
                                          , 5
                                          , d$BUILDINGS_YEARS_NC)
                                   , d$BUILDINGS_YEARS_NC)

d$Insurer = ifelse(d$POL_SCHEME=='HZ'
                   ,'Towergate'
                   , ifelse(d$POL_SCHEME=='JH'
                            , 'AXA'
                            , ifelse(d$POL_SCHEME=='SH'
                                     ,'Sterling'
                                     ,'Advantage')))


d$GROSSPREMIUM_fixed = ifelse(d$PREM_TEST==0.02
                              , ((((d$GROSSPREMIUM-d$NETPREMIUM) / d$NETPREMIUM) - 0.02 + 1) * d$NETPREMIUM)
                              , ifelse(d$PREM_TEST==-0.02
                                       , ((((d$GROSSPREMIUM-d$NETPREMIUM) / d$NETPREMIUM) + 0.02 + 1) * d$NETPREMIUM)
                                       , d$GROSSPREMIUM))

d$POL_DT_QUOTE = as.Date(d$POL_DT_QUOTE, format = "%C%y-%m-%d")
d$POL_DT_INCEPT = as.Date(d$POL_DT_INCEPT, format = "%C%y-%m-%d")

# ----------------------------------------------------------------------------------------------------------------------------
# Subsetting
# ----------------------------------------------------------------------------------------------------------------------------
# Only want buildings and contents
d = subset(d, POL_COVER=="B&C")

# Remove ultra high premiums - only ~6k out of the 1M records
d = subset(d, NETPREMIUM < 600)

# Remove weird days to inception
d = subset(d, DAYSTOINCEPTION < 50)

# Exclude Sterling and Towergate
d = subset(d, Insurer != 'Sterling')
d = subset(d, Insurer != 'Towergate')

##############################################################################################################################
#
#  DATASET SPLIT
#
##############################################################################################################################

set.seed(1066)

totrows = nrow(d)
rownos = seq(totrows)
parts = c(0.8,0.15,0.05)

sets <- vector(mode = "list", length = length(parts))

for( i in seq(parts))
{
  # calculating random % row numbers, % specified by parts[i]
  sets[[i]] <- sample(x = rownos, size = parts[i]*totrows)
  # removing used row nos
  rownos <- setdiff(rownos, sets[[i]])
}

b = d[sets[[1]],] # Build
t = d[sets[[2]],] # Test
v = d[sets[[3]],] # Validate
rec = d[which(difftime(max(d$POL_DT_INCEPT),d$POL_DT_INCEPT,units="days")<14),] # Check

rm(totrows,rownos,parts,i,sets)

br = b$SALE
tr = t$SALE
vr = v$SALE
recr = rec$SALE

feb = subset(d, POL_DT_INCEPT < '2018-02-28')
march = subset(d, POL_DT_INCEPT > '2018-02-28')

champ = c(1:3,9:13,19:23,29:31)
champion = subset(d, as.numeric(substr(DATEOFBIRTH,9,10)) %in% champ)
challenger = subset(d, ! as.numeric(substr(DATEOFBIRTH,9,10)) %in% champ)

##############################################################################################################################
#
#  BUILD MODEL WITH TRAINING DATA
#
##############################################################################################################################

# MODEL FORMULA
# -------------------------------------------------------------------------------------------------------------------------
f = ~ 
# -------------------------------------------------------------------------
# HOUSE RELATED FACTORS
# -------------------------------------------------------------------------
# BUILD YEAR
I(pmax(pmax(BUILD_YEAR,1800)-1800,0)/100)  +

# NUMBER BEDROOMS
pmin(NUMBER_OF_BEDROOMS,7) +

# NUMBER ROOMS
I(pmin(NUMBER_OF_ROOMS,12)^2/10) +

# NUMBER BATHROOMS
pmin(NUMBER_BATHROOMS,7) +

# RISK AREA
ifelse(RISK_AREA == "South East", 1, 0) +
ifelse(RISK_AREA == "Outer London", 1, 0) +
ifelse(RISK_AREA == "East Midlands", 1, 0) +

# FLAGS
ifelse(NEAR_WATER == "Y", 1, 0) + 
ifelse(NEIGHBOUR_WATCH == "Y", 1, 0) +

# PROPERTY_TYPE
ifelse(PROPERTY_TYPE == "Semi-Detached Bungalow", 1, 0) +
ifelse(PROPERTY_TYPE == "Detached", 1, 0) +
ifelse(PROPERTY_TYPE == "Detached Bungalow", 1, 0) +

# CONTENS_VALUE
ifelse(CONTENTS_VALUE>75000, 1, 0) +

# TOTAL_SPEC_VALUE
pmax(round(log(TOTAL_SPEC_VALUE + 1),1),6) +
I(pmax(round(log(TOTAL_SPEC_VALUE + 1),1),6)^2/10) +

# UNSPECIFIED_ITEMS_VALUE
pmax(round(log(UNSPECIFIED_ITEMS_VALUE + 1),1),4) +
I(pmax(round(log(UNSPECIFIED_ITEMS_VALUE + 1),1),4)^2/10) +
ifelse(round(log(UNSPECIFIED_ITEMS_VALUE + 1),1) == 0, 1, 0) +

# -------------------------------------------------------------------------
# CUSTOMER RELATED FACTORS
# -------------------------------------------------------------------------
# AGE
I(pmax(pmax(pmin(AGE,80),18) -25,0)^2/100) +

# CLAIMS
pmax(1-CLM_NUM_L5Y,0) +

# BUILDINGS_YEARS_NC +
ifelse(BUILDINGS_YEARS_NC==5,1,0) +

# NUM_ADULT_OCCUPANTS
pmax(pmin(NUM_ADULT_OCCUPANTS,5)-3,0) +

# NUM_CHILD_OCCUPANTS
NUM_CHILD_OCCUPANTS +

# -------------------------------------------------------------------------
# QUOTE RELATED FACTORS
# -------------------------------------------------------------------------
# INDICATOR FLAGS
ifelse(BUILDINGS_ACC_DAMAGE == "Y", 1, 0) +
ifelse(INSTALMENTSREQUESTEDIND == "Y", 1, 0) +

# AGGREGATORS
ifelse(ORIGINALCHANNELCODE == "Money Supermarket",1,0) +
ifelse(ORIGINALCHANNELCODE == "GoCompare",1,0) +
ifelse(ORIGINALCHANNELCODE == "Confused.com",1,0) +
ifelse(ORIGINALCHANNELCODE == "Compare The Market",1,0) +

# Interact INSURER with PREMIUM
I(ifelse(Insurer == "AXA",1,0)*log(NETPREMIUM)) +
I(ifelse(Insurer == "AXA",1,0)*(pmin(log(NETPREMIUM),4.5)^2)) +
I(ifelse(Insurer == "Advantage",1,0)*log(NETPREMIUM)) +

# PRICES
PREM_TEST + 

# Interact PREM_TEST and NETPREMIUM to get pound amount change
I(PREM_TEST*log(NETPREMIUM)) +
I(PREM_TEST*(1/(pmax((round(log(NETPREMIUM),2))-4.5,0)+1))) +

# CONTENTS_EXCESS
h(250-CONTENTS_EXCESS) +

# DATES TO INCEPTION
ifelse(pmin(DAYSTOINCEPTION,30)==0,1,0) + 
ifelse(pmin(DAYSTOINCEPTION,30)==21,1,0) +
ifelse(pmin(DAYSTOINCEPTION,30)==28,1,0) +
ifelse(pmin(DAYSTOINCEPTION,30)==29,1,0) +
pmin(DAYSTOINCEPTION,30) +
I(pmin(DAYSTOINCEPTION,30)^2/10) + 

# SEENTOCURRENT
pmin(SEENTOCURRENT,30) + 
ifelse(pmin(SEENTOCURRENT,30) == 0, 1, 0) +

- 1


bm = model.matrix(f, data = b, sparse = T)  
tm = model.matrix(f, data = t, sparse = T)  
recm = model.matrix(f, data = rec, sparse = T)  
febm = model.matrix(f, data = feb, sparse = T)  
marchm = model.matrix(f, data = march, sparse = T) 
champm = model.matrix(f, data = champion, sparse = T) 
challm = model.matrix(f, data = challenger, sparse = T) 
head(bm)

# BUILD MODEL
# -------------------------------------------------------------------------------------------------------------------------
q = 1

# # Fixed effects model
# b1 = glm(I(1-br) ~ bm, family = binomial(link=ao2(q)))
# summary(b1)

re1 = ifelse(b$POL_DT_INCEPT > '2018-01-12',1,0)
re2 = ifelse(b$POL_DT_INCEPT > '2018-02-27',1,0)

b1 = glmer(I(1-br) ~ bm 
                      + (1|re1) 
                      + (1|re2)
           , data=b
           , binomial(link=ao2(q)), verbose = 1, nAGQ = 0)
 
summary(b1)
# fixef(b1)
# ranef(b1)


# PREDICTIONS
# -------------------------------------------------------------------------------------------------------------------------
b1p = 1 - predict(b1, type = "response", re.form=NULL, newdata = b)
r1 = data.frame(cbind(br,b1p))
head(r1)

(mean(b1p)/mean(b$SALE) - 1)*100
# 6.574741e-11

# -------------------------------------------------------------------------------------------------------------------------
# PERFORMANCE - TRAIN DATA
# -------------------------------------------------------------------------------------------------------------------------
# Gini 
#------------------------------------------------------------------------------------------------
as.numeric(performance(prediction(b1p,r1$br),"auc")@y.values)*2-1
# 0.8786549

# Actuals vs Predicted 
#------------------------------------------------------------------------------------------------
UDF_act_pred_overtime(b$POL_DT_INCEPT,br,b1p,xlabel="Inception Date")

# -------------------------------------------------------------------------------------------------------------------------
# PERFORMANCE - TEST DATA
# -------------------------------------------------------------------------------------------------------------------------
# Predictions
#------------------------------------------------------------------------------------------------
# Rand effects for test dataset
tz1 = as.matrix(ranef(b1)$re1[match(ifelse(t$POL_DT_INCEPT > '2018-01-12',1,0), rownames(as.matrix(ranef(b1)$re1))),])
tz2 = as.matrix(ranef(b1)$re2[match(ifelse(t$POL_DT_INCEPT > '2018-02-27',1,0), rownames(as.matrix(ranef(b1)$re2))),])

t1p = 1 - 1/((1 + q * exp(-(tz1
                            +tz2
                            +(fixef(b1)[1]+tm%*%as.matrix(fixef(b1)[2:length(fixef(b1))])))))^(1/q))
r1t = data.frame(cbind(tr,t1p))
head(r1t)

(mean(t1p)/mean(t$SALE) - 1)*100
# -1.293202

# Gini 
#------------------------------------------------------------------------------------------------
as.numeric(performance(prediction(t1p,r1t$tr),"auc")@y.values)*2-1
# 0.8752372

# -------------------------------------------------------------------------------------------------------------------------
# PERFORMANCE - RECENT DATA
# -------------------------------------------------------------------------------------------------------------------------
# Predictions
#------------------------------------------------------------------------------------------------
# Rand effects for test dataset
recz1 = as.matrix(ranef(b1)$re1[match(ifelse(rec$POL_DT_INCEPT > '2018-01-12',1,0), rownames(as.matrix(ranef(b1)$re1))),])
recz2 = as.matrix(ranef(b1)$re2[match(ifelse(rec$POL_DT_INCEPT > '2018-02-27',1,0), rownames(as.matrix(ranef(b1)$re2))),])

rec1p = 1 - 1/((1 + q * exp(-(recz1
                              +recz2
                              +(fixef(b1)[1]+recm%*%as.matrix(fixef(b1)[2:length(fixef(b1))])))))^(1/q))
r1rec = data.frame(cbind(recr,rec1p))
head(r1rec)

(mean(rec1p)/mean(rec$SALE) - 1)*100
# 2.653023

# Gini 
#------------------------------------------------------------------------------------------------
as.numeric(performance(prediction(rec1p,r1rec$recr),"auc")@y.values)*2-1
# 0.910998

# -------------------------------------------------------------------------------------------------------------------------
# ELASTICITY PLOT - TRAIN DATA
#    - there is another way to do it (see end of script commented section)
#    - but we cant figure out why does the +/-5% doesnt work with that
#    - therefore we have gone with doing brute force elasticity plots
#    - by brute force I mean taking the model formula and changing the prem_test with +0.05 or -0.05
# -------------------------------------------------------------------------------------------------------------------------

bz1_1 = as.matrix(ranef(b1)$re1[match(ifelse(b$POL_DT_INCEPT > '2018-01-12',1,0), rownames(as.matrix(ranef(b1)$re1))),])
bz2_2 = as.matrix(ranef(b1)$re2[match(ifelse(b$POL_DT_INCEPT >'2018-02-27',1,0), rownames(as.matrix(ranef(b1)$re2))),])

# 0% Flex
e = b
e$PREM_TEST = ifelse(e$NETPREMIUM>0,0.00,0)
em = model.matrix(f, data = e, sparse = T)  
base = as.vector(1 - 1/((1+q*exp(-(bz1_1 
                                + bz2_2 
                                + (fixef(b1)[1]) + em %*% as.matrix(fixef(b1)[2:length(fixef(b1))])))))^(1/q))

# +5% Flex
e = b
e$PREM_TEST = ifelse(e$NETPREMIUM>0,0.05,0)
em = model.matrix(f, data = e, sparse = T)  
plus5 = as.vector(1 - 1/((1+q*exp(-(bz1_1 
                                   + bz2_2 
                                   + (fixef(b1)[1]) + em %*% as.matrix(fixef(b1)[2:length(fixef(b1))])))))^(1/q))

# -5% Flex
e = b
e$PREM_TEST = ifelse(e$NETPREMIUM>0,-0.05,0)
em = model.matrix(f, data = e, sparse = T)  
minus5 = as.vector(1 - 1/((1+q*exp(-(bz1_1 
                                   + bz2_2 
                                   + (fixef(b1)[1]) + em %*% as.matrix(fixef(b1)[2:length(fixef(b1))])))))^(1/q))

head(base)
head(plus5)
head(minus5)


factor <- round((b$NETPREMIUM),0)
hist(factor)
ElasPlt(actual=br,base=base,plus=plus5,minus=minus5,
        factor=factor,
        ylab="Conversion rate",xlab="Net Premium",ylim=c(0,0.2),
        title="Impact of \U00b1 5% Price Change on Conversion rate")

# -------------------------------------------------------------------------------------------------------------------------
# ELASCITY PLOT - ACTUALS VS PREDICTED
# -------------------------------------------------------------------------------------------------------------------------
factor = ifelse(b$NETPREMIUM>600,600,floor(b$NETPREMIUM/5)*5)

# Actual data
xtitle <- "Net premium"
factor_volume <- table(factor)
act_data = data.frame("Factor" = factor,
                      "PriceTest" = b$PREM_TEST,
                      "Sale" = br,
                      "Quote" = 1)

hist(factor)
function_actuals_v_predicted_elasticity(act_data = act_data)

#### FUNCTION 1

function_actuals_v_predicted_elasticity = function(act_data){
  # Actual Sale
  act_sale = aggregate(act_data$Sale, by=list(act_data$Factor, act_data$PriceTest), FUN=sum)
  
  # Actual Quote
  act_quote = aggregate(act_data$Quote, by=list(act_data$Factor,act_data$PriceTest), FUN=sum)
  
  # Actual Base
  act_base = as.matrix(act_sale[act_sale[,2]==0,3]/act_quote[act_quote[,2]==0,3])
  
  # Negative test
  act_minus = as.matrix(aggregate(act_sale[act_sale[,2]<0,3],by=list(act_sale[act_sale[,2]<0,1]),FUN=sum)$x/
                          aggregate(act_quote[act_quote[,2]<0,3],by=list(act_quote[act_quote[,2]<0,1]),FUN=sum)$x)
  act_dec =  (act_minus/act_base-1)/-0.02
  
  # Positive test
  act_plus = as.matrix(aggregate(act_sale[act_sale[,2]>0,3],by=list(act_sale[act_sale[,2]>0,1]),FUN=sum)$x/
                         aggregate(act_quote[act_quote[,2]>0,3],by=list(act_quote[act_quote[,2]>0,1]),FUN=sum)$x)
  act_inc = (act_plus/act_base-1)/0.02

  # Predicted
  pred_1 = aggregate(minus5, by=list(factor), FUN=mean)
  pred_2 = aggregate(base, by=list(factor), FUN=mean)
  pred_3 = aggregate(plus5, by=list(factor), FUN=mean)

  elasticity <- data.frame("Factor" = 
                             pred_2$Group.1,
                           "Actual.Dec." =
                             act_dec,
                           "Pred.Dec." =
                             as.matrix((pred_1[,2]/pred_2[,2]-1)/-0.05),
                           "Actual.Inc." =
                             act_inc,
                           "Pred.Inc." =
                             as.matrix((pred_3[,2]/pred_2[,2]-1)/0.05)
  )
  
  plot(elasticity$Factor,elasticity$Actual.Dec., type="l",
       xlab=xtitle, ylab="Negative Elasticity",
       main="Elasticity Actuals vs Predicted - Price Decrease - Conversion")
  lines(elasticity$Factor,elasticity$Pred.Dec.,col=2)
  legend("topright", legend=c("Actual","Predicted"), col=c("black","red"),
         lty=c(1,1), lwd=c(1,1), cex=0.9)
}


###### FUNCTION 2

# Elasticity plot function
ElasPlt <- function (actual=NA,base=NA,plus=NA,minus=NA,factor=NA,ylab=NA,xlab=NA,ylim=NA,xlim=NA,
                     title="Predicted Impact of \U00b1 5% Price Change",legend="topright") {
  
  xlim <- if(is.na(xlim)) {c(min(factor),max(factor))} else xlim
  plot(aggregate(actual, by=list(factor), FUN=mean),
       main=title, ylab=ylab,xlab=xlab, pch=20, ylim=ylim, xlim=xlim)
  lines(aggregate(base, by=list(factor), FUN=mean),col="blue", lwd=2)
  lines(aggregate(plus, by=list(factor), FUN=mean),col="red",lty=2, lwd=2)
  lines(aggregate(minus, by=list(factor), FUN=mean),col="green",lty=2, lwd=2)
  legend(legend,
         legend=c("Actual Top5%","Base Prediction", "+5% Price Change", "-5% Price Change"),
         col=c("black","blue","red","green"),
         lty=c(NA,1,2,2),
         lwd=c(NA,2,2,2),
         pch=c(20,NA,NA,NA),
         cex=0.9)
  
}
