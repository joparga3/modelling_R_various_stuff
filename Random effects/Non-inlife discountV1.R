##############################################################################################################################
#
#  LOAD REQUIRED PACKAGES
#
##############################################################################################################################

require(earth)
require(lme4)
require(ROCR)
require(caret)
require(data.table)
require(plyr)

source('/home/garciaj/Car/Renewals/UDF_list.R')

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

##############################################################################################################################
#
#  LOAD DATA
#
##############################################################################################################################

# RENEWALS EXTRACT
# -------------------------------------------------------------------------------------------------------

# d_raw = fread('/home/garciaj/Car/Renewals/RNExtractFeb17_Feb18.csv', sep = ',')
# 
# d_raw = as.data.frame(d_raw)
# d_raw = d_raw[,sort(colnames(d_raw))]
# head(d_raw)
# 
# # There are duplicate records because of the dataset being run for more than a year
# # sqldf('SELECT Pol_Ref, COUNT(*) FROM d_raw GROUP BY Pol_Ref HAVING COUNT(*) > 1')
# # subset(d_raw, Pol_Ref == '157028/5')
# 
# duplicated_pols = sqldf('SELECT Pol_Ref, COUNT(*) FROM d_raw GROUP BY Pol_Ref HAVING COUNT(*) > 1 ORDER BY Pol_Ref')
# d_raw$duplicated_row = ifelse(d_raw$Pol_Ref %in% duplicated_pols$Pol_Ref, 1, 0)
# head(d_raw)
# 
# # To fix this duplication we rank over pol_dt_renew data by pol_ref, then subset by the latest quote
# d_raw = d_raw[order(d_raw$Pol_Ref),]
# d_raw$Order.by.group = unlist(with(d_raw, tapply(Pol_Dt_Renew, Pol_Ref, function(x) rank(x, ties.method= "first"))))
# 
# d_raw$remove_row = ifelse(d_raw$duplicated_row == 1 & d_raw$Order.by.group == 1, 1, 0)
# d_raw = subset(d_raw, remove_row == 0)
# 
# d_raw$duplicated_row = NULL
# d_raw$Order.by.group = NULL
# d_raw$remove_row = NULL
#
# write.csv(d_raw, '/home/garciaj/Car/Renewals/RNExtractFeb17_Feb18_clean.csv', row.names = FALSE)

# subset(d_raw, Pol_Ref == '157028/5')

d_raw = fread('/home/garciaj/Car/Renewals/RNExtractFeb17_Feb18_clean.csv', sep = ',')
d_raw = as.data.frame(d_raw)
d_raw = d_raw[,sort(colnames(d_raw))]
head(d_raw)
# sqldf('SELECT Pol_Ref, COUNT(*) FROM d_raw1 GROUP BY Pol_Ref HAVING COUNT(*) > 1')


# MARKET PRICE PREDICTION EXTRACT
# -------------------------------------------------------------------------------------------------------
market_pred = fread('/home/garciaj/Car/Renewals/RNExtractFeb17_Feb18.csv_Predictions.csv', sep = ',')
market_pred = as.data.frame(market_pred)
head(market_pred)

# There are duplicate records because of the dataset being run for more than a year
# sqldf('SELECT pol_ref, COUNT(*) FROM market_pred GROUP BY pol_ref HAVING COUNT(*) > 1')
# subset(market_pred, pol_ref == '157028/5')

# Merge both datasets
d = merge(x = d_raw, y = market_pred, by.x = c("Pol_Ref"), by.y = c("pol_ref"))
head(d)

# Given that we cant get an updated version of the market predictions quickly, we will work knowing that 7k rows
# out of 1.7 million are duplicated
# nrow(d) - nrow(d_raw)

##############################################################################################################################
#
#  PREPROCESSING
#
##############################################################################################################################

# ----------------------------------------------------------------------------------------------------------------------------
# NAs and columns to omit
# ----------------------------------------------------------------------------------------------------------------------------
# Checking for NAs
na_count_d_extract = sapply(d, function(y) sum(length(which(is.na(y)))))
na_count_d_extract = data.frame(na_count_d_extract)
na_count_d_extract

d$Veh_Grp_Old = NULL
d$Veh_Grp_Adv_Old = NULL
d$Veh_Doors = NULL
d$SEVERITY = NULL
d$NO_CANX = NULL
d$MIDTERM_CANX = NULL
d$COOLOFF_CANX = NULL
d$BrandBPViews = NULL
d$AvgTop5Prem = NULL
d$Area_Mosaic_Type = NULL
d$Area_Grp_Adv_Old = NULL

# ----------------------------------------------------------------------------------------------------------------------------
# Correct and Transform variableS
# ----------------------------------------------------------------------------------------------------------------------------

# As Date
# ----------------------------------------------------
d$Pol_Dt_Renew <- as.Date(d$Pol_Dt_Renew)
d$Pol_Dt_Incept <- as.Date(d$Pol_Dt_Incept)

# Subsetting
# ----------------------------------------------------
d <- subset(d, is.na(d$NBPrice_Gross) == FALSE)
d <- subset(d, d$NBPrice_Gross <= 6000)
d <- subset(d, d$Prem_Gross <= 6000)
d <- subset (d, d$Prem_Gross_PY > 60)

d$inlife <- ifelse(d$Pol_MTA_Num == 0 & d$Clm_Num_L1Y == 0, 0, 1)
d <- subset(d, d$inlife == 0)

# # ET creation
# d$ET_Prem_Gross <- log(d$Prem_Gross)
# d$ET_Prem_Net <- log(d$Prem_Net)
# d$ET_Market_Price <- round_any(log(d$MarketPrice_Predicted),0.01)
# # d$ET_NBPrice_Net <- log(d$NBPrice_Net)
# d$ET_NBPrice_Gross <- round_any(log(d$NBPrice_Gross),0.01)
# # d$ET_Prem_Gross_PY <- log(d$Prem_Gross_PY)
# # d$ET_Prem_Net_PY <- log(d$Prem_Net_PY)
# # d$ET_netyoy <- log(d$Prem_Net / d$Prem_Net_PY)
# # d$ET_Prem_Gross_YoY <- d$ET_Prem_Gross/d$ET_Prem_Gross_PY
# # d$ET_NetNB_ratio <- d$ET_Prem_Net/d$ET_NBPrice_Net
# d$ET_gross_to_market <- round_any(log(d$Prem_Gross/d$MarketPrice_Predicted),0.01)
# d$ET_NB_to_market <- round_any(log(d$NBPrice_Gross/d$MarketPrice_Predicted),0.01)

# Extra variables
# ----------------------------------------------------
d$credit_score <-  ifelse(is.na(ifelse(d$DE_Credit_Score>999|d$DE_Credit_Score<=0,0,d$DE_Credit_Score)),0,ifelse(d$DE_Credit_Score>999|d$DE_Credit_Score<=0,0,d$DE_Credit_Score))

# IPT (Insurance Premium Taxes) rates
# Rates	
# From 1 June 2017	
# From 1 October 2016 to 31 May 2017	
# From 1 November 2015 to 30 September 2016	
# From 4 January 2011 to 31 October 2015	
# Up to 3 January 2011
# 
# Standard rate	12%	10%	9.5%	6%	5%
# Higher rate	20%	20%	20%	20%	17.5%

d$ipt_rate = ifelse(d$Pol_Dt_Renew <'2015-11-01', 6
                    ,ifelse(d$Pol_Dt_Renew >= '2015-11-01' & d$Pol_Dt_Renew<'2016-10-01', 9.5
                            ,ifelse(d$Pol_Dt_Renew >= '2016-10-01' & d$Pol_Dt_Renew<'2017-06-01', 10
                                    , 12)))

# Ratio variables - CTM fit
# ----------------------------------------------------
d$netyoy <- pmax(pmin(d$Prem_Net / d$Prem_Net_PY,2),0.5)
d$net_yoy_change <- log(d$Prem_Net/d$Prem_Net_PY)
d$yoy_change <- log(d$Prem_Gross / d$Prem_Gross_PY)
d$prem_to_NB <- log(d$Prem_Gross/d$NBPrice_Gross)
d$net_to_NB <- log(d$Prem_Net/d$NBPrice_Net)
d$commission <- (d$Prem_Gross/d$Prem_Net)-1
d$commission_PY <- (d$Prem_Gross_PY/d$Prem_Net_PY)-1

##### CHANGE PREM_GROSS TO STRIP PREM TEST OUT
# d$Prem_Gross = d$Prem_Gross - d$Prem_Net*d$Prem_Test_Intent

# Trying to subset for more recent data to check if that yields any improvements
# d = subset(d, Pol_Dt_Renew >= '2017-10-01')

# Data breaches flags
d$data_breach_flags = ifelse(as.character(d$Pol_Dt_Renew) %in% c('2017-10-12'
                                                                 ,'2017-10-13'
                                                                 ,'2017-10-21'
                                                                 ,'2017-10-25'
                                                                 ,'2017-10-26'
                                                                 ,'2017-10-27'
                                                                 ,'2017-10-28'
                                                                 ,'2017-11-08'
                                                                 ,'2017-11-09'
                                                                 ,'2017-11-10'
                                                                 ,'2017-11-11'
                                                                 ,'2017-11-12'
                                                                 ,'2017-11-13'
                                                                 ,'2017-11-14'
                                                                 ,'2017-11-16'
                                                                 ,'2017-11-17'
                                                                 ,'2017-11-20'
                                                                 ,'2017-11-21'
                                                                 ),1,0)

# Subset data breach flags?
# d = subset(d, data_breach_flags != 1)

# Response
# ----------------------------------------------------
d$Response <- ifelse(d$Pol_Sale_Type=='RD', 1, 0)

##############################################################################################################################
#
#  DATASET SPLIT
#
##############################################################################################################################

set.seed(1066)

totrows = nrow(d)
rownos = seq(totrows)
parts = c(0.8,0.2)

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
rec = d[which(difftime(max(d$Pol_Dt_Renew),d$Pol_Dt_Renew,units="days")<30),] # Check

rm(totrows,rownos,parts,i,sets)

br = b$Response
tr = t$Response
recr = rec$Response

##############################################################################################################################
#
#  QUICK MARS
#
##############################################################################################################################
# f_i = ~ I(log(Prem_Gross/Prem_Gross_PY)) +
#         I(log(Prem_Gross/NBPrice_Gross)) +
#         I(log(NBPrice_Gross / MarketPrice_Predicted)) +
#         I(log(Prem_Gross)) +
#         I(Max_Allowed_Discount) + 
#         I(Cust_Age_1) +
#         I(credit_score) + 
#         I(ifelse(PaymentType == "I", 1, 0)) +
#         I(Pol_Tenure) + 
#         I(ipt_rate) + 
#         I(data_breach_flags) +
#         -1
# 
# bm_i = model.matrix(f_i, data = b, sparse = T)    
# head(bm_i)
# 
# e = earth(bm_i, br, nk = 200, degree = 2, fast.k = 20, thresh = 0.0005, #edit to 0.01 for testing
#           newvar.penalty = 0.01, glm = list(family=binomial), keepxy=T, Use.beta.cache=T, trace=1)
# summary(e)
# evimp(e)
# 
# b1p = predict(e, type = "response")
# r1 = data.frame(cbind(br,b1p))
# head(r1)



##############################################################################################################################
#
#  BUILD MODEL WITH TRAINING DATA
#
##############################################################################################################################

# Formula
# ------------------------------------------------------------------------------------
f = ~ 
      # YOY CHANGE PREM GROSS
      I(pmax(log(Prem_Gross/Prem_Gross_PY),0.5)) +
      I(pmin(log(Prem_Gross/Prem_Gross_PY),0.6)) +
  
      # GROSS PREMIUM / NB PRICE GROSS
      I(pmax(log(Prem_Gross/NBPrice_Gross),0.2)) +
      I(pmin(log(Prem_Gross/NBPrice_Gross),0.2)) +

      # NB PRICE GROSS / MARKET PRICE
      I(pmax(log(NBPrice_Gross / MarketPrice_Predicted),-0.4)) + 
      
      # PREM GROSS THIS YEAR
      I(pmin(log(Prem_Gross),5.2)) +
      I(pmin(log(Prem_Gross),5.9)) +
      I(pmax(log(Prem_Gross),5.5)) +
      
      # MAX ALLOWED DISCOUNT
      I(Max_Allowed_Discount) +
      I(pmax(Max_Allowed_Discount,50)) +
  
      # # TENURE WITH GROSS YOY
      # I(pmax(Pol_Tenure,2)*(round(log(Prem_Gross / Prem_Gross_PY),2))) + 
      # I(pmin(Pol_Tenure,2)*(round(log(Prem_Gross / Prem_Gross_PY),2))) + 
  
      - 1

# Model matrix
# ------------------------------------------------------------------------------------
bm = model.matrix(f, data = b, sparse = T)  
# tm = model.matrix(f, data = t, sparse = T)  
# recm = model.matrix(f, data = rec, sparse = T)  
head(bm)

# Model build
# ------------------------------------------------------------------------------------
b1 = glm(br ~ bm, family = binomial(link= "logit"))
summary(b1)

re1 = ifelse(b$Pol_Dt_Renew > '2017-09-24',1,0)
re2 = ifelse(b$Pol_Dt_Renew < '2017-09-24',1,0)
re3 = ifelse(b$Pol_Dt_Renew > '2018-01-01',1,0)

b1 = glmer(br ~ bm 
           + (1|re1)
           + (1|re2)
           # + (1|re3)
           , data=b
           , binomial(link= "logit"), verbose = 1, nAGQ = 0)
summary(b1)

##############################################################################################################################
#
#  KPIs - Build set
#
##############################################################################################################################

# Predictions
# ------------------------------------------------------------------------------------
b1p = predict(b1, type = "response", re.form=NULL, newdata = b)
r1 = data.frame(cbind(br,b1p))
head(r1)

# Avg error and Gini
# ------------------------------------------------------------------------------------
# (mean(b1p)/mean(b$Response) - 1)*100
as.numeric(performance(prediction(b1p,r1$br),"auc")@y.values)*2-1

##############################################################################################################################
#
#  Actuals vs predicted renewals over time
#
##############################################################################################################################

UDF_act_pred_overtime(b$Pol_Dt_Renew,br,b1p,xlabel="Renewal Date")

##############################################################################################################################
#
#  PLOTTING ELASTICITY EARNIX
#
##############################################################################################################################

# 0% Flex
e = b
e$Prem_Test_Intent = 0.00
e$Prem_Gross = ((e$Prem_Gross/e$Prem_Net-1)+1)*e$Prem_Net
em = model.matrix(f, data = e, sparse = T)  

base = as.vector(1/(1+exp(-(b1$coefficients[1] + em %*% b1$coefficients[2:length(b1$coefficients)]))))


# +5% Flex
e = b
e$Prem_Test_Intent = 0.05
e$Prem_Gross = ((e$Prem_Gross/e$Prem_Net-1)+1.05)*e$Prem_Net
em = model.matrix(f, data = e, sparse = T)  

plus5 = as.vector(1/(1+exp(-(b1$coefficients[1] + em %*% b1$coefficients[2:length(b1$coefficients)]))))

# -5% Flex
e = b
e$Prem_Test_Intent = -0.05
e$Prem_Gross = ((e$Prem_Gross/e$Prem_Net-1)+0.95)*e$Prem_Net
em = model.matrix(f, data = e, sparse = T)  

minus5 = as.vector(1/(1+exp(-(b1$coefficients[1] + em %*% b1$coefficients[2:length(b1$coefficients)]))))

head(base)
head(plus5)
head(minus5)


factor = round(log(b$Prem_Gross/b$NBPrice_Gross),2)
#factor = ifelse(b$Prem_Gross>2000,2000,floor(b$Prem_Gross/10)*10)
#factor = round(log(b$Prem_Gross/b$Prem_Gross_PY),2)


hist(factor)
ElasPlt(actual=br,base=base,plus=plus5,minus=minus5,
        factor=factor,
        ylab="Auto Renewal %",xlab="Factor",ylim=c(0,0.6),
        title="Impact of \U00b1 5% Price Change on No-Inlife Auto Renewal")

##############################################################################################################################
#
#  PLOTTING ELASTICITY ACTUALS VS PREDICTED
#
##############################################################################################################################

factor = round_any(pmax(-1,pmin(1, (log(b$Prem_Gross/b$NBPrice_Gross)))), 0.05)

xtitle <- "Prem Gross / NBPrice Gross"

factor_volume <- table(factor)
act_data <- data.frame("Factor" = factor,
                       "PriceTest" = b$Prem_Test_Intent,
                       "Retain" = b$Response,
                       "Invite" = 1)

hist(factor)
function_actuals_v_predicted_elasticity(act_data = act_data)


#### FUNCTION 1

function_actuals_v_predicted_elasticity = function(act_data){
  act_retain <- aggregate(act_data$Retain, by=list(act_data$Factor,act_data$PriceTest), FUN=sum)
  act_invite <- aggregate(act_data$Invite, by=list(act_data$Factor,act_data$PriceTest), FUN=sum)
  
  # Base
  act_base <- as.matrix(act_retain[act_retain[,2]==0,3]/act_invite[act_invite[,2]==0,3])
  
  # Negative test
  act_ntest <- as.matrix(sapply(split(act_invite[act_invite[,2]<0,],act_invite[act_invite[,2]<0,1]),
                                function(x) weighted.mean(x$Group.2,x$x)))
  
  act_minus <- as.matrix(aggregate(act_retain[act_retain[,2]<0,3],by=list(act_retain[act_retain[,2]<0,1]),FUN=sum)$x/
                           aggregate(act_invite[act_invite[,2]<0,3],by=list(act_invite[act_invite[,2]<0,1]),FUN=sum)$x)
  
  act_dec <- (act_minus/act_base-1)/act_ntest
  
  # Positive test
  act_ptest <- as.matrix(sapply(split(act_invite[act_invite[,2]>0,],act_invite[act_invite[,2]>0,1]),
                                function(x) weighted.mean(x$Group.2,x$x)))
  
  act_plus <- as.matrix(aggregate(act_retain[act_retain[,2]>0,3],by=list(act_retain[act_retain[,2]>0,1]),FUN=sum)$x/
                          aggregate(act_invite[act_invite[,2]>0,3],by=list(act_invite[act_invite[,2]>0,1]),FUN=sum)$x)
  
  act_inc <- (act_plus/act_base-1)/act_ptest
  
  # Predicted
  pred_1 <- aggregate(minus5, by=list(factor), FUN=mean)
  pred_2 <- aggregate(base, by=list(factor), FUN=mean)
  pred_3 <- aggregate(plus5, by=list(factor), FUN=mean)
  
  elasticity <- data.frame("Actual.Dec." =
                             act_dec,
                           "Pred.Dec." =
                             as.matrix((pred_1[,2]/pred_2[,2]-1)/-0.05),
                           "Actual.Inc." =
                             act_inc,
                           "Pred.Inc." =
                             as.matrix((pred_3[,2]/pred_2[,2]-1)/0.05)
  )
  
  #sma_actual <- sma(elasticity$Actual.Dec., order = 30, level = 0.95, silent = "all")
  
  par(mar = c(4, 4.5, 4, 4) + 0.3)
  plot(row.names(elasticity),elasticity$Actual.Dec., type="l", ylim = c(-10,5),
       xlab=xtitle, ylab="Elasticity",
       main="Elasticity AvP - Non-inlife autorenew",col='Dark red',lty = 2, lwd=1.5,cex.main=1,cex.lab=1)
  lines(row.names(elasticity),elasticity$Actual.Inc, lty = 2, col='Dark green', lwd=1.5)
  lines(row.names(elasticity),elasticity$Pred.Dec.,col=2,lwd=1.5)
  lines(row.names(elasticity),elasticity$Pred.Inc.,col=3,lwd=1.5)
  par(new = TRUE)
  plot(row.names(factor_volume), factor_volume, type = "l"
       , axes = FALSE, bty = "n", xlab = "", ylab = "", col = ' dark grey',lwd=1.5)
  axis(side=4, at = pretty(range(factor_volume)))
  mtext("Volume", side=4, line=3,cex=1)
  par(mar=c(0, 0, 0, 0))
  # layout(rbind(1,2), heights=c(7,1))
  # legend("left",'groups', legend=c("Actual Dec", "Actual Inc", "Predicted Dec", "Predicted Inc","Volume")
  #        , col=c('Dark red','Dark green',2,3,' dark grey'),
  #        lty=c(2,2,1,1,1), lwd=c(1.5,1.5,1.5,1.5), cex=1,bty ="n",ncol=3)
}

####################

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
