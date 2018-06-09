titles = c("round((b$NETPREMIUM),0)"
           , "round(pmax(b$BUILD_YEAR,1800),0)"
           , "pmin(b$NUMBER_OF_BEDROOMS,7)" 
           , "pmin(b$NUMBER_OF_ROOMS,12)" 
           , "round(pmin(b$NUMBER_OF_BEDROOMS,7)/pmin(b$NUMBER_OF_ROOMS,12),2)" 
           , "pmin(b$NUMBER_BATHROOMS,5)" 
           , "pmin(b$DAYSTOINCEPTION,30)" 
           , "pmin(pmax(b$AGE,18),80)"
           , "ifelse(b$CREDITSCORE>998 | b$CREDITSCORE < 0,0,b$CREDITSCORE)" 
           , "pmin(b$NUM_ADULT_OCCUPANTS,5)"
           , "pmin(b$NUM_CHILD_OCCUPANTS,4)")

factors = list(round((b$NETPREMIUM),0)
               , round(pmax(b$BUILD_YEAR,1800),0)
               , pmin(b$NUMBER_OF_BEDROOMS,7) 
               , pmin(b$NUMBER_OF_ROOMS,12) 
               , round(pmin(b$NUMBER_OF_BEDROOMS,7)/pmin(b$NUMBER_OF_ROOMS,12),2) 
               , pmin(b$NUMBER_BATHROOMS,5) 
               , pmin(b$DAYSTOINCEPTION,30) 
               , pmin(pmax(b$AGE,18),80) 
               , ifelse(b$CREDITSCORE>998 | b$CREDITSCORE < 0,0,b$CREDITSCORE) 
               , pmin(b$NUM_ADULT_OCCUPANTS,5)
               , pmin(b$NUM_CHILD_OCCUPANTS,4))


for(i in 1:length(factors)){
  
  f0 = factors[[i]]
  
  ap1 <- aggregate(br, by=list(f0), FUN=mean)
  ap2 <- aggregate(b1p, by=list(f0), FUN=mean)
  plot(ap1, ylab="Response", xlab="Candidate Variable", main = titles[i])
  lines(ap2,col=2)
  
}


###############################


f0 = round((d$NETPREMIUM),0)
ap1 = sqldf("SELECT POL_DT_INCEPT, Insurer, ROUND(AVG(NETPREMIUM),0) as NETPREMIUM FROM d GROUP BY POL_DT_INCEPT, Insurer ORDER BY POL_DT_INCEPT")
head(ap1)

plot_ly(data = ap1
          , x = ~POL_DT_INCEPT) %>%
  
  add_trace(y = ~ NETPREMIUM
            , color = ~Insurer
            , hoverinfo = 'text'
            , text=~paste('Date:',POL_DT_INCEPT, '</br>'
              , '</br>Avg NETPREMIUM: £', NETPREMIUM
              , '</br>Brand:', Insurer)
            , mode='lines+markers'
            , type='scatter'
  ) %>%
  
  layout(title = paste('<b>Average Net Premium</b>')
         , xaxis = list(title='Quote Inception Date'
                        , showgrid = FALSE)
         , yaxis = list(title='Price (£)')
         , legend = list(y=-0.35
                         , orientation = 'h')
         , margin = list(b = 50, r = 70))


###############################

# FEBRUARY
# b1p = 1 - 1/((1 + q * exp(-((coef(b1)[1]+febm%*%as.matrix(coef(b1)[2:length(coef(b1))])))))^(1/q))
febre1 = as.matrix(ranef(b1)$re1[match(ifelse(feb$POL_DT_INCEPT > '2018-01-12',1,0), rownames(as.matrix(ranef(b1)$re1))),])
febre2 = as.matrix(ranef(b1)$re2[match(ifelse(feb$POL_DT_INCEPT > '2018-02-27',1,0), rownames(as.matrix(ranef(b1)$re2))),])

b1p = 1 - 1/((1 + q * exp(-(febre1
                            +febre1
                            +(fixef(b1)[1]+febm%*%as.matrix(fixef(b1)[2:length(fixef(b1))])))))^(1/q))


f0 = round(log((feb$NETPREMIUM)),2)
f1 = feb$Insurer
res = feb$SALE
ap1 = aggregate(res, by=list(f0,f1), FUN=mean)
ap2 = aggregate(b1p, by=list(f0,f1), FUN=mean)

head(ap1)
head(ap2)

hist(f0)

plot(x = subset(ap1, Group.2 == "AXA")[,c("Group.1")]
     , y = subset(ap1, Group.2 == "AXA")[,c("x")]
     , ylab="Response"
     , xlab="Candidate Variable"
     , main = "AXA Jan - Feb")
lines(x = subset(ap2, Group.2 == "AXA")[,c("Group.1")]
      , y = subset(ap2, Group.2 == "AXA")[,c("V1")]
      , col=2)

plot(x = subset(ap1, Group.2 == "Advantage")[,c("Group.1")]
     , y = subset(ap1, Group.2 == "Advantage")[,c("x")]
     , ylab="Response"
     , xlab="Candidate Variable"
     , main = "Advantage Jan - Feb")
lines(x = subset(ap2, Group.2 == "Advantage")[,c("Group.1")]
      , y = subset(ap2, Group.2 == "Advantage")[,c("V1")]
      , col=2)


# MARCH
# b1p = 1 - 1/((1 + q * exp(-((coef(b1)[1]+marchm%*%as.matrix(coef(b1)[2:length(coef(b1))])))))^(1/q))
marre1 = as.matrix(ranef(b1)$re1[match(ifelse(march$POL_DT_INCEPT > '2018-01-12',1,0), rownames(as.matrix(ranef(b1)$re1))),])
marre2 = as.matrix(ranef(b1)$re2[match(ifelse(march$POL_DT_INCEPT > '2018-02-27',1,0), rownames(as.matrix(ranef(b1)$re2))),])

b1p = 1 - 1/((1 + q * exp(-(marre1
                            +marre2
                            +(fixef(b1)[1]+marchm%*%as.matrix(fixef(b1)[2:length(fixef(b1))])))))^(1/q))



f0 = round(log((march$NETPREMIUM)),2)
f1 = march$Insurer
res = march$SALE
ap1 = aggregate(res, by=list(f0,f1), FUN=mean)
ap2 = aggregate(b1p, by=list(f0,f1), FUN=mean)

head(ap1)
head(ap2)

hist(f0)

plot(x = subset(ap1, Group.2 == "AXA")[,c("Group.1")]
     , y = subset(ap1, Group.2 == "AXA")[,c("x")]
     , ylab="Response"
     , xlab="Candidate Variable"
     , main = "AXA Champ")
lines(x = subset(ap2, Group.2 == "AXA")[,c("Group.1")]
      , y = subset(ap2, Group.2 == "AXA")[,c("V1")]
      , col=2)

plot(x = subset(ap1, Group.2 == "Advantage")[,c("Group.1")]
     , y = subset(ap1, Group.2 == "Advantage")[,c("x")]
     , ylab="Response"
     , xlab="Candidate Variable"
     , main = "Advantage March")
lines(x = subset(ap2, Group.2 == "Advantage")[,c("Group.1")]
      , y = subset(ap2, Group.2 == "Advantage")[,c("V1")]
      , col=2)


# CHAMPION
# b1p = 1 - 1/((1 + q * exp(-((coef(b1)[1]+champm%*%as.matrix(coef(b1)[2:length(coef(b1))])))))^(1/q))

champre1 = as.matrix(ranef(b1)$re1[match(ifelse(champion$POL_DT_INCEPT > '2018-01-12',1,0), rownames(as.matrix(ranef(b1)$re1))),])
champre2 = as.matrix(ranef(b1)$re2[match(ifelse(champion$POL_DT_INCEPT > '2018-02-27',1,0), rownames(as.matrix(ranef(b1)$re2))),])

b1p = 1 - 1/((1 + q * exp(-(champre1
                            +champre2
                            +(fixef(b1)[1]+champm%*%as.matrix(fixef(b1)[2:length(fixef(b1))])))))^(1/q))


f0 = round(log((champion$NETPREMIUM)),2)
f1 = champion$Insurer
res = champion$SALE
ap1 = aggregate(res, by=list(f0,f1), FUN=mean)
ap2 = aggregate(b1p, by=list(f0,f1), FUN=mean)

head(ap1)
head(ap2)

plot(x = subset(ap1, Group.2 == "AXA")[,c("Group.1")]
     , y = subset(ap1, Group.2 == "AXA")[,c("x")]
     , ylab="Response"
     , xlab="Candidate Variable"
     , main = "AXA Champion")
lines(x = subset(ap2, Group.2 == "AXA")[,c("Group.1")]
      , y = subset(ap2, Group.2 == "AXA")[,c("V1")]
      , col=2)

plot(x = subset(ap1, Group.2 == "Advantage")[,c("Group.1")]
     , y = subset(ap1, Group.2 == "Advantage")[,c("x")]
     , ylab="Response"
     , xlab="Candidate Variable"
     , main = "Advantage Champion")
lines(x = subset(ap2, Group.2 == "Advantage")[,c("Group.1")]
      , y = subset(ap2, Group.2 == "Advantage")[,c("V1")]
      , col=2)


# CHALLENGER
# b1p = 1 - 1/((1 + q * exp(-((coef(b1)[1]+challm%*%as.matrix(coef(b1)[2:length(coef(b1))])))))^(1/q))

challpre1 = as.matrix(ranef(b1)$re1[match(ifelse(challenger$POL_DT_INCEPT > '2018-01-12',1,0), rownames(as.matrix(ranef(b1)$re1))),])
challpre2 = as.matrix(ranef(b1)$re2[match(ifelse(challenger$POL_DT_INCEPT > '2018-02-27',1,0), rownames(as.matrix(ranef(b1)$re2))),])

b1p = 1 - 1/((1 + q * exp(-(challpre1
                            +challpre2
                            +(fixef(b1)[1]+challm%*%as.matrix(fixef(b1)[2:length(fixef(b1))])))))^(1/q))


f0 = round(log((challenger$NETPREMIUM)),2)
f1 = challenger$Insurer
res = challenger$SALE
ap1 = aggregate(res, by=list(f0,f1), FUN=mean)
ap2 = aggregate(b1p, by=list(f0,f1), FUN=mean)

head(ap1)
head(ap2)

plot(x = subset(ap1, Group.2 == "AXA")[,c("Group.1")]
     , y = subset(ap1, Group.2 == "AXA")[,c("x")]
     , ylab="Response"
     , xlab="Candidate Variable"
     , main = "AXA Challenger")
lines(x = subset(ap2, Group.2 == "AXA")[,c("Group.1")]
      , y = subset(ap2, Group.2 == "AXA")[,c("V1")]
      , col=2)

plot(x = subset(ap1, Group.2 == "Advantage")[,c("Group.1")]
     , y = subset(ap1, Group.2 == "Advantage")[,c("x")]
     , ylab="Response"
     , xlab="Candidate Variable"
     , main = "Advantage Challenger")
lines(x = subset(ap2, Group.2 == "Advantage")[,c("Group.1")]
      , y = subset(ap2, Group.2 == "Advantage")[,c("V1")]
      , col=2)
