# Visualise variables
# f0s
# YOY CHANGE PREM GROSS
f0 = round(log(b$Prem_Gross / b$Prem_Gross_PY),2)

# GROSS PREMIUM / NB PRICE GROSS
f0 = round(log(b$Prem_Gross/b$NBPrice_Gross),2)

# NB PRICE GROSS / MARKET PRICE
f0 = round(log(b$NBPrice_Gross / b$MarketPrice_Predicted),2)

# PREM GROSS THIS YEAR
f0 = round(log(b$Prem_Gross),2)

# MAX ALLOWED DISCOUNT
f0 = b$Max_Allowed_Discount

# POL TENURE
f0 = b$Pol_Tenure

# CUST AGE
f0 = b$Cust_Age_1

# YOY PREM GROSS CHANGE AND TENURE
f0 = round(log(b$Prem_Gross / b$Prem_Gross_PY)*b$Pol_Tenure,2)

f0 = b$data_breach_flags
f0 = round(b$Prem_Gross - b$Prem_Net*b$Prem_Test_Intent,-1)

# Actual vs. Predicted
#par(mfrow=c(1,2))
hist(f0)
ap1 <- aggregate(br, by=list(f0), FUN=mean)
ap2 <- aggregate(b1p, by=list(f0), FUN=mean)
plot(ap1, ylab="Response", xlab="Candidate Variable")
lines(ap2,col=2)







