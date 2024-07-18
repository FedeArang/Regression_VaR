## DATA PROCESSING ##

break_even = read.csv("T10YIE.csv")
be = as.numeric(break_even$T10YIE)

#fill NA by sampling values from a normal distribution with mean the mean of the available values
be[is.na(be)] = rnorm( n=sum(is.na(be)) , mean=mean(be[is.na(be)==FALSE]),  sd=0*sd(be[is.na(be)==FALSE]) )
sum(is.na(be)) #0

summary(be)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.040   1.760   2.110   2.012   2.300   3.314 

#time series of be
plot(be)

#histogram/distribution of be
hist(be)

qqnorm(be)

#be is not exactly normal, in particular it is left skewed

###

ty_10_full = read.csv("DGS10.csv")
ty_10 = as.numeric(ty_10_full$DGS10)

#fill NA by sampling values from a normal distribution with mean the mean of the available values
ty_10[is.na(ty_10)] = rnorm( n=sum(is.na(ty_10)) , mean=mean(ty_10[is.na(ty_10)==FALSE]),  sd=0*sd(ty_10[is.na(ty_10)==FALSE]) )
sum(is.na(ty_10)) #0

summary(ty_10)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.520   1.910   2.500   2.569   3.210   4.980

#time series of be
plot(ty_10)

#histogram/distribution of be
hist(ty_10)

qqnorm(ty_10)

#this time the data looks more Gaussian also from the qqplot, apart from
#a small skew on the left

###

ty_5_full = read.csv("DGS5.csv")
ty_5 = as.numeric(ty_5_full$DGS5)

#fill NA by sampling values from a normal distribution with mean the mean of the available values
ty_5[is.na(ty_5)] = rnorm( n=sum(is.na(ty_5)) , mean=mean(ty_5[is.na(ty_5)==FALSE]),  sd=0*sd(ty_5[is.na(ty_5)==FALSE]) )
sum(is.na(ty_5)) 

summary(ty_5)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.190   1.250   1.760   1.948   2.530   4.950 

#time series of be
plot(ty_5)

#histogram/distribution of be
hist(ty_5)

qqnorm(ty_5)

#data is right skewed in this case


## REGRESSION ##

#data processing, removing noisy outliers
data = data.frame(be = be, ty_10 = ty_10)
data_new = data.frame(be = be[log(ty_10)>0.25& be>=1.25], ty_10 = ty_10[log(ty_10)>0.25& be>=1.25])
data_new = data.frame(be = data_new$be[!( log(data_new$ty_10)<0.8 & data_new$be>2)], ty_10 = data_new$ty_10[!( log(data_new$ty_10)<0.8 & data_new$be>2)])
data_new = data.frame(be = data_new$be[data_new$be<=2.6], ty_10 = data_new$ty_10[data_new$be<=2.6])
data_new = data.frame(be = data_new$be[!( log(data_new$ty_10)>0.9 & data_new$be<1.9)], ty_10 = data_new$ty_10[!( log(data_new$ty_10)>0.9 & data_new$be<1.9)])

fm = lm(log(ty_10) ~ be, data = data_new)
summary(fm)

plot(data_new$be, log(data_new$ty_10))
#plot(data_new$be, data_new$ty_10)
abline(fm)

plot(fm, which = c(1,2,3))

#residuals are almost normal and centered around 0.

## VAR ##

# Compute bond prices

# Function to compute historical VaR and contributions
# As holding period is 1 month, we assume roughly 22 business days in one month

compute_historical_var <- function(yield_10y, yield_5y, face_value, qtl=0.99, holding_period=22) {
  # Compute bond prices
  prices_10y <- face_value / (1 + yield_10y)^10
  prices_5y <- face_value / (1 + yield_5y)^5
  
  # Compute portfolio value
  portfolio_values <- prices_10y + prices_5y
  
  # Compute holding period returns
  holding_period_returns <- (portfolio_values[(holding_period + 1):length(portfolio_values)] /
                               portfolio_values[1:(length(portfolio_values) - holding_period)]) - 1
  
  # Calculate the VaR at the 99% confidence level
  var_99 <- quantile(holding_period_returns, 1 - qtl)
  
  # Contribution to VaR - removing 10-year bond
  portfolio_values_no_10y <- prices_5y
  holding_period_returns_no_10y <- (portfolio_values_no_10y[(holding_period + 1):length(portfolio_values_no_10y)] /
                                      portfolio_values_no_10y[1:(length(portfolio_values_no_10y) - holding_period)]) - 1
  var_99_no_10y <- quantile(holding_period_returns_no_10y, 1 - qtl)
  
  # Contribution to VaR - removing 5-year bond
  portfolio_values_no_5y <- prices_10y
  holding_period_returns_no_5y <- (portfolio_values_no_5y[(holding_period + 1):length(portfolio_values_no_5y)] /
                                     portfolio_values_no_5y[1:(length(portfolio_values_no_5y) - holding_period)]) - 1
  var_99_no_5y <- quantile(holding_period_returns_no_5y, 1 - qtl)
  
  # Contribution calculation
  contribution_10y <- var_99 - var_99_no_10y
  contribution_5y <- var_99 - var_99_no_5y
  
  list(
    VaR_99 = var_99,
    Contribution_10y = contribution_10y,
    Contribution_5y = contribution_5y
  )
}



# Face value of the bonds
face_value <- 1e6

# Compute historical VaR and contributions
result <- compute_historical_var(ty_10, ty_5, face_value)
result

#VaR_99: -0.8974389 
#Contribution_10y: 2.557669e-05 
#Contribution_5y: 0.04510842