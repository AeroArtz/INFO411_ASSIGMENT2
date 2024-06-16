install.packages("pacman")
install.packages("changepoint")
install.packages("TTR")

library("pacman")
library(tidyverse)
library(forecast)
library("changepoint")
library("TTR")
library(ggplot2)

#Preparing data ----
df1 <- read.csv("sample1_sales_timeseries.csv", header = TRUE, 
                colClasses = c(id="numeric", date="Date", store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))

df2 <- read.csv("sample2_sales_timeseries.csv", header = TRUE, 
                colClasses = c(id="numeric", date="Date", store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))

df3 <- read.csv("sample3_sales_timeseries.csv", header = TRUE, 
                colClasses = c(id="numeric", date="Date", store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))

df4 <- read.csv("sample4_sales_timeseries.csv",  header = TRUE, 
                colClasses = c(id="numeric", date="Date", store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))

df5 <- read.csv("sample5_sales_timeseries.csv",  header = TRUE, colClasses = c(id="numeric", date="Date", store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))


colnames(df3)

View(df1)


#binding all the files
df <- rbind(df1,df2,df3,df4,df5)
names(df)
df<-df %>%
  arrange(date)
View(df)


str(df1)
str(df2)
str(df3)
str(df4)
str(df5)

names(df)


#Grouping by date

df_ver2<-df %>%
  group_by(date, family) %>%
  summarise(TOTAL_SALES = sum(sales))%>%
  ungroup()

#Normalization ----

cpi_lookup <- data.frame(year = c(2013, 2014, 2015),
                         cpi = c(95.84, 99.28, 103.22))

reference_cpi<- 95.84
df_ver2_normalized <- df_ver2
df_ver2_normalized$year <- lubridate::year(df_ver2$date)

df_ver2_normalized <-  merge(df_ver2_normalized, cpi_lookup, 
                             by.x = "year", by.y = "year", all.x = TRUE)

df_ver2_normalized <- df_ver2_normalized%>% arrange(date)

df_ver2_normalized$NORMALIZED_SALES <- 
  df_ver2_normalized$TOTAL_SALES * (reference_cpi / df_ver2_normalized$cpi)

df_ver2 <- df_ver2_normalized

#separating dataframes

df_frozen_foods<- df_ver2 %>%
  filter(family=='FROZEN FOODS')

df_liquor_wine_beer<- df_ver2 %>%
  filter(family=='LIQUOR,WINE,BEER', date<=as_date('2015-02-08'))

df_meats<- df_ver2 %>%
  filter(family=='MEATS')

df_poultry<- df_ver2 %>%
  filter(family=='POULTRY')

df_prepared_foods<- df_ver2 %>%
  filter(family=='PREPARED FOODS')

#checking out data

summary(df_frozen_foods)
str(df_frozen_foods)

summary(df_liquor_wine_beer)
str(df_liquor_wine_beer)

summary(df_meats)
str(df_meats)

summary(df_poultry)
str(df_poultry)

summary(df_prepared_foods)
str(df_prepared_foods)

#converting to time series ----

ts_frozen_foods <- ts(df_frozen_foods[,6], start=c(2013,1,1),
                      frequency = 365)
df_frozen_foods
ts_liquor_wine_beer <- ts(df_liquor_wine_beer[,6],
                          start=c(2013,1,1),
                          frequency = 365)

ts_meats <- ts(df_meats[,6], start=c(2013,1,1),
               frequency = 365)

ts_poultry <- ts(df_poultry[,6], start=c(2013,1,1),
                 frequency = 365)

ts_prepared_foods <- ts(df_prepared_foods[,6], start=c(2013,1,1),
                        frequency = 365)

#setting dimensions to null
dim(ts_frozen_foods)<- NULL
dim(ts_liquor_wine_beer)<- NULL
dim(ts_poultry)<- NULL
dim(ts_meats)<- NULL
dim(ts_prepared_foods)<- NULL


#checking out time series

summary(ts_frozen_foods)
str(ts_frozen_foods)
start(ts_frozen_foods)
end(ts_frozen_foods)
frequency(ts_frozen_foods)
class(ts_frozen_foods)
time(ts_frozen_foods)
cycle(ts_frozen_foods)

summary(ts_liquor_wine_beer)
str(ts_liquor_wine_beer)
start(ts_liquor_wine_beer)
end(ts_liquor_wine_beer)
frequency(ts_liquor_wine_beer)
class(ts_liquor_wine_beer)
time(ts_liquor_wine_beer)
cycle(ts_liquor_wine_beer)

summary(ts_meats)
str(ts_meats)
start(ts_meats)
end(ts_meats)
frequency(ts_meats)
class(ts_meats)
time(ts_meats)
cycle(ts_meats)

summary(ts_poultry)
str(ts_poultry)
start(ts_poultry)
end(ts_poultry)
frequency(ts_poultry)
class(ts_poultry)
time(ts_poultry)
cycle(ts_poultry)

summary(ts_prepared_foods)
str(ts_prepared_foods)
start(ts_prepared_foods)
end(ts_prepared_foods)
frequency(ts_prepared_foods)
class(ts_prepared_foods)
time(ts_prepared_foods)
cycle(ts_prepared_foods)

#plotting time series
plot.ts(ts_frozen_foods)
plot.ts(ts_liquor_wine_beer)
plot.ts(ts_meats)
plot.ts(ts_poultry)
plot.ts(ts_prepared_foods)

#Decomposition ----

#method 1 

#additive

decomp_frozen_foods_additive<-decompose(ts_frozen_foods)
plot(decomp_frozen_foods_additive)

decomp_liquor_wine_beer_additive<- decompose(ts_liquor_wine_beer)
plot(decomp_liquor_wine_beer_additive)

decomp_meats_additive<- decompose(ts_meats)
plot(decomp_meats_additive)

decomp_poultry_additive<- decompose(ts_poultry)
plot(decomp_poultry_additive)

decomp_prepared_foods_additive<- decompose(ts_prepared_foods)
plot(decomp_prepared_foods_additive)

#multiplicative
decomp_frozen_foods_multiplicative<-decompose(ts_frozen_foods, 
                                              type='multiplicative')
plot(decomp_frozen_foods_multiplicative)

decomp_liquor_wine_beer_multiplicative<- decompose(ts_liquor_wine_beer, 
                                                   type='multiplicative')
plot(decomp_liquor_wine_beer_multiplicative)

decomp_meats_multiplicative<- decompose(ts_meats, type='multiplicative')
plot(decomp_meats_multiplicative)

decomp_poultry_multiplicative<- decompose(ts_poultry, 
                                          type='multiplicative')
plot(decomp_poultry_multiplicative)

decomp_prepared_foods_multiplicative<- decompose(ts_prepared_foods, 
                                                 type='multiplicative')
plot(decomp_prepared_foods_multiplicative)

#method 2

#additive
stl_decomp_frozen_foods_additive<-stl(ts_frozen_foods,s.window=15, t.window = 365)
plot(stl_decomp_frozen_foods_additive)

str(ts_frozen_foods)

stl_decomp_liquor_wine_beer_additive<- stl(ts_liquor_wine_beer, 
                                           s.window=15, t.window=365)
plot(stl_decomp_liquor_wine_beer_additive)

stl_decomp_meats_additive<- stl(ts_meats, s.window=15, t.window=365)
plot(stl_decomp_meats_additive)

stl_decomp_poultry_additive<- stl(ts_poultry, s.window=15, t.window=365)
plot(stl_decomp_poultry_additive)

stl_decomp_prepared_foods_additive<- stl(ts_prepared_foods, s.window=15, 
                                         t.window=365)
plot(stl_decomp_prepared_foods_additive)


#multiplicative

#creating a log transformed time series
log_ts_frozen_foods <- log(ts_frozen_foods)
log_ts_liquor_wine_beer <- log(ts_liquor_wine_beer+1)
log_ts_meats <- log(ts_meats)
log_ts_poultry <- log(ts_poultry)
log_ts_prepared_foods <- log(ts_prepared_foods)

stl_decomp_frozen_foods_log<-stl(log_ts_frozen_foods,s.window=15, t.window = 365)
plot(stl_decomp_frozen_foods_log)


stl_decomp_liquor_wine_beer_log<- stl(log_ts_liquor_wine_beer, 
                                      s.window=15, t.window=365)
plot(stl_decomp_liquor_wine_beer_log)

stl_decomp_meats_log<- stl(log_ts_meats, s.window=15, t.window=365)
plot(stl_decomp_meats_log)

stl_decomp_poultry_log<- stl(log_ts_poultry, s.window=15, t.window=365)
plot(stl_decomp_poultry_log)

stl_decomp_prepared_foods_log<- stl(log_ts_prepared_foods, s.window=15, 
                                    t.window=365)
plot(stl_decomp_prepared_foods_log)

#Change point analysis ---- 

#for mean
cpt_mean_frozen_foods <- cpt.mean(
  ts_frozen_foods,
  test.stat = "Normal"
)
plot(cpt_mean_frozen_foods)

cpts.ts(cpt_mean_frozen_foods) #the year in which the mean changed

cpt_mean_liquor_wine_beer <- cpt.mean(
  ts_liquor_wine_beer,
  test.stat = "Normal"
)
plot(cpt_mean_liquor_wine_beer)

cpts.ts(cpt_mean_liquor_wine_beer) #the year in which the mean changed

cpt_mean_meats <- cpt.mean(
  ts_meats,
  test.stat = "Normal"
)
plot(cpt_mean_meats)

cpts.ts(cpt_mean_meats) #the year in which the mean changed

cpt_mean_poultry <- cpt.mean(
  ts_poultry,
  test.stat = "Normal"
)
plot(cpt_mean_poultry)

cpts.ts(cpt_mean_poultry) #the year in which the mean changed

cpt_mean_prepared_foods <- cpt.mean(
  ts_prepared_foods,
  test.stat = "Normal"
)
plot(cpt_mean_prepared_foods)

cpts.ts(cpt_mean_prepared_foods) #the year in which the mean changed

#for variance

cpt_var_frozen_foods <- cpt.var(
  ts_frozen_foods,
  test.stat = "Normal"
)
plot(cpt_var_frozen_foods)

cpts.ts(cpt_var_frozen_foods) #the year in which the variance changed

cpt_var_liquor_wine_beer <- cpt.var(
  ts_liquor_wine_beer,
  test.stat = "Normal"
)
plot(cpt_var_liquor_wine_beer)

cpts.ts(cpt_var_liquor_wine_beer) #the year in which the variance changed

cpt_var_meats <- cpt.var(
  ts_meats,
  test.stat = "Normal"
)
plot(cpt_var_meats)

cpts.ts(cpt_var_meats) #the year in which the variance changed

cpt_var_poultry <- cpt.var(
  ts_poultry,
  test.stat = "Normal"
)
plot(cpt_var_poultry)

cpts.ts(cpt_var_poultry) #the year in which the variance changed

cpt_var_prepared_foods <- cpt.var(
  ts_prepared_foods,
  test.stat = "Normal"
)
plot(cpt_var_prepared_foods)

cpts.ts(cpt_var_prepared_foods) #the year in which the variance changed

#for mean and variance
cpt_meanvar_frozen_foods <- cpt.meanvar(
  ts_frozen_foods,
  test.stat = "Normal"
)
plot(cpt_meanvar_frozen_foods)

cpts.ts(cpt_meanvar_frozen_foods) #the year in which the mean and variance changed

cpt_meanvar_liquor_wine_beer <- cpt.meanvar(
  ts_liquor_wine_beer,
  test.stat = "Normal"
)
plot(cpt_meanvar_liquor_wine_beer)

cpts.ts(cpt_meanvar_liquor_wine_beer) #the year in which the mean and variance changed

?cpts.ts
cpt_meanvar_meats <- cpt.meanvar(
  ts_meats,
  test.stat = "Normal"
)
plot(cpt_meanvar_meats)

cpts.ts(cpt_meanvar_meats) #the year in which the mean and variance changed

cpt_meanvar_poultry <- cpt.meanvar(
  ts_poultry,
  test.stat = "Normal"
)
plot(cpt_meanvar_poultry)

cpts.ts(cpt_meanvar_poultry) #the year in which the mean and variance changed

cpt_meanvar_prepared_foods <- cpt.meanvar(
  ts_prepared_foods,
  test.stat = "Normal"
)
plot(cpt_meanvar_prepared_foods)

cpts.ts(cpt_meanvar_prepared_foods) #the year in which the mean and variance changed

#Smoothing----

ts_frozen_foods_sma <- SMA(ts_frozen_foods, n=15)

plot(ts_frozen_foods, type = "l", col = "blue",
     main = "Sales of Frozen Foods with Simple Moving Average")

lines(ts_frozen_foods_sma, col = "red")
legend("topleft", legend = c("Sales of Frozen Foods", "Simple Moving Average"), 
       col = c("blue", "red"), lty = 1, cex = 0.6)

#aggregate ----

plot(aggregate(ts_frozen_foods,FUN=mean),
     xlab = "Year",
     ylab = "Sales of Frozen Foods") 

plot(aggregate(ts_liquor_wine_beer,FUN=mean),
     xlab = "Year",
     ylab = "Sales of Liquor, Wine, Beer") 

plot(aggregate(ts_meats,FUN=mean),
     xlab = "Year",
     ylab = "Sales of Meat") 

plot(aggregate(ts_poultry,FUN=mean),
     xlab = "Year",
     ylab = "Sales of Poultry") 

plot(aggregate(ts_prepared_foods,FUN=mean),
     xlab = "Year",
     ylab = "Sales of Prepared Foods") 

#boxplot ----
library(feasts)

boxplot(ts_frozen_foods~cycle(ts_frozen_foods),
        xlab = "Month",
        ylab = "Passanger")

#adf test for stationarity ----
library("tseries")

adf_result_frozen_foods<- adf.test(ts_frozen_foods)
adf_result_frozen_foods

adf_result_liquor_wine_beer<- adf.test(ts_liquor_wine_beer)
adf_result_liquor_wine_beer

adf_result_meats<- adf.test(ts_meats)
adf_result_meats

adf_result_poultry<- adf.test(ts_poultry)
adf_result_poultry

adf_result_prepared_foods<- adf.test(ts_prepared_foods)
adf_result_prepared_foods


# Define the significance levels
alpha <- c(0.01, 0.05, 0.10)  # 1%, 5%, and 10%
# Calculate critical values based on the normal distribution
critical_values_frozen_foods <- qnorm(1 - alpha/(2*length(ts_frozen_foods)))
#If the test statistic < critical value -> stationary
critical_values_frozen_foods
adf_result_frozen_foods$statistic

# Define the significance levels
alpha <- c(0.01, 0.05, 0.10)  # 1%, 5%, and 10%
# Calculate critical values based on the normal distribution
critical_values_liquor_wine_beer <- qnorm(1 - alpha/(2*length(ts_liquor_wine_beer)))
#If the test statistic < critical value -> stationary
critical_values_liquor_wine_beer
adf_result_liquor_wine_beer$statistic

# Define the significance levels
alpha <- c(0.01, 0.05, 0.10)  # 1%, 5%, and 10%
# Calculate critical values based on the normal distribution
critical_values_meats <- qnorm(1 - alpha/(2*length(ts_meats)))
#If the test statistic < critical value -> stationary
critical_values_meats
adf_result_meats$statistic

# Define the significance levels
alpha <- c(0.01, 0.05, 0.10)  # 1%, 5%, and 10%
# Calculate critical values based on the normal distribution
critical_values_poultry <- qnorm(1 - alpha/(2*length(ts_poultry)))
#If the test statistic < critical value -> stationary
critical_values_poultry
adf_result_poultry$statistic

# Define the significance levels
alpha <- c(0.01, 0.05, 0.10)  # 1%, 5%, and 10%
# Calculate critical values based on the normal distribution
critical_values_prepared_foods<- qnorm(1 - alpha/(2*length(ts_prepared_foods)))
#If the test statistic < critical value -> stationary
critical_values_prepared_foods
adf_result_prepared_foods$statistic


#ACF----
acf(ts_frozen_foods, main = "ACF for Original Time Series",
    xlab = "Lag",
    ylab = "Correlation")

acf(ts_liquor_wine_beer, main = "ACF for Original Time Series",
    xlab = "Lag",
    ylab = "Correlation")

acf(ts_meats, main = "ACF for Original Time Series",
    xlab = "Lag",
    ylab = "Correlation")

acf(ts_poultry, main = "ACF for Original Time Series",
    xlab = "Lag",
    ylab = "Correlation")

acf(ts_prepared_foods, main = "ACF for Original Time Series",
    xlab = "Lag",
    ylab = "Correlation")

#PACF ----
pacf(ts_frozen_foods, main = "PACF for Original Time Series")
pacf(ts_liquor_wine_beer)
pacf(ts_meats)
pacf(ts_poultry)
pacf(ts_prepared_foods)


#splitting for training and testing ----
trn_frozen_foods <- ts_frozen_foods %>% window(end = c(2014,172))
length(trn_frozen_foods)

trn_frozen_foods <- ts_frozen_foods %>% window(end = c(2014,172))
length(trn_frozen_foods)

trn_frozen_foods <- ts_frozen_foods %>% window(end = c(2014,172))
length(trn_frozen_foods)

trn_frozen_foods <- ts_frozen_foods %>% window(end = c(2014,172))
length(trn_frozen_foods)

trn_frozen_foods <- ts_frozen_foods %>% window(end = c(2014,172))
length(trn_frozen_foods)

tst_frozen_foods <- ts_frozen_foods %>% window(start = c(2014,173))

tst_liquor_wine_beer<- ts_liquor_wine_beer %>% window(start = c(2014,173))

tst_meats <- ts_meats %>% window(start = c(2014,173))

tst_poultry <- ts_poultry %>% window(start = c(2014,173))

tst_prepared_foods <- ts_prepared_foods %>% window(start = c(2014,173))


#linear model / regression ----
