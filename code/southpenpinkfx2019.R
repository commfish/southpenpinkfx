# notes ----
# South Peninsula Pink forcast for Kodiak AK
# sarah.power@alaska.gov
# 11/19/2019

# load ----
if(!require("tidyverse"))   install.packages("tidyverse") # data manipulation & graphing
if(!require("RDS"))   install.packages("RDS") # forecasting
if(!require("fpp2"))   install.packages("fpp2") # forecasting
options(scipen=999) # prevent scientific notation everywhere

# data ----
spenpinks2019 <- read.csv('data/spenpinks2019.csv') %>% 
  select(year, run = post_june_harvest_escapement)
#run = The number of pinks that return to south peninsula estimanted either in the harvest or in escapement.

#Check for NAs and fix in this small dataset
spenpinks2019 %>% filter(run == "NA") 

# Forecasts are done on even and odd years.
even <- spenpinks2019  %>% 
  filter(year %% 2 == 0) %>% 
  select(run) %>% 
  as.ts()

odd <- spenpinks2019 %>% 
  filter(year %% 2 == 0) %>% 
  as.ts()

# analysis ----

#choose one type of analysis

#naive (last years value)
fc <- fcn <- naive(even, h = 1)
#simple exponential smoothing
fc <- fcses <- ses(even, h = 1)
#holt
fc <- fch <- holt(y = even, h = 1,  exponential = FALSE) 
#holt for the 2020 forecast don't use this method since residuals don't pass the Ljung-Box test.
fc <- fch <- holt(y = even, h = 1,  exponential = TRUE) 
#damped holt
#fc <- holt(y = even, h = 1, level = c(80, 80),  damped = TRUE, lambda = "auto") #can specify different Prediction intervals if needed. 
fc <- holt(y = even, h = 1, damped = TRUE, lambda = "auto") 

#Check
#autoplot(even)
#ggAcf(even)
#ggAcf(diff(even))

summary(fc)
checkresiduals(fc)
accuracy(fc)
autoplot(fc) + autolayer(fitted(fc))

#Checking we have the right number of things also used inspection to check that the most recent values are accurate. 
length(fitted(fc))
length(even)
