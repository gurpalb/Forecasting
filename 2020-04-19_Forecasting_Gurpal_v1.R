##################################################################
# Title: Advanced Analytics Pre-booking Project
# Author: Gurpal Bisra
# Date: last revised on March 19, 2020
# File Location: G:\QUIST\Production\Gurpal Bisra\Advanced Analytics
# Function: Expolore dataset + come up with questios to ask about it

##################################################################
# USER STORY
##################################################################
# As a: Director, Decision Support
# I want: look at the processed data in a graphical or statistical form 
        # and trying to find patterns, connections, and relations in the data.
# So that: we can decide if forecasting sick time is feasible.

##################################################################
# BACKGROUND 
##################################################################

# This dataset is reporting all shifts worked or not but needed (Relief not found) for all units using Kronos scheduling system, 
# it is restricted to a subset of front line job families only: RN-DC1, RN-CH1 (community nurses), LPN, Care Aide Acute and residents. 
# It has data from April 2016 to the last reporting period available.

# Column description:
  # Emplid: unique employee identifier
  # Home department/jobcode/family: that’s the department and job that the employee belongs to (fields will probably not be used in 
                                  # the forecast)
  # Charged department/jobcode/family: that’s the department and job where the hours are worked. The distinction between home vs 
                                  # charged is useful to determine if the shift is considered as regular (=planned) or non-regular 
                                  # (=that’s what we want to predict). See below for more details
  # Date_only: date of the shift
  # Weekday, shift start/end time: more details about WHEN it happened
  # Exception notice date / time (hours): time from the moment the exception is created, only useful when Category = relief and reason 
                                  # is Sick, Workload

# Regular / Non Regular hours: are considered regular hours the hours worked by non-casual employees assigned to the charged department 
                                  # and paid at straight time. Assigned to the charged department means that the employee home and charged 
                                  # department is the same, the employee owns a line in the rotation of this unit (could be a Baseline, 
                                  # Other Relief or a Vacation Relief line). Non regular hours are all other hours and include hours worked 
                                  # by casual, float, agency hours and all hours paid overtime, are also included Relief Not Found (RNF) 
                                  # hours (see below).

# For all hours charged to a department (or needed but not found), we determined the WHO, HOW and REASON.

# WHO worked the hours: 
#   •	Regular is a non-casual employee who is assigned to the charged unit on a permanent or temporary basis: for this employee the home 
                                  # and charged department is the same.
# •	Casual is defined by the job type of the employee working the hours. 
# •	Float is a non-casual employee who has a different home department than the charged one.
# •	Agency Nurse are the hours coded specifically to non VCH employees who are coming from agencies. (you can ignore/exclude this one, 
                                  # data is not collected accurately for now)
# •	Not Worked (RNF) are the hours needed but not found, no one worked these hours.
# •	Not Worked (RNF) filled by alternate are the hours needed but could not find the job code needed, these hours coded Relief Not Found 
                                  # were worked by an alternate, usually a lower job code.

# HOW was charged the hours to the department:
  # •	Straight time are the hours paid at regular rate.
  # •	Straight over FTE are the hours paid at regular rate but worked by part time over their FTE.
  # •	Overtime are the hours paid at overtime rate.
  # •	RNF are the hours not worked (include both RNF and RNF filled by alternate).
# REASON links to the ESP code used for the charged hours: 
  # •	For the exceptions we aggregated all ESP codes into self-explanatory buckets. 
  # •	The category ‘Other’ aggregates less used reasons, the Reason Detail will provide more information for those hours. 
            # The matching list of ESP codes and categories is available on demand (see Contact section at the end of this document).

# Objective
# We want to predict at the department - job code level (charged department, charged jobcode), per day – shift portion 
# (date_only, shift description) for the upcoming 12 weeks the number of non-regular shifts needed (and/or total hours)
# Non regular shifts are all shifts having the WHO <> ‘Regular’ AND How <> ‘Straight time’
# The output of the model can be both the number of shift and total hours needed
# In the live data set some future non-regular shifts will already be booked (known relief needed), some will not 
# (model will need to predict these ones). 
# The most important reliefs to forecast are when Reason = Sick or workload, and/or How = Overtime, RNF
# Target departments: focus on department that have a lot of RNF-OT and Sick (emerg, ICU etc, see below)

##################################################################
# SET THE WORKING DIRECTORY + LIBRARY DIRECTOR
##################################################################

# Clear current variable history + console
cat("\014")   # control + L can clear console
rm(list=ls()) # clear environment saved variables

# set library path
libPath <-"G:/QUIST/Production/Gurpal Bisra/Advanced Analytics/Library"
.libPaths(libPath) #set the library path
# .libPaths()

# set WD
wd <- "G:/QUIST/Production/Gurpal Bisra/Advanced Analytics"
setwd(wd)

##################################################################
# INSTALL PACKAGES
##################################################################

# install.packages('packagename')
# install.packages('ggplot2') # used
# install.packages('dplyr') # used
# install.packages('lattice') # used
# install.packages('gridExtra') # used
# install.packages('RODBC') # used
# install.packages('data.table') # used
# install.packages("fpp") # load the Forecasting: Principles and Practice package, as well as all its dependencies
# install.packages('xts') # used
# install.packages('txbox') # used to convert data frame into timeseries BUT need R 3.6.3
# install.packages('fma')
# install.packages('tseries')
# install.packages('forecast')
# install.packages("MPV")

# load required pacakages 
library(ggplot2) #for plotting
library(dplyr)
library(lattice)
library(gridExtra)
library(RODBC)
library(data.table)
library(fpp) 
library(xts)
library(fma)
library(tseries)
library(forecast)
library(MPV) # Data Sets from Montgomery, Peck and Vining's Book used for regression in forecasting

##################################################################
# LOAD DATA + SET DATA TYPES
##################################################################

    # conn <- odbcConnect("SDDBSDSUP003") # Open connection to SQL server
    # mydata <- as.data.frame(sqlQuery(conn, 
    #                                 paste("SELECT *
    #                                        FROM [HR].[dbo].[hr]")))
    # odbcCloseAll() # close connection to SQL Server
   
# Add New Fields for dates & times (with and without HH:MM:SS)
  # mydata$shift_start_date <- as.Date(mydata$shift_start_date_time)
  # mydata$shift_end_date <- as.Date(mydata$shift_end_date_time)
  # mydata$exception_notice_date <- as.Date(mydata$exception_notice_datetime)
  # mydata$exception_notice_time <- as.POSIXct(mydata$exception_notice_datetime, format="%H:%M:%S")
  # mydata$exception_notice_timeHMS <- as.ITime(mydata$exception_notice_time) # package data.table
  # mydata$shift_start_timeHMS <- as.POSIXct(mydata$shift_start_date_time, format="%H:%M:%S")
  # mydata$shift_start_timeHMS <- as.ITime(mydata$shift_start_timeHMS) # package data.table
  # mydata$shift_end_timeHMS <- as.POSIXct(mydata$shift_end_date_time, format="%H:%M:%S")
  # mydata$shift_end_timeHMS <- as.ITime(mydata$shift_end_timeHMS) # package data.table # BLANK BECAUSE NO TIME WAS GIVEN

# Check Data Types now
# str(mydata)

##################################################################
# CREATE EMERGENCY DATASET
##################################################################

## FIRST, WORK ON RELIEF NOT FOUND (RNF)
## charged_dept_description = 73102000-700 : Emergency

Date = seq(from = as.Date("2016-01-03"), to = as.Date("2020-03-31"), by = 'day') # start on Sunday
date_table = data.frame(Date) # need in dataframe format to left join to data
# str(date_table)

# USE ODBC TO PULL DATA
conn <- odbcConnect("SDDBSDSUP003") # Open connection to SQL server
emrg_table <- as.data.frame(sqlQuery(conn, 
                                 paste("SELECT date_only
                                       , COUNT(*) AS [ShiftsNeeded]
                                       FROM [HR].[dbo].[hr]
                                       WHERE who <> 'Regular'
                                       AND how <> 'Straight time'
                                       AND reason IN ('Sick', 'Workload')
                                       AND date_only >= '2016-01-01'
                                       AND charged_dept_description LIKE ('%Emergency')
                                       GROUP BY date_only
                                       ORDER BY date_only")))
odbcCloseAll() # close connection to SQL Server

# LEFT JOIN TO GET EMERGENCY DATASET
df1 = dplyr::left_join(date_table, emrg_table, by = c("Date" = "date_only"))
str(df1)

# REPLACE NA VALUES IWTH 0
df1$ShiftsNeeded[is.na(df1$ShiftsNeeded)] <- 0

##################################################################
# MAKE EMERGENCY TIME SERIES DATASET + EXPLORE
# Note: make weekly object with period 7, start on Sunday (i.e. January 3, 2016)
##################################################################

# (1) PLOT DATA AND OBSERVE PATTERNS ######################################################
    plot(df1)
    x <- ts(df1$ShiftsNeeded, frequency=7) ## i.e. start on Sunday, January 3rd, 2016, then weekly after
    class(x)
    plot(x, xlab = "Week", ylab = "Total Reliefs Not Found", main = "Emergency: All Sites", col = "grey11")
    ## observations:
        # seasonal patterns,
        # level patterns as the maximum and minimum number of RNF remain relatively constant for each year,
        # cycle patterns as the data exhibits rises and falls.
    
    ### try basic fits to ensure R can work
    # select training set
    xtrain <- window(x,start=1,end=215) # select the training set 
                                        # (period 1 through period 218 (i.e. week 221 now))
    xtest <- window(x, start=215,end=221) # select testing set
    
    
    xfit1 <- meanf(xtrain, h = 14) ## Time Horizon = 2 weeks into the future or 2 periods
    xfit2 <- naive(xtrain, h = 14) 
    xfit3 <- snaive(xtrain, h = 14)
    
    plot(xtrain, xlab = "Week", ylab = "Reliefs Not Found", main = "Emergency: All Sites", col = "black", xlim=c(205, 220))
    ##plot(xfit1, plot.conf=FALSE, PI = FALSE, main ="Forecasts for Emergency: All Sites", xlab = "Week", ylab = "Reliefs Not Found", col = 4, xlim=c(200, 225))
    
    lines(xfit1$mean, col='blue')
    lines(xtest, col="grey")
    lines(xfit2$mean,col=2)
    lines(xfit3$mean,col=3)
    legend("topright",lty=1,col=c("black","grey",4,2,3), legend=c("Training Set", "Testing Set", "Mean Method","Naive Method","Seasonal Naive Method"))
    
    accuracy(xfit1, xtest)[2, c(2,3,5,6)]
    accuracy(xfit2, xtest)[2, c(2,3,5,6)]
    accuracy(xfit3, xtest)[2, c(2,3,5,6)]
    
    # Compute the naive residuals and plot them. 
    
    checkresiduals(xfit2)
    
    # res <- residuals(xfit2) # residuals from the seasonal naive method fitted to the training set
    # 
    # plot(res, main="Residuals from Naive Method", ylab="", xlab="Years")
    # abline(0,0, lty=2)
    # # actually pretty good, fits around 0
    # 
    # # ACF plot of residuals
    # Acf(res, main="ACF of residuals")
    #     # pretty good (i.e. stays mostly within the dotted lines) except for the lag at 7
    #     # but still, < 20% of the lines are within the dotted lines 
    # 
    # # Histogram of residuals
    # hist(res, nclass="FD", main="Histogram of residuals") #nclass = FD uses a way of choosing the number of bins based on the IQR
    #     # actually pretty good, very normally distributed
    # 
## EXPLORE: SMOOTHING Will definitely require smoothing (i.e. averaging past values)
    
    par(mfrow=c(3,1), mar=c(4,4,2,1))
    plot(x, xlab = "Week", ylab = "Reliefs Not Found", main = "Emergency: All Sites (ma = 3)", col = "grey", xlim=c(200, 225))
    lines(ma(x,3),col="red")
    # works very well
  
    plot(x, xlab = "Week", ylab = "Reliefs Not Found", main = "Emergency: All Sites (ma = 7)", col = "grey", xlim=c(200, 225))
    lines(ma(x,7),col="blue")
    # does not work well, essentially have a straight line
    # have 7, so lose 3 on each end
    
    plot(x, xlab = "Week", ylab = "Reliefs Not Found", main = "Emergency: All Sites (ma = 3 x 3)", col = "grey", xlim=c(200, 225))
    lines(ma(ma(x,3),3),col="green")
    # double-moving average
    
    dev.off()
    
## EXPLORE: DECOMPOSITION to identify the seasonal + trend-cycle components
      # DATA = PATTERN + ERROR = f(TREND-CYCLE, SEASONALITY, ERROR)
      # Yi = actual data
      # St = seasonal component
      # Tt = trend-cycle component
    
    ##  PLOT DECOMPOSITION PLOT
    xstl <- stl(x, s.window = "periodic") #seasonal and trend decomposition using Loess
    plot(xstl)
    
# (2) BOX-COX TRANSFORMATION? ##############################################################
    monthplot(x, xlab = "Week", ylab = "Reliefs Not Found", main = "Emergency: All Sites", col = "blue")
    
      # The seasonal deviation plot suggests there are variations due to the days of the week. 
      # Therefore, decided to use the Box-Cox technique to reduce the increasing variation I observed. 
      # My plot is found below. I observed the time-series data is did not change significantly.
    
    lambda <- BoxCox.lambda(x) # = 0.4066952; 
    plot(BoxCox(x,lambda), xlab = "Week", ylab = "Reliefs Not Found", main = "Emergency: All Sites", col = "blue")
    # however, need symmetric technique because you cannot have -2 RNFs

# (3a) TRY AUTOMATED ALGORITHM = auto.arima() ###############################################
    fit <- auto.arima(xtrain) # uses unit roots tests to minimize AICc and MLE to get
    
      # looks good https://www.rdocumentation.org/packages/ggbio/versions/1.20.1/topics/autoplot
    autoplot(forecast(fit), conf.int = F, PI = FALSE, xlim=c(205, 220), ylim=c(-1,8), xlab = "Week", ylab = "Reliefs Not Found") + theme_light() + autolayer(xtest, series="Data", color = "grey") + autolayer(xfit2, series="Data", color = "red") 
    ##   legend("topright",lty=1,col=c("black","grey",4,2), legend=c("Training Set", "Testing Set", "ARIMA Method","Naive Method"))

    summary(fit)                     # ARIMA model
  
    # looks good https://www.rdocumentation.org/packages/ggbio/versions/1.20.1/topics/autoplot
    autoplot(forecast(fit), xlim=c(205, 220), ylim=c(-1,8), xlab = "Week", ylab = "Reliefs Not Found") + theme_light() + autolayer(xtest, series="Data", color = "grey") + autolayer(xfit2, series="Data", color = "red") 
    ##   le
    
    # ARIMA(1,1,1)(0,0,1)[7]
    checkresiduals(fit)
    
# (3b) SELECT MODEL MYSELF ###################################################################
  # fit <- Arima(x, order = c(p,d,q))
  # fit <- Arima(x, order = c(p,d,q), seasonal = c(P,D,Q))
      
    # first difference data until stationary
  # break training data into 80% of data & test data into 20% of data 
  # (can make as small as 2 weeks though (i.e. time horizon))
# PLOT ACF/PACF OF DIFFERENCED DATA TO DETERMINE MODELS
    
    # ACF plots are autocorrelation plots (correlograms) which show the autocorrelation coefficients for different lags. The purpose of the ACF plot is to check for seasonality, cycles, and other time series patterns. 
    Acf(x, lag.max = 52, main = "ACF Plot for Emergency: All Sites")
    
# TRY OUT MODEL + USE AIC TO FIND BEST MODEL

# (4) CHECK RESIDUALS OF CHOSEN MODEL AND PLOT ACF OF THEM, NORMALITY OF RESIDUALS ETC. #######

# (5) IF RESIDUALS LOOK LIKE WHITE NOISE, THEN CALCULATE FORECASTS ############################

##################################################################
# MAKE EMERGENCY TIMESERIES DATASET + EXPLORE
# Note: make weekly object with period 7, start on Sunday (i.e. January 3, 2016)
##################################################################

## Attempt 2: Plot as multi seasonal time series objects because could have weekly and annual seasonality

## source: https://robjhyndman.com/hyndsight/dailydata/
## When the time series is long enough to take in more than a year, 
# then it may be necessary to allow for annual seasonality as well as weekly seasonality. 
# In that case, a multiple seasonal model such as TBATS is required.

y <- msts(df1$ShiftsNeeded, seasonal.periods=c(7,365.25)) # multiple timeseries models
# source: https://www.rdocumentation.org/packages/forecast/versions/8.12/topics/tbats
  # TBATS Model (Exponential Smoothing State Space Model With Box-Cox Transformation, 
  # ARMA Errors, Trend And Seasonal Components)
fit <- tbats(y,  use.parallel = FALSE) ## note: tbats function is part of 'forecast' package
fc <- forecast(fit)
plot(fc)

# This should capture the weekly pattern as well as the longer annual pattern. 
# The period 365.25 is the average length of a year allowing for leap years. 
