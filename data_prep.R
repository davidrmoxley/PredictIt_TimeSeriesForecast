
prep_ts_data <- function(date_range){
  
  ############################################################
  ##                                                        ##      
  ##  Takes date_range list of start and end date for ts    ##
  ##                                                        ##
  ##      (1) Get data from GitHub repo                     ##
  ##      (2) Conform dates and interpolate missing values  ##
  ##      (3) Create single dataframe of time serires       ##
  ##                                                        ##
  ############################################################
  
  # load required packages
  #if(!require(readr)){install.packages("readr")}; library(readr)
  if(!require(dplyr)){install.packages("dplyr")}; library(dplyr)
  if(!require(zoo)){install.packages("zoo")}; library(zoo)
  
  # get data
  df_predictIt <- read.csv("https://raw.githubusercontent.com/davidrmoxley/predictit_time_series_forecast/master/predictItprez.csv")
  df_job <- read.csv("https://raw.githubusercontent.com/davidrmoxley/predictit_time_series_forecast/master/jobApproval.csv")
  df_google <- read.csv("https://raw.githubusercontent.com/davidrmoxley/predictit_time_series_forecast/master/googleTrend_Pres.csv")
  df_sp <- read.csv("https://raw.githubusercontent.com/davidrmoxley/predictit_time_series_forecast/master/sp500.csv")


  # data prep
  ## predictive market data
  df_predictIt$Date <- as.Date(df_predictIt$Date,"%m/%d/%y")
  df_Trump <- df_predictIt[df_predictIt$ContractName =="Trump",]
  df_Biden <- df_predictIt[df_predictIt$ContractName =="Biden",]
  df_price <- data.frame(Date = df_Trump$Date, priceSpread = df_Trump$OpenSharePrice - df_Biden$OpenSharePrice)
  df_tradeVol <- data.frame(Date = df_Trump$Date, volume = df_Trump$TradeVolume - df_Biden$TradeVolume)
  rm(df_Trump, df_Biden)
  
  
  ## job approval data
  ### format dates
  df_job$index <- seq.int(nrow(df_job))
  df_job$Date <- as.character(df_job$Date)
  df_job$Date_Start <- sapply(strsplit(df_job$Date, " -"), "[", 1)
  df_job$Date_Start <- paste0(df_job$Date_Start,"/",as.character(df_job$Year))
  df_job$Date_Start <- as.Date(df_job$Date_Start,"%m/%d/%Y")
  df_job$Date_Start_Wk <- paste0(as.character(df_job$Year),strftime(df_job$Date_Start, format = "%V"))
  df_job$Date_End <- sapply(strsplit(df_job$Date, " - "), "[", 2)
  df_job$Date_End <- paste0(df_job$Date_End,"/",as.character(df_job$Year))
  df_job$Date_End <- as.Date(df_job$Date_End,"%m/%d/%Y")
  df_job$Date_End_Wk <- paste0(as.character(df_job$Year),strftime(df_job$Date_End, format = "%V"))
  
  ### create daily active poll average
  df_job_new <- data.frame(Date=as.Date(character())
                              ,Approve=integer()
                              ,Disapprove=integer()
  )
  
  for(i in 1:nrow(df_job)){
    temp <- as.data.frame(seq(df_job$Date_End[i], df_job$Date_End[i] + 4, by="day"))
    colnames(temp) <- "Date"
    temp$index <- i
    temp <- merge(temp,df_job[colnames(df_job)%in%c("index","Approve","Disapprove")],by="index")
    df_job_new <- rbind(df_job_new,temp[,2:4])
  }
  
  df_job_new$jobSpread <- df_job_new$Approve - df_job_new$Disapprove
  df_job <- aggregate(jobSpread ~ Date, data=df_job_new, FUN=mean)
  rm(df_job_new,i)
  
  ### google data
  colnames(df_google) <- c("Date","gTrend")
  df_google$Date <- as.Date(df_google$Date,"%m/%d/%Y")  
  
  ### S&P 500
  df_sp$Date <- as.Date(df_sp$Date,"%m/%d/%Y")
  df_sp$Month <- paste0(strftime(df_sp$Date, format = "%m"),"/1/",strftime(df_sp$Date, format = "%Y"))
  df_sp$Month <- as.Date(df_sp$Month,"%m/%d/%Y")
  
  temp <- aggregate(Close~Month,data=df_sp,FUN=mean)
  colnames(temp) <- c("Month","AvgClose")
  temp <- temp[order(temp$Month),]
  temp$PrevMonth <- lead(temp$Month, 1, order_by=temp$Month)
  df_sp <- merge(df_sp,temp,by.x="Month",by.y="PrevMonth")
  df_sp$devPrevMonth <- with(df_sp, (Close-AvgClose)/AvgClose)
  df_sp <- df_sp[colnames(df_sp)%in%c("Date","devPrevMonth")]
  rm(temp)
  
  ## Combine datasets
  df <- merge(df_price,df_tradeVol,by="Date",all.x=TRUE)
  df <- merge(df,df_google,by="Date",all.x=TRUE)
  df <- merge(df,df_job,by="Date",all.x=TRUE)
  df <- merge(df,df_sp,by="Date",all.x=TRUE)
  
  ## Interpolate NAs with previous values
  df$devPrevMonth <- na.locf(df$devPrevMonth, fromLast = FALSE)
  df$gTrend <- na.locf(df$gTrend, fromLast = FALSE)
  df$t <- row(df)
  
  
  rm(df_google,df_job,df_predictIt,df_price,df_sp,df_tradeVol)
  
  return(df)
}

df <- prep_ts_data()
