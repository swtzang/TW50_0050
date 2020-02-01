# Purpose: import quarterly constituent stocks (ticker names) of TW50_0050
# setwd("D:/Data TWN50")
rm(list=ls())
library(dplyr)
#clibrary(tidyverse)
# data source: http://www.twse.com.tw/zh/ETF/fund/0050
# import the latest ticker names (short names) and code 
ticker<-read.csv('stock_2018_short.csv', sep=",", strip.white = TRUE,  header=TRUE)
ticker %>% mutate_if(is.factor, as.character) -> ticker
str(ticker)
lookup<-ticker[,1:2]
names(lookup)<-c("id","name")
str(lookup)
dim(lookup)
head(lookup)
# append some ticker names 
lookup[dim(lookup)[1] +1, 1] = 2633
lookup[dim(lookup)[1]   , 2] = "台灣高鐵"
lookup[dim(lookup)[1] +1, 1] = 6505
lookup[dim(lookup)[1]   , 2] = "台塑石化"
lookup[dim(lookup)[1] +1, 1] = 2892
lookup[dim(lookup)[1]   , 2] = "第一金控"
lookup[dim(lookup)[1] +1, 1] = 2801
lookup[dim(lookup)[1]   , 2] = "彰化銀行"
lookup[dim(lookup)[1] +1, 1] = 2890
lookup[dim(lookup)[1]   , 2] = "永豐金控"
lookup[dim(lookup)[1] +1, 1] = 2884
lookup[dim(lookup)[1]   , 2] = "玉山金控"
lookup[dim(lookup)[1] +1, 1] = 2887
lookup[dim(lookup)[1]   , 2] = "台新金控"
lookup[dim(lookup)[1] +1, 1] = 3474
lookup[dim(lookup)[1]   , 2] = "華亞科"
lookup[dim(lookup)[1] +1, 1] = 3697
lookup[dim(lookup)[1]   , 2] = "F-晨星"
lookup[dim(lookup)[1] +1, 1] = 3009
lookup[dim(lookup)[1]   , 2] = "奇美電子"
lookup[dim(lookup)[1] +1, 1] = 3012
lookup[dim(lookup)[1]   , 2] = "廣輝下市"
lookup[dim(lookup)[1] +1, 1] = 8078
lookup[dim(lookup)[1]   , 2] = "華寶通訊"
lookup[dim(lookup)[1] +1, 1] = 6004
lookup[dim(lookup)[1]   , 2] = "元京證"
#
head(lookup)
dim(lookup)
# 2009/11/15: 鴻海集團（2317）旗下的群創（3481）昨宣布購併奇美電（3009），換股比率為1股群創換2.05股奇美電，
#以上周五奇美電收盤價18.8元、群創47元計算，奇美電以溢價21.9％嫁入鴻海集團，
#---------------------------------------------------------------------------------------------------
# F-晨星（3697）因與聯發科（2454）合併，申請其有價證券終止上市已生效，F-晨星將在2014年1月16日停止買賣，
# 同年2月1日起終止上市。
#---------------------------------------------------------------------------------------------------
# 2006/04/08: 友達（2409）與廣輝（3012）宣布合併！就在昨日下午，友達與廣輝臨時召開記者會宣布合併，
# 換股比例為友達1股換廣輝3.5股，友達為存續公司，董事長由李焜耀出任，廣輝副董事長梁次震將出任副董事長，
# 合併基準日在今年10月1日。
#-----------------------------------------------------------------------------------------------
# 2013/12/19:華寶通訊(8078)因併入仁寶電腦，自明年2月17日停止買賣，2月27日起下市。
#-------------------------------------------------------------------------------------------------
# 2007/3/9: 國內券商龍頭元京證（6004），即將在3月20日下市停止交易！
# 金管會昨天正式核准復華金（2885）、元京證換股合併，元京證因此將先行下市，據元大集團內部估算，3月20日將是最後交易日，之後，元京證股東們手中的股票，將以每股換復華金1.615股比例，自動轉換為復華金股票，4月2日即可上市買賣。
#--------------------------------------------------------------------------------------------------------

# Now we have lookup table with id and names
#========================================================================================================
# import ticker names (full and long names)
ticker.f<-read.csv('ticker_2018_full.csv', sep=",", strip.white = TRUE,  header=TRUE)
ticker.f %>% mutate_if(is.factor, as.character) -> ticker.f
lookup.f<-ticker.f[,1:2]
names(lookup.f)<-c("id","name")
str(lookup.f)
head(lookup.f)
#=====================================================
#import 2012Q1 stock names
Q1.2012<-read.csv('2012Q1.csv', sep=",", strip.white = TRUE)
Q1.2012 %>% mutate_if(is.factor, as.character) -> Q1.2011
str(Q1.2012)
head(Q1.2012)
# match names of Q1.2012 with lookup table names
# use partial string matching function pmatch()
# pmatch(Q1.2012$name, lookup$name)
Q1.2012$id<- with(lookup, id[pmatch(Q1.2012$name, name)])
Q1.2012$id
# select na values, i.e., not matching names
name.f<-Q1.2012$name[is.na(Q1.2012$id)]
name.f
# rematch names with full names again
#i=5
for (i in 1:length(Q1.2012$id)){
  if (is.na(Q1.2012$id[i]))
     Q1.2012$id[i] = lookup.f$id[pmatch(Q1.2012$name[i], lookup.f$name)]
}
#確認股票代碼沒有出現NA 
sum(is.na(Q1.2012$id)) 
#testing codes end here
#========================================================================

# import quarterly data: from 2004Q1.csv to 2018Q4.csv 
year<-seq(2004, 2018)
year1<-rep(year, each=4)
# 共有14年(2017-2004+1)
quarter<-rep(c('Q1', 'Q2', 'Q3', 'Q4'), 15)
qt = rep(0, length(quarter))
for (i in 1:length(quarter)){
   qt[i] = paste(year1[i], quarter[i], sep='')
}

qt
# create an empty df
tempi<-data.frame(matrix(rep(0, 100), ncol=2))*NA
dim(tempi)
# when j=57, it means 2018Q1.csv which already has id column so we don't need to look up stock 
# ids by their names  
i=1
for (j in 1:length(qt)){
  file.name <- paste(qt[j], '.csv', sep='')
  temp<-read.csv(file.name, sep=",", strip.white = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  # temp<-read_csv(file.name, locale=locale(encoding="UTF-8"))
  temp %>% mutate_if(is.factor, as.character) -> temp
  if (j <=57) {
              temp$id<- with(lookup, id[pmatch(temp$name, name)])
  }  else {
           temp$id <- temp$id
          }
  #name.f<-Q1.2012$name[is.na(Q1.2012$id)]   
  for (i in 1:length(temp$id)){
    if (is.na(temp$id[i]))
      temp$id[i] = lookup.f$id[pmatch(temp$name[i], lookup.f$name)]
  }
  tempi[,2*j-1] = temp$id
  tempi[,2*j] = temp$name
  colnames(tempi)[2*j-1] = paste(paste("X", qt[j], sep=""), ".id", sep="")
  colnames(tempi)[2*j] = paste(paste("X", qt[j], sep=""), ".name", sep="")
}
head(tempi)
dim(tempi)
sum(is.na(tempi))
write.csv(tempi, file="tw50_hist_code.csv", fileEncoding = "UTF-8")
