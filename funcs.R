library(shiny)
library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)
library(R.utils)
library(plotly)
library(shinyBS)
library(shinyjs)
library(RMySQL)
library(V8)
#devtools::install_github('andrewsali/shinycssloaders')
library(shinycssloaders)


"%+%" <- function(x,y) {paste(x,y,sep="")}
"%g%" <- function(x,y) {x[grepl(y,x)]}
"%gp%" <- function(x,y) {x[grepl(y,x,perl=TRUE)]}

sql_get = function(query){
  con <- dbConnect(MySQL(), user="cryptowatch", password="!algotrade!", dbname="cryptowatch", host="85.214.37.84") #85.214.37.84
  rs = dbSendQuery(con, query)
  p = as.data.table(fetch(rs, n = -1))
  if ("time" %in% names(p) ) {p[,time :=as.POSIXct(time) ]}
  dbDisconnect(con)
  p=p[, agg_sd := sd(ask), by = list(exchange,pair)]
  p=p[agg_sd > 0]
  print(p %>% head(1))
  p
}


time_now = function() {
  as.POSIXct(Sys.time(), origin="1970-01-01",tz="UTC")
}



get_timediff_modified = function(file) {
  
  mod=file.info(file)$mtime
  if (is.na(mod)) mod=time_now()-24*3600*52
  diff = as.numeric(time_now() - mod,unit="secs")  
  diff
}


downloaded_data= function() {
  url <- "http://algotrade.glueckert.net/public/arbi_shiny.csv.gzip"
  arbi <- "arbi_shiny.csv.gzip"
  arbi_csv = "arbi_shiny.csv"
  if (get_timediff_modified(arbi_csv) >= 1200 ) {
      print(".......download new file")
      if (file.exists(arbi_csv)) file.remove(arbi_csv)
      download.file(url, arbi, mode="wb")
      gunzip(filename=arbi,destname=arbi_csv)
      return(TRUE)
  } else {
      print(".......data up to date")
      return(FALSE)
  }
}

load_data = function() {
  d <- data.table::fread("arbi_shiny.csv")
  d=prep_data(d)
  d
}


prep_data = function(d) {
  
  d[,time :=as.POSIXct(timstamp, origin="1970-01-01",tz="UTC") ]
  d=d[roi != -20.0]
  d
  
}


find_any = function(x,list) {
  sapply(list,function(y) {
    any(x %in% y)
  })
}

find_all = function(x,list) {
  sapply(list,function(y) {
    all(x %in% y)
  })
}