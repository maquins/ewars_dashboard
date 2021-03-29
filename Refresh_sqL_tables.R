# contact: sewemaquins@gmail.com
library(rsconnect)
library(doParallel)
library(foreach)
library(dplyr)
library(RSQLite)
library(dplyr)
#path<-""
#setwd(path)
db1<-paste(getwd(),"/model_runs.sqlite",sep='')
#db2<-paste(getwd(),"/users.sqlite",sep='')
db3<-paste(getwd(),"/prospective_data.sqlite",sep='')

## refresh model runs
dbConnect(SQLite(),db1) # district_runs
#dbConnect(SQLite(),db2) ## users_db users_districts
dbConnect(SQLite(),db3) ## district prospective data
