# contact: sewemaquins@gmail.com
add_Variable<-function(p){
  vat_DB<-covariates_d[p]
  create_prodD<-paste("create table ",vat_DB,"(
                      District num(8),
                      year num(8),
                      week num(8),",
                      paste(vat_DB,'num(8)'),
                      ',entered_by char(20),
                      timestamp char(20)
                      ',')')
  cat(create_prodD)
  if (!dbExistsTable(con_pros,vat_DB)){
    dbSendQuery(con_pros, create_prodD)
  }else{
    NULL
  }
}

update_pros.Tables<-function(t){
  
  UP_str<-paste("data.frame(District=input$district_dB2,
                      year=input$year,
                      week=input$week,",
                paste0(covariates_d[t],"=",'as.numeric(input$',covariates_d[t],')'),
                ",entered_by=input$user_name,
                      timestamp=as.character(Sys.time()))") 
  #cat(UP_str)
  data_UP<-eval(parse(text=UP_str))
  con_up <- dbConnect(SQLite(),"prospective_data.sqlite")
  ## decide whether to update or insert new record
  
  var.UP_val<-eval(parse(text=paste0('as.numeric(input$',covariates_d[t],')')))
  
  ## get data from database
  
  ds_cu<-dbGetQuery(con_pros, 
                    paste(paste('SELECT distinct * FROM ',
                                covariates_d[t],sep=''),
                          collapse ='')) %>% 
    dplyr::filter(District==input$district_dB2 & year==input$year & week==input$week)
  
  var.val_Prev<-eval(parse(text=paste0('as.numeric(ds_cu$',covariates_d[t],')')))[1]
  
  
  if(nrow(ds_cu)==0 & !is.na(var.UP_val)){
    dbWriteTable(con_up,covariates_d[t],data_UP,append=T)
  }else if (nrow(ds_cu)>0 & !is.na(var.UP_val) & (!var.UP_val==var.val_Prev)){
    del_qry<-paste(paste('delete  from  ',covariates_d[t],
                         paste(" where year=",input$year,sep=''),
                         paste(" and  week=",input$week,sep=''),sep='')
                   ,collapse ='')
    
    dbSendQuery(con_pros,del_qry)
    
    dbWriteTable(con_up,covariates_d[t],data_UP,append=T)
  }else{
    NULL
  }
  ax<-dbGetQuery(con_up, paste("select * from ",covariates_d[t],collapse = " "))
  dbDisconnect(con_up)
  #data_UP
  ## get the appropriate table and populate
  ax
  # NULL
}

get_merge_STR<-function(var_list){
  length_var<-length(var_list)
  
  get_str<-function(pp){
    if(pp==1){
      str.cmd<-"unique(Cases) %>%"
    }else if(pp>1 & pp<length_var){
      str.cmd<-paste0("dplyr::left_join(unique(",
                      var_list[pp],
                      "),by=c('District','year','week')) %>% ")
      
    }else{
      str.cmd<-paste0("dplyr::left_join(unique(",
                      var_list[pp],
                      "),by=c('District','year','week'))") 
    }
    str.cmd
  }
  #all_str.cmd<-foreach(a=1:length_var,.final =function(x) paste(x,collapse =" "))%do% get_str(a)
  all_str.cmd<-foreach(a=1:length_var,.combine =c)%do% get_str(a)
  
  all_str.cmd
}

