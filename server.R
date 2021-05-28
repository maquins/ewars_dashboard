#pkgs<-c('XLConnect','plyr','dplyr','car','stringr','zoo','foreign','ggplot2','splines',
       # 'mgcv','Hmisc','xtable','foreach','xlsx','lattice','latticeExtra',"gridExtra","grid","shiny","googlesheets")
#for (i in length(pkgs)){
 # if(pkgs[i] %in% rownames(installed.packages()) == FALSE) {
    #install.packages(pkgs[i])
  #}
#}


library(plyr)## This package should be loaded before dplyr
library(dplyr)
library(car)
library(stringr)
library(zoo)
library(foreign)
library(ggplot2)
library(splines)
library(mgcv)
library(Hmisc)
library(xtable)
library(foreach)
#library(xlsx)## Ensure java installed on system same as one for R, e.g 64 bit R with 64 bit Java
#library(XLConnect)
library(lattice)
library(latticeExtra)
library(gridExtra)
library(grid)
#library(googlesheets)
library(reshape2)
library(plotly)
library(tidyr)
library(lubridate)
library(pse)# for automatic calibration
library(reportROC)
library(caret)
library(e1071)
library(knitr)
library(pkgload)
library(rgeos)
library(raster)
#library(pak)

#options(repos=c(INLA="https://inla.r-inla-download.org/R/stable",
               # CRAN="https://cran.rstudio.com/"))
##load INLA package

#library(INLA)

library(doParallel)
library(profvis)

#library(future)
#library(promises)
#library(ipc)
library(readxl)
library(ROCit)
library(RSQLite)
library(DT)
## included after risk mapping integration

library(tmap)
library(tmaptools)
library(leaflet)
library(rgdal)
library(stringr)
library(xts)
library(dygraphs)
library(SpatialEpi)
library(spdep)
library(cleangeo)
#library(INLA)



if (!getDoParRegistered()){
  cl <<- makeCluster(5)
  registerDoParallel(cl)
}


server<-function(input,output,session) { 
  
  #ISO2<-"LKA"
  #Country_name<-"Sri Lanka"
  
  output$title_txt<-renderUI(tags$h3("Ewars Dashboard",style="font:cambria"))
  con <- dbConnect(SQLite(),"users.sqlite")
  #pb<-dbGetQuery(con, "SELECT user_name,password,role FROM users_db")
  pb<-data.frame(user_name="demo",password="demo_2019",role="admin")
  pb_dis<-dbGetQuery(con, "SELECT user_name,district_code FROM users_districts")
  dbDisconnect(con)
  
  login = F
  USER <- reactiveValues(login = login)
  user_info<-reactiveValues(nm_pwd="demo_2019",user="demo")
  
  observeEvent(input$lg_in11,{
    user_info$nm_pwd<-paste(str_trim(tolower(input$user_name)),str_trim(input$pwd))
    user_info$user<-str_trim(tolower(input$user_name))
    output$logged_in_user<-renderUI(input$user_name)
    
  })
  
 
  observe({
    
    role_d1<-pb %>% dplyr::filter(str_trim(tolower(user_name))==user_info$user)
    #role_d<-pb %>% dplyr::filter(str_trim(tolower(user_name))==user_info$user)
    if(nrow(role_d1)==1){
      role_d<-role_d1
    }else{
      role_d<-data.frame(user_name="xxx",role="xxx",stringsAsFactors =F)
    }
    #print(role_d)
    ##change this part for production 2020-11-26
    #-------------------------
    #-------------------------
    if (user_info$nm_pwd %in% paste(str_trim(tolower(pb$user_name)),str_trim(pb$password))){

      USER$login=T
    }  
    #print(user_info$nm_pwd)
    #print(pb)
    output$log_list<-renderUI(
      if(USER$login==T & str_trim(tolower(role_d$role))=="admin"){
        ui_yes
      }
      
      else if(USER$login==T & str_trim(tolower(role_d$role))=="district manager"){
        ui_yes_Restricted
      }
      
      else if(USER$login==F & !user_info$nm_pwd ==""){
        tagList(login_screen,br(),tags$h4("Please enter a valid user name and password"))
      }else if(USER$login==F & user_info$nm_pwd ==""){
        login_screen
      }
    )
  })
  
 
  ## read in the user database
  
  
  
  
  
  var_names <- eventReactive(input$dat,{
   #req(USER$login)
   # validate(need(input$dat, message = FALSE))
    inFile <- input$dat
    
    original_data_sheet_name<-input$original_data_sheet_name
    
    get_D_ata<-function(p){
      if(str_detect(p,".xlsx$")){
        data <- data.frame(read_xlsx(p,sheet=1),stringsAsFactors =F)
      }
      else if(str_detect(p,".xls$")){
        data <- data.frame(read_xls(p,sheet=1),stringsAsFactors =F)
      } 
      else if(str_detect(p,".csv$")){
        data <- read.csv(p,header =T,stringsAsFactors =F)
      } else{
        data <- data.frame(read_xlsx("Demo_Data.xlsx",sheet=1),stringsAsFactors =F)
      }
      data
    }
   data<-get_D_ata(inFile$datapath)
   print("from vars_names")
   print(head(data))
   print("from vars_names...")
    dist<-sort(unique(data$district))
    names(data)
    data<-data %>% arrange(district,year,week) 
    #choose  temp , rainfall
    
    
    list(vars=names(data),dat=data,dists=dist)
  })
  
  
 
  spline_vars <- eventReactive(input$alarm_indicators,{
    
    input$alarm_indicators  
  })
  
  observe({
    updateSelectInput(session,"spline_alarm_indicators",choices=spline_vars(),
                      selected =spline_vars() )
  })
  
  
  sel_districts <- eventReactive(input$run_per_district,{
    
    sort(input$run_per_district) 
  })
  
  observe({
    updateSelectInput(session,"district",choices=sel_districts(),
                      selected =sel_districts()[1] )
  })
  
  output$input_dataset<-renderUI({
    
    tagList(list(eval(parse(text=paste(paste('inputPanel(',base_input,',',
                                             paste(paste("numericInput('",
                                                         spline_vars(),"','",
                                                         spline_vars(),"',value=NA)",sep=''),
                                                   
                                                   collapse =','),
                                             
                                             ')'))
    )),
    eval(parse(text=output_dis1))
    ))
    
  
    
  })
 
  ## read in script depending on whether automated or not
  
  code_Ex<-reactiveValues(tx_t="")
  
  observeEvent(input$automate,{
    
    print(c("Auto status:",input$automate))
   # freezeReactiveValue(input, "automate")
    if(input$automate){
      code_Ex$tx_t<-parse(text=readLines("automated_reactive_Function.R"))

    }else if(!input$automate){
      code_Ex$tx_t<-parse(text=readLines("Manual_reactive_Function.R"))
      
    }
  } 
)
  

  out_x<-reactive({
    #input$dat
   validate(need(!is.null(input$dat), message = FALSE))
    print(c("Auto status 2:",input$automate))
    dd<<-code_Ex$tx_t
    
    #invalidateLater(1000)
  
    #req(!is.null(input$dat))
    
    eval(code_Ex$tx_t)

   out_x()
})
  
  
  observe({
    
    
    #al_vs<-grep("rain|temp|rainfall|precipitation|prec|humid|rh|humid|ovi|eggs",var_names()$vars,ignore.case=T)
    al_vs<-grep("rain|temp|rainfall|precipitation|prec",var_names()$vars,ignore.case=T)
    
    hos_vs<-grep("hosp",var_names()$vars,ignore.case=T)
    pop_vs<-grep("population|poblacion",var_names()$vars,ignore.case=T)
    
    freezeReactiveValue(input, "alarm_indicators")
    freezeReactiveValue(input, "number_of_cases")
    freezeReactiveValue(input, "population")
    freezeReactiveValue(input, "spline_vars")
    freezeReactiveValue(input, "run_per_district")
    freezeReactiveValue(input, "district_manage")
    
    
    updateSelectInput(session,"alarm_indicators",choices=var_names()$vars,
                      selected =var_names()$vars[al_vs] )
    updateSelectInput(session,"number_of_cases",choices=var_names()$vars,
                      #selected=var_names()$vars[3])
                      selected=var_names()$vars[hos_vs])
    updateSelectInput(session,"population",choices=var_names()$vars,
                      selected =var_names()$vars[pop_vs])
    #selected ="population")
    updateSelectInput(session,"spline_vars",choices=var_names()$vars)
    
    updateSelectInput(session,"run_per_district",
                      choices=var_names()$dist,
                      selected=var_names()$dist[1])
    
    updateSelectInput(session,"district_manage",
                      choices=var_names()$dist,
                      selected=var_names()$dist[1])
    #selected=15)
    updateSliderInput(session,"stop_runinYear",
                      min=min(var_names()$dat$year),
                      max=max(var_names()$dat$year),
                      value=max(var_names()$dat$year)-1)
    
    
  },priority =-1
  )
  ##update input elements for plotting
  
  observeEvent(c(input$automate),{
    if(input$automate){
      dat_p<-out_x()$auto_tab %>% filter(district==input$district)
      
      updateSelectInput(session,"outbreak_week_length",selected =dat_p$outbreak_week_length1[1] )
      updateSelectInput(session,"alarm_window",selected =dat_p$alarm_window1[1] )
      updateSelectInput(session,"alarm_threshold",selected =dat_p$alarm_threshold1[1] )
      updateSelectInput(session,"z_outbreak",selected =dat_p$z_outbreak1[1] )
      updateSelectInput(session,"outbreak_window",selected =dat_p$outbreak_window1[1] )
      updateSelectInput(session,"prediction_distance",selected =dat_p$prediction_distance1[1] )
      updateSelectInput(session,"outbreak_threshold",selected =dat_p$outbreak_threshold1[1])
      updateSelectInput(session,"season_length",selected =dat_p$season_length[1])
      updateSelectInput(session,"stop_runinYear",selected =as.numeric(substr(dat_p$stop_runin1[1],1,4)))
      
      
    }
  }
  )
  
  
  ## save sqlite
  ## save models and data
  
  observeEvent(input$save_mode,{
    tim_stamp<-str_replace_all(Sys.time(),"[-: ]","_")
    mod_r_nam<-paste("model_Run_",tim_stamp,sep='')
    assign(mod_r_nam,out_x())
    sav_str<-paste("save(",mod_r_nam,
                   ",file='",mod_r_nam,"')",sep='')
    eval(parse(text=sav_str))
    
    #add time to database of disticts and times
    bx<-str_replace_all(tim_stamp,"[-: ]","_")
    
    bcc<-str_split_fixed(bx,pattern='[_]',n=6)
    dist_s<-input$run_per_district
    dat_ins<-data.frame(district=dist_s,
                        Year=NA,
                        month=NA,
                        day=NA,
                        hour=NA,
                        minute=NA,
                        second=NA,
                        model_run_Name=mod_r_nam)
    dat_ins[,2:7]<-matrix(as.numeric(bcc[1,]),length(dist_s),6,byrow =T)
    
    con_mod <- dbConnect(SQLite(),"model_runs.sqlite")
    dbWriteTable(con_mod,'district_runs',dat_ins,append=T)
    dbDisconnect(con_mod)
    
    #create prospective
    # extract vars
    ## for all the districts
    for( dis in input$run_per_district){
      al_ev<-paste("arl_vars<-as.character(out_x()$param_values_",dis,"$value)[1]",sep='')
      eval(parse(text=al_ev))
      ## replace this part with new algorithm 2020-12-04
      covariates_d<-c("Cases","Population",as.character(str_split(arl_vars,',',simplify =T)))
      con_pros <- dbConnect(SQLite(),"prospective_data.sqlite")
      eval(parse(text=readLines("database_Functions.R")))
      foreach(a=1:length(covariates_d))%do% add_Variable(a)
      
    }
    
    
  }
  )
  
  ##update dashboard 2 based on saved model parameters
  
  dB2_pars<-eventReactive(input$Refresh_DB,{
   
    ## read model values and get latest run for each district
    con_mod <- dbConnect(SQLite(),"model_runs.sqlite")
    runs_D<-dbGetQuery(con_mod, "select * from district_runs")
    runs_D11<<-runs_D
    if(nrow(runs_D)>0){
    
    dist_chs<-as.numeric(unique(runs_D$district))
    #print(paste0("the districts::",paste0(sort(unique(runs_D$district))),collapse ="<>"))
    
    runs_D.a<-runs_D %>% mutate(ti1=paste(Year,month,day,sep ="-"),
                                   ti2=paste(hour,minute,second,sep =":"),
                                   date_time=as_datetime(paste(ti1,' ',ti2,sep ='')))
    runs_D1<-runs_D.a %>%arrange(district,desc(date_time)) %>% 
      dplyr::group_by(district) %>% dplyr::mutate(n=1:n())%>% filter(n==1)
    
    runs_D2<-data.frame(ungroup(runs_D1))
    
    print(runs_D2)
    ## create list of models and data values to use
    get_list_M<-function(p){
      d<-dist_chs[p]
      list_r<-runs_D2 %>% filter(district==d)
      load(list_r$model_run_Name)
      list_g<-c('data_runin_','data_eval_','gam_model_','param_values_')
      list_g1<-paste(list_g,d,sep='')
      ext_str<-paste("c(",paste("'",list_g1,"'",sep='',collapse =','),")",sep='')
      
      eval(parse(text=paste(list_r$model_run_Name,"[",ext_str,']',sep='')))
      
    }
     
    all_forDBS<-foreach(a=1:length(dist_chs),.combine =c) %do% get_list_M(a)
    names(all_forDBS)
    
    list(all_forDBS=all_forDBS,dis=sort(dist_chs),
         curr_Run=runs_D2)
    }else{
      NULL
    }
    })
  
  #update list selectable based on role and the districts they can see
  
  #create universal list
  
  
  observe({
    
    role_d1<-pb %>% dplyr::filter(str_trim(tolower(user_name))==user_info$user)
    #role_d<-pb %>% dplyr::filter(str_trim(tolower(user_name))==user_info$user)
    if(nrow(role_d1)==1){
      role_d<-role_d1
    }else{
      role_d<-data.frame(user_name="xxx",role="xxx",stringsAsFactors =F)
    }
    print(role_d)
    if(str_trim(tolower(role_d$role)) %in% c("admin","")){
      dist_SEL<-dB2_pars()$dis
    }else if (str_trim(tolower(role_d$role))=="district manager"){
      distr_access<-pb_dis %>% filter(str_trim(tolower(user_name))==user_info$user)
      print("these are the districts")
      print(distr_access)
      dist_SEL1<-sort(unique(c(distr_access$district_code)))
      dist_SEL<-dist_SEL1[which(dist_SEL1 %in% dB2_pars()$dis)]
      print(dist_SEL)
      class(dist_SEL)
    }else{
      dist_SEL<-c(999,9999)

    }
      
    updateSelectInput(session,"district_dB2",choices=dist_SEL,
                      #selected=var_names()$vars[3])
                      selected=dist_SEL[1])
    
    observeEvent(input$district_dB2,{
      pv_v<-as.numeric(input$district_dB2) %in% dB2_pars()$dis
      print(paste0("district id in dB2_pars ::",input$district_dB2,'::',pv_v))
      if(as.numeric(input$district_dB2) %in% dB2_pars()$dis){
        
      al_ev<-paste("arl_vars<-as.character(dB2_pars()$all_forDBS$param_values_",input$district_dB2,"$value)[1]",sep='')
      
      #eval(parse(text=al_evt))
      print(al_ev)
      eval(parse(text=al_ev))
      print(arl_vars)
      arl_vars1<-as.character(str_split(arl_vars,',',simplify =T))
      #update UI elements
      
      output_dis1.a<-"selectInput(inputId = 'district_dB2',
                           label = 'District',
                   choices = dist_SEL,
                   selected =input$district_dB2,
                   multiple =F)
                   "
      
      output_dis1<-"inputPanel(fluidRow(
             column(6,
                    selectInput(inputId = 'district_dB2',
                           label = 'District',
                   choices = dist_SEL,
                   selected =input$district_dB2,
                   multiple =F))))
                   "
      
    output$input_dataset<-renderUI({
      
      tagList(list(eval(parse(text=paste(paste('inputPanel(',base_input,',',
                                               paste(paste("numericInput('",
                                                           as.character(arl_vars1),"','",
                                                           as.character(arl_vars1),"',value=NA)",sep=''),
                                                     
                                                     collapse =','),
                                               
                                               ')'))
      )),
      eval(parse(text=output_dis1))
      ))
      
      
      
    }
    )
      print("passed here ..")
  eval(parse(text=readLines("refresh_Update_DB2.R") )) 
  
      }else{
     NULL
   }
      })
    
    })
  ## update entry UI
  
  ## read in users
  
  
  observeEvent(input$Enter_user,{
    
    entry_dat<-data.frame(First_name =input$First_name, 
                Last_name =input$Last_name,
                user_name =input$users_name,
                password =input$pass_word,
                email =input$email,
                role =input$role,
                stringsAsFactors =F)
    
    ##create user district entry 
    
    entry_district<-expand.grid(user_name=input$users_name,
                                district_code=input$district_manage,
                                stringsAsFactors =F)
    
    
    
    con_dist <- dbConnect(SQLite(),"users.sqlite")
    
    ## decide whether to update or insert new record
    ds_cu<-dbGetQuery(con_dist, 'SELECT distinct * FROM users_db')
    ds_cu1<-dbGetQuery(con_dist, 'SELECT distinct * FROM users_districts')
    
    if (!str_trim(entry_dat$user_name)=="" & !str_trim(entry_dat$password)==""){
      if (!entry_dat$user_name %in% ds_cu$user_name){
        dbWriteTable(con_dist,'users_db',entry_dat[1,],append=T)
      }else{
        bbp<-paste(paste(names(entry_dat),'=',paste("'",entry_dat[1,],"'",sep=''),collapse =','))
        
        dbSendQuery(con_dist,paste("update", "users_db","set",bbp,paste("where user_name='",entry_dat$user_name,"'",sep='')))
        # )))
      }
    
    #write users and districts
    if (input$role=="District Manager"){
      ## delete existing records before updating
      if (unique(entry_district$user_name) %in% ds_cu1$user_name){
        dbSendQuery(con_dist,paste("delete  from users_districts",paste("where user_name='",unique(entry_district$user_name),"'",sep='')))
      }
        
    dbWriteTable(con_dist,'users_districts',entry_district,append=T)
    
    }
    }else{
      print("do nothing")
    }
    
    user_di<-dbGetQuery(con_dist, 'SELECT distinct * FROM users_db')
    user_dist<-dbGetQuery(con_dist, 'SELECT distinct * FROM users_districts')
    dbDisconnect(con_dist)
    output$users_dat<-DT::renderDataTable(DT::datatable(user_di,
                                                options = list(autoWidth = TRUE,
                                                               searching = T)))
    output$users_districts<-DT::renderDataTable(DT::datatable(user_dist,
                                                options = list(autoWidth = TRUE,
                                                               searching = T)))
   
    
      con_dist1 <- dbConnect(SQLite(),"users.sqlite")
      
      dx<-dbGetQuery(con_dist1, 'SELECT distinct * FROM users_db')
      
      dbDisconnect(con_dist1)
      n.a<-paste(dx$user_name,' [',dx$First_name,',',dx$Last_name,']',sep='')
      
      
   
      updateSelectInput(session,"user_de",
                        choices =c(n.a,"John_doe [johh,doe]"),
                        selected ="John_doe [johh,doe]")
      
      
      observeEvent(input$Delete_user,{
        user_to_del<<-input$user_de
        to_de<-str_split(input$user_de,' ',simplify =T)[,1]
        
        
        con_dist1 <- dbConnect(SQLite(),"users.sqlite")
        del_qry<-paste("delete    from users_db where user_name in ",
                       paste('(',paste(paste("'",to_de,"'",sep=''),collapse=','),sep=''),')',sep='')
        
        del_qry1<-paste("delete    from users_districts where user_name in ",
                       paste('(',paste(paste("'",to_de,"'",sep=''),collapse=','),sep=''),')',sep='')
        
        
        dbExecute(con_dist1,del_qry)
        dbExecute(con_dist1,del_qry1)
        
        
        
      })
    
  }
  ) 
  
  observeEvent(input$ins_dat,{
    
    entry_dat<-data.frame(District=as.numeric(input$district_dB2),
                          year=as.numeric(input$year),
                          week=as.numeric(input$week),
                          Cases=as.numeric(input$Cases),
                          Population=as.numeric(input$Population),stringsAsFactors =F)
    entry_dat_cc<<-entry_dat
    ##variables from saved list
    if(input$district_dB2 %in% dB2_pars()$dis){
    al_ev<-paste("arl_vars<-as.character(dB2_pars()$all_forDBS$param_values_",input$district_dB2,"$value)[1]",sep='')
    arl_varsT<<-dB2_pars()
    #eval(parse(text=al_evt))
    print(al_ev)
    eval(parse(text=al_ev))
    print(arl_vars)
    arl_vars1<-as.character(str_split(arl_vars,',',simplify =T))
    ##update UI entry screen
    
    eval(parse(text=paste('entry_dat$',arl_vars1,'<-as.numeric(input$',arl_vars1,')',sep='')))
    
    entry_dat$entered_by<-input$user_name
    entry_dat$timestamp<-as.character(Sys.time())
    entry_dat_c<<-entry_dat
    
    
    con_pros <- dbConnect(SQLite(),"prospective_data.sqlite")
    
    runs_c<-dB2_pars()$curr_Run %>% filter(district==input$district_dB2)
    tim_stamp<-str_remove(runs_c$model_run_Name,"model_Run_")
    ## additions 2020-12-04
    eval(parse(text=readLines("database_Functions.R")))
    covariates_d<-c("Cases","Population",as.character(str_split(arl_vars,',',simplify =T)))
    foreach(a=1:length(covariates_d))%do% update_pros.Tables(a)
    
    
    dbDisconnect(con_pros)
    ## test 2020-12-06
    observeEvent(c(input$district_dB2,input$year),{
      #req(input$district_dB2,input$year)
      print(paste("district is: ",input$district_dB2))
      #print(names(dat_ou$df))
      p<-input$district_dB2
      covariates_d<-c("Cases","Population",as.character(str_split(arl_vars,',',simplify =T)))
      
      
      
      ## select data from sql database
      
      con_pros <- dbConnect(SQLite(),"prospective_data.sqlite")
      runs_c<-dB2_pars()$curr_Run %>% filter(district==input$district_dB2)
      tim_stamp<-str_remove(runs_c$model_run_Name,"model_Run_")
      
      dat_nam<-paste('pros_Dat_',input$district_dB2,'_',tim_stamp,sep='')
      print(dat_nam)
      eval(parse(text=readLines("database_Functions.R")))
      merge_pros_dat_str<-get_merge_STR(c(covariates_d))
      ## write the merge script for all tables in the list
      
      ## function to get objects to merge
      con_up <- dbConnect(SQLite(),"prospective_data.sqlite")
      
      for(hh in 1:length(covariates_d)){
        assign(covariates_d[hh],dbGetQuery(con_up, paste("select * from ",covariates_d[hh],collapse = " "))[,1:4])
      }
      
      dbDisconnect(con_up)
      
      ds_cu1<-eval(parse(text=merge_pros_dat_str)) %>% 
        dplyr::filter(District==input$district_dB2)
      print("reached here 2020-12-05...")
      ## decide whether to update or insert new record
      #ds_cu1<-dbGetQuery(con_pros, paste(paste('SELECT distinct * FROM ',dat_nam," order by year,week",sep=''),collapse =''))
      ds_cu1_<-ds_cu1 %>% 
        dplyr::filter(year==input$year)
      output$data_dis<-DT::renderDataTable(DT::datatable(ds_cu1,
                                                         options = list(autoWidth = TRUE,
                                                                        searching = FALSE)))
      ##update input values for easy editing
      print("reached here 2020-12-05...")
      val_in_up<-reactiveValues(dat=ds_cu1)
      #val_in_up<-reactiveValues(dat=ds_cu1)
      
      observeEvent(c(input$week,input$year),{
        
        al_ev<-paste("al_var1<-as.character( dB2_pars()$all_forDBS$param_values_",input$district_dB2,"$value)[1]",sep='')
        print(names(ds_cu1))
        eval(parse(text=al_ev))
        al_var<- c("Cases","Population",as.character(str_split(al_var1,pattern=",",simplify =T)))
        al_var11<-as.character(str_split(al_var1,pattern=",",simplify =T))
        fa_f<-ds_cu1 %>% dplyr::filter(week==input$week & year==input$year)
        print("this is where it changed")
        print(val_in_up$dat)
        
        check_t<-sum(sapply(al_var11,FUN =function(x) as.numeric(x %in% names(fa_f))))
        print(paste("total is::",check_t))
        if(check_t==length(al_var11)){
          #rev<-as.numeric(fa_f[,al_var]) 
          rev<-fa_f %>% 
            dplyr::select(all_of(al_var)) %>% 
            as.numeric()
          print(c(input$district_dB2,input$week))
          print(rev)
          up_st<-paste("updateNumericInput(session,'",al_var,"'",",value=",rev,')',sep='')
          print(up_st)
          eval(parse(text=up_st))
        }
      })
      
      #observe({
      #eval(parse(text=val_in_up()))
      #})
      
      # model data and evaluation
      d_str<-paste("dat_eval<-dB2_pars()$all_forDBS","[[",paste("'data_runin_",p,"'",sep=''),"]]",sep='')
      m_str<-paste("mod<-dB2_pars()$all_forDBS","[[",paste("'gam_model_",p,"'",sep=''),"]]",sep='')
      eval(parse(text=d_str))
      eval(parse(text=m_str))
      print("head :data_eval")
      ##print(head(dat_eval))
      ##get year 
      yrs_ch<-max(dat_eval$year)
      dat_eval<-dat_eval %>% filter(year==yrs_ch)
      dat_eval_merge<-dat_eval %>% 
        dplyr::mutate(outbreak_moving=round(outbreak_moving,6),
                      outbreak_moving_sd=outbreak_moving_sd,
                      outbreak_moving_limit=round(outbreak_moving_limit,6),
                      endemic_chanel=round(outbreak_moving_limit,6),
                      season=season) %>% 
        dplyr::select(district,week,outbreak_moving,outbreak_moving_sd,outbreak_moving_limit,
                      endemic_chanel,season)
      
      
      tem.d.a<-expand.grid(District=p,year=input$year,week=2:52)
      val_year_Merge<-ds_cu1 %>% 
        dplyr::filter(year==input$year)
      tem.d<-merge(tem.d.a,val_year_Merge,by=c("District","week"),sort=T,all.x=T)
      
      temp.d_check<<-tem.d
      ds_c1_check<<-ds_cu1
      ## merge with entered values for prediction
      cc_d<-paste("cc_v<-dB2_pars()$all_forDBS$param_values_",input$district_dB2,sep='')
      eval(parse(text=cc_d))
      a_n<-which(cc_v$parameter=="alarm threshold")
      print(cc_v$value[a_n])
      tem.d<-tem.d %>% dplyr::mutate(outbreak=(Cases/Population)*1000,
                                     alarm_threshold=as.numeric(as.character(cc_v$value[a_n])))%>% 
        #alarm_threshold=as.numeric(as.character(cc_v$value[a_n]))) %>% 
        dplyr::left_join(dat_eval_merge,by="week")
      ##map the alarm indicators
      
      al_ev<-paste("al_var1<-as.character( dB2_pars()$all_forDBS$param_values_",input$district_dB2,"$value)[1]",sep='')
      
      eval(parse(text=al_ev))
      al_var<- str_split(al_var1,pattern=",",simplify =T)
      n_alarm_indicators<-length(al_var)
      
      
      ##create alarm indicators
      
      #alarm_ind<-paste(paste(paste('mean_alarm',1:n_alarm_indicators,sep=''),'=',input$alarm_indicators),collapse =',')
      #create_alarm_inds<-paste('tem.d %>%mutate(',alarm_ind,')',sep='')
      
      ##create correctly
      #alarm_window<-3
      
      for_mean1<-paste('mean_alarm',1:n_alarm_indicators,sep='')
      
      al_w<-paste("alarm_window<-as.numeric( dB2_pars()$all_forDBS$param_values_",input$district_dB2,"$value)[5]",sep='')
      
      eval(parse(text=al_w))
      
      for_mean2<-paste("rollapply(",as.character(al_var),',FUN=mean,width=list(-',alarm_window-1,':0),
                                          align = "center",
                                          fill = NA,na.rm = T,
                                          partial=T)',sep='')
      
      for_mean3<-paste('tem.d %>% mutate(',paste(for_mean1,'=',for_mean2,collapse =','),')',sep='')
      
      cat(for_mean3)
      #parse(text=for_mean3)
      
      tem.d<-eval(parse(text=for_mean3))
      
      #tem.d<-eval(parse(text=create_alarm_inds))
      
      tem.d$Year_F<-yrs_ch-1
      ## compute outbreak probability
      out_prob<-predict(mod,tem.d,'response')
      tem.d$outbreak_probability<-round(as.numeric(out_prob),5)
      ## replace with missing unobserved weeks
      
      
      rep_pro<-which(!tem.d$week %in% ds_cu1$week)
      tem.d$outbreak_probability[rep_pro]<-NA
      ##compute the other indicators
      tem.d<-tem.d %>% mutate(outbreak_period=case_when(outbreak>endemic_chanel~1,
                                                        TRUE~0),
                              alarm_signal=case_when(outbreak_probability>alarm_threshold~1,
                                                     is.na(outbreak_probability)~as.double(NA),
                                                     TRUE~0))
      
      tem.d<-tem.d %>% mutate(lag0=dplyr::lag(alarm_signal,0),
                              lag1=dplyr::lag(alarm_signal,1),
                              lag2=dplyr::lag(alarm_signal,2),
                              lag3=dplyr::lag(alarm_signal,3),
                              lag4=dplyr::lag(alarm_signal,4)) %>% 
        mutate(response_cat=case_when(lag0==1 & lag1==1 & lag2 %in% c(0,NA) ~1,
                                      lag0==1 & lag1==1 & lag2==1 & lag3 %in% c(0,NA) ~1.5,
                                      lag0==1 & lag1==1 & lag2==1  & lag3==1 ~2,
                                      is.na(alarm_signal)~ as.double(NA),
                                      TRUE~0.5))
      tem.d$year<-as.numeric(as.character(tem.d$year.x))
      
      ## Reduce the variables to display
      vars.left<-c("District","year","week","Cases","Population","outbreak",
                   "endemic_chanel","alarm_threshold",
                   "outbreak_probability","alarm_signal",as.character(al_var))
      
      
      eval(parse(text=paste("sel.dat<-tem.d %>% dplyr::select(",paste(vars.left,collapse =','),")",sep='')))
      
      output$pred_dis<-DT::renderDataTable(DT::datatable(sel.dat,
                                                         options = list(autoWidth = F,
                                                                        searching = FALSE)) )
      
      ##send the plots
      
      
      
      dat_lab<-data.frame(response_cat=c("No response",
                                         "Initial response",
                                         "Early response",
                                         "Late/emergency response"),
                          x=-20,y=seq(0.65,2.5,0.5))
      plot1<-ggplot(aes(x=week,y=outbreak_moving_limit),data=tem.d)+
        geom_area(aes(fill="Endemic channel"))+
        geom_line(aes(y=outbreak,col="Confirmed cases"),lwd=0.3)+
        geom_point(aes(y=outbreak,col="Confirmed cases"),size=2.5,pch=15)+
        theme_bw()+
        scale_fill_manual(values =c("Endemic channel"=grey(0.7)))+
        scale_color_manual(values =c("Confirmed cases"='red1'))+
        scale_x_continuous(breaks=2:52,limits =c(2,52))+
        theme(panel.grid.major.x =element_blank(),
              panel.grid.minor.x =element_blank(),
              panel.grid.major.y =element_line(linetype=2),
              panel.grid.minor.y =element_blank(),
              axis.line.x.top =element_blank(),
              panel.border =element_blank(),
              axis.line.y =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
              axis.line.x =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
              legend.position ="top",
              axis.title.y =element_blank(),
              legend.text =element_text(size=14)
        )+
        guides(fill=guide_legend(title =NULL),
               color=guide_legend(title =NULL))+
        xlab("Epidemiological week")
      
      plot2<-ggplot()+
        
        geom_line(aes(x=week,y=outbreak_probability,col="Outbreak probability"),lwd=0.3,data=tem.d)+
        geom_point(aes(x=week,y=outbreak_probability,col="Outbreak probability"),size=2.5,pch=15,data=tem.d)+
        geom_line(aes(x=week,y=alarm_threshold,col="Alarm threshold"),lwd=0.7,data=tem.d,lty=2)+
        
        theme_bw()+
        scale_color_manual(values =c("Outbreak probability"='dark blue',
                                     "Alarm threshold"="forest green"))+
        scale_x_continuous(breaks=2:52,limits =c(2,52))+
        theme(panel.grid.major.x =element_blank(),
              panel.grid.minor.x =element_blank(),
              panel.grid.major.y =element_line(linetype=2),
              panel.grid.minor.y =element_blank(),
              axis.line.x.top =element_blank(),
              panel.border =element_blank(),
              axis.line.y =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
              axis.line.x =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
              legend.position ="top",
              axis.title.y =element_blank(),
              legend.text =element_text(size=14)
        )+
        guides(fill=guide_legend(title =NULL),
               color=guide_legend(title =NULL))+
        xlab("Epidemiological week")
      
      
      plot3<-ggplot(aes(x=week,y=outbreak_moving_limit),data=tem.d)+
        geom_area(aes(fill="Endemic channel"))+
        geom_line(aes(y=outbreak,col="Confirmed cases"),lwd=0.3)+
        geom_point(aes(y=outbreak,col="Confirmed cases"),size=2.5,pch=15)+
        geom_line(aes(x=week,y=outbreak_probability,col="Outbreak probability"),lwd=0.3,data=tem.d)+
        geom_point(aes(x=week,y=outbreak_probability,col="Outbreak probability"),size=2.5,pch=15,data=tem.d)+
        geom_line(aes(x=week,y=alarm_threshold,col="Alarm threshold"),lwd=0.7,data=tem.d,lty=2)+
        theme_bw()+
        scale_fill_manual(values =c("Endemic channel"=grey(0.7)))+
        scale_color_manual(values =c("Confirmed cases"='red1',
                                     "Outbreak probability"='blue',
                                     "Alarm threshold"="forest green"))+
        scale_x_continuous(breaks=2:52,limits =c(2,52))+
        theme(panel.grid.major.x =element_blank(),
              panel.grid.minor.x =element_blank(),
              panel.grid.major.y =element_line(linetype=2),
              panel.grid.minor.y =element_blank(),
              axis.line.x.top =element_blank(),
              panel.border =element_blank(),
              axis.line.y =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
              axis.line.x =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
              legend.position ="top",
              axis.title.y =element_blank(),
              legend.text =element_text(size=14)
              
        )+
        guides(fill=guide_legend(title =NULL),
               color=guide_legend(title =NULL))+
        xlab("Epidemiological week")
      
      plot4<-ggplot(aes(x=week,y=response_cat),data=tem.d)+geom_point(pch=21,size=2.5)+
        geom_hline(yintercept =0.5,col="yellowgreen",lwd=0.8)+
        geom_hline(yintercept =1,col="orange",lwd=0.8)+
        geom_hline(yintercept =1.5,col="brown",lwd=0.8)+
        geom_hline(yintercept =2,col="red",lwd=0.8)+
        geom_text(aes(x=x,y=y,label=response_cat,col=response_cat),data=dat_lab,
                  show.legend =F,hjust=0,nudge_x =0.2)+
        theme_bw()+
        scale_x_continuous(breaks=seq(2,52,2))+
        
        scale_color_manual(values=c("No response"='yellowgreen',
                                    "Initial response"='orange',
                                    "Early response"='brown',
                                    "Late/emergency response"='red'))+
        
        theme(panel.grid.minor.y =element_blank(),
              panel.grid.major.y =element_blank(),
              panel.grid.major.x =element_blank(),
              panel.grid.minor.x =element_blank(),
              panel.border =element_blank(),
              axis.line.x =element_line(linetype=1,
                                        colour="grey",
                                        size=0.4,
                                        lineend="butt"),
              axis.title.y =element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y =element_blank(),
              legend.text =element_text(size=14))+
        coord_fixed(6,ylim =c(0.3,3),xlim = c(-20,52))+
        xlab("Epidemiological week")
      
      output$db2_plot1<-renderPlot(plot1)
      output$db2_plot2<-renderPlot(plot2)
      output$db2_plot3<-renderPlot(plot3)
      output$db2_plot4<-renderPlot(plot4)
      
      
      cc_d<-paste("cc_v<-dB2_pars()$all_forDBS$param_values_",input$district_dB2,sep='')
      eval(parse(text=cc_d))
      
      output$a_vars<-renderPrint({
        cat(as.character(al_var),sep ='\n')
      })
      
      s1<-nrow(cc_v)
      s2<-s1-1
      sp_n<-which(cc_v$parameter=="Spline")
      
      output$alr_vars<-renderTable(cc_v[2:s2,])
      
      output$s_vars<-renderPrint({
        if(cc_v$value[sp_n]=="TRUE"){
          s_c<-as.character(str_split(cc_v$value[s1],',',simplify =T))
          cat(as.character(s_c),sep ='\n')
        }
        
      })
      
      
    })
    
    }else{NULL}
    }
  
  )
 
  #out_x<-callModule(auto_manual,"input_pan")
  
  #out_x<-eventReactive(input$automate,{
    
    #if(input$automate=="Yes"){
      #eval(parse(text=readLines("automated_reactive_Function.R")))
    #}else{
      #eval(parse(text=readLines("Manual_reactive_Function.R")))
      
    #}
    #out_x()
  #})
  
  #out_x<-eventReactive(input$automate,{
    
      #eval(parse(text=readLines("automated_reactive_Function.R")))
    
    #out_x()
  #})
  
  

  #eval(parse(text=readLines("Manual_reactive_Function.R")))
  
  
    #print(class(auto_s()$out_x))
  
  ## add for surveillance workbook
  
  ##compute prospective results by district
  
  output$logout <- renderUI({
    req(USER$login)
    tags$h5(a( "logout", 
               href="javascript:window.location.reload(true)"))
    #class = "dropdown", 
    #style = "background-color: #eee !important; border: 0;
    #font-weight: bold; margin:5px; padding: 10px;height:40px")
  })
  
  
  observeEvent(c(input$stop_runinYear,
                 input$stop_runinWeek,
                 input$number_of_cases,
                 input$population,
                 input$run_per_district,
                 input$alarm_indicators,
                 #input$outbreak_week_length,
                 #input$alarm_indicators,
                 #input$alarm_window,
                 #input$alarm_threshold,
                 #input$season_length,
                 #input$z_outbreak,
                 #input$outbreak_window,
                 #input$prediction_distance,
                 #input$outbreak_threshold,
                 input$spline,
                 input$District,
                 input$season_length
                 ),{
                  
                   
                   
                   
  observeEvent(c(input$district),{
  output$plot1 <- renderPlotly({
    
    #if(!is.null(out_x()$plotA1))  
    #eval( parse(text=out_x()$plot1.a))
    
    #tagList(eval(parse(text=paste(paste('renderPlot(out_x()$plotA',1:out_x()$dist_n,')',sep=''),collapse =',')))) 
    #csp<-paste(paste('renderPlotly(out_x()$runin_',input$district:input$district,')',sep=''),collapse =',')
    #csp1<-paste('tagList(',csp,')',sep='')
    
    csp1<-paste('out_x()$runin_',input$district:input$district,sep='')
    eval(parse(text=csp1))
    
    
  })
  
  #prosp_da<-reactiveValues()
  
  
  #pr_c<-readLines("get_pros_data.R")
  
  #eval(parse(text=pr_c))
  
  output$plot2 <- renderPlotly({ 
    
    #csp<-paste(paste('renderPlotly(out_x()$eval_',input$district:input$district,')',sep=''),collapse =',')
    #csp<-paste(paste('out_x()$eval_',input$district:input$district,sep=''),collapse =',')
    
    #csp1<-paste('tagList(',csp,')',sep='')
    csp1<-paste('out_x()$eval_',input$district:input$district,sep='')
    
    eval(parse(text=csp1))
    #out_x()$eval_',input$district:input$district
    
    
  })
  
  output$plot3 <- renderPlotly({
    #find the district to dsiplay
    
    
    
    #csp<-paste(paste('renderPlot(out_x()$plotC',1:out_x()$dist_n,')',sep=''),collapse =',')
    #csp<-paste(paste('renderPlotly(out_x()$runin_eval_',input$district:input$district,')',sep=''),collapse =',')
    #csp1<-paste('tagList(',csp,')',sep='')
    csp1<-paste('out_x()$runin_eval_',input$district:input$district,sep='')
    
    eval(parse(text=csp1))
    #renderPlot(plot(1:20))
    
  })
  
  ##compute the tables
  
  output$table1 <- renderTable({
    
    #if(!is.null(out_x()$ret2)){
    sub_dis<-out_x()$tab %>% filter(district==input$district) %>% mutate(id=1)
    aa<-melt(sub_dis,"id")
    ab<-aa[,-1]
    
    par_names<-c("District",
                 "Weeks",
                 "Outbreak Weeks",
                 "Outbreak Periods",
                 "Defined Outbreaks",
                 "Alarms",
                 "Correct Alarms",
                 "False Alarms",
                 "Missed Outbreaks",
                 "No alarm no outbreak",
                 "All Cases",
                 "Cases Below Threshold",
                 "Sensitivity",
                 "Positive Predictive Value (PPV)")
    bx<-c(paste(as.integer(ab[,2][1:12])),
          paste(round(ab[,2][13:14],2)))
    
    cc_v<-data.frame(Variable=par_names,
                     value=bx)
    cc_v
  #}
  })
  
  output$table2 <- renderTable({
    
    if(!is.null(out_x()$auto_tab)){
    sub_dis1<-out_x()$auto_tab %>% filter(district==input$district&
                                          Kappa>=0 & !is.na(Kappa))%>% mutate(run=paste('Rank',str_pad(1:n(),side="left",pad="0",width=4),sep=''))
    aa<-melt(sub_dis1,"run")
    ab<-aa %>% dplyr::group_by(variable) %>% spread(run,value)
    ac<-data.frame(ungroup(ab[,1:2]))
    par_names<-c("District",
                 "Outbreak week length",
                 "Alarm window",
                 "Alarm threshold",
                 "Z outbreak",
                 "Outbreak window",
                 "Prediction distance",
                 "Seasons",
                 "Stop Runin",
                 "Outbreak threshold",
                 "Kappa")
    #vvvvv<<-ac
    seas_vals<-c(1,2,3,4,6,12)
    bx<-c(paste(as.integer(ac[,2][1:3])),
          paste(round(as.numeric(ac[,2][4:5]),3)),
          paste(as.integer(ac[,2][6:7])),
         # paste(seas_vals[as.integer(ac[,2][9])]),
          paste(as.integer(ac[,2][9])),
          ac[,2][10],
          paste(round(as.numeric(ac[,2][c(8,11)]),3))
    )
    
    cc_v<-data.frame(parameter=par_names,
                     Best_combination=bx)
    cc_v 
    }
  })
  }) 
                   
  #output$px<-renderValueBox({
    #valueBox("Alarm threshold",
             #input$alarm_threshold)
 #}
#)
  

  output$workbooks <- renderUI({
    
    if(!is.null(out_x()$xls_file))
      
      eval( parse(text=out_x()$xls_file))
    
  }) 
  })
  
  
##the output files
  output$output_files <- renderUI({
    
 
      csv_auto_nam1<-paste("Auto calib table by dis",Sys.time(),'.csv',sep='')
      
      fn1<-out_x()$fn1
      fn2<-out_x()$fn2
      fn3<-out_x()$fn3
      
      bt1<-c()
      bt2<-c()
      bt3<-c()
      for (i in 1:length(fn1)){
        bt1[i]<-paste('tags$a(href="',fn1[i], '",target="_blank", tags$br(),tags$b("',fn1[i],'"))')
      }
      
      for (i in 1:length(fn2)){
        bt2[i]<-paste('tags$a(href="',fn2[i], '",target="_blank", tags$br(),tags$b("',fn2[i],'"))')
      }
      
      for (i in 1:length(fn3)){
        bt3[i]<-paste('tags$a(href="',fn3[i], '",target="_blank", tags$br(),tags$b("',fn3[i],'"))')
      }
      
      bq1<-paste('tags$strong("Plots by district"),', 
                 paste(bt1,collapse =',')
                 ,sep = "")
      bq2<-paste('tags$strong("Automatic calibration table by district"),', 
                paste(bt2,collapse =',')
                ,sep = "")
      bq3<-paste('tags$strong("Senitivity and specificity table by district"),', 
                paste(bt3,collapse =',')
                ,sep = "")
      
      
      c1<-paste('tags$div(',
                  bq1,')',sep='')
      
      c2<-paste('tags$div(',
                bq2,')',sep=''
      )
      
      c3<-paste('tags$div(',
                  bq3,')',sep=''
      )
      
      b1<-eval( parse(text=c1))
      b2<-eval( parse(text=c2))
      b3<-eval( parse(text=c3))
      tagList(tags$br(),
              b1,
              tags$br(),
              b2,tags$br(),
              b3)
    
    
    
    
})

##risk mapping part
  
  validate_spat_Input<-function(sph,surv){
    
    get_D_ata<-function(p){
      if(str_detect(p,".xlsx$")){
        data <- data.frame(read_xlsx(p,sheet=1),stringsAsFactors =F)
      }
      else if(str_detect(p,".xls$")){
        data <- data.frame(read_xls(p,sheet=1),stringsAsFactors =F)
      } 
      else if(str_detect(p,".csv$")){
        data <- read.csv(p,header =T,stringsAsFactors =F)
      } else{
        data <- data.frame(read_xlsx("Demo_Data.xlsx",sheet=1),stringsAsFactors =F)
      }
      data
    }
    
    original_data_sheet_name<-input$original_data_sheet_name
    inFile <- surv
    data<-get_D_ata(inFile$datapath)
    print("from vars_names")
    print(head(data))
    print("from vars_names...")
    dist<-sort(unique(data$district))
    names(data)
    data<-data %>% arrange(district,year,week) 
    #choose  temp , rainfall
    
    inFile_shp <- sph
    print(inFile_shp)
    shp_pos1<-grep(".shp",inFile_shp$name)
    layer_nm<-str_remove(inFile_shp$name[shp_pos1],'.shp')
    #rename file
    
    print("layer_name")
    print(layer_nm)
    ##get file with shp
    rename_fnm<-paste0(dirname(inFile_shp$datapath),'/',inFile_shp$name)
    file.rename(inFile_shp$datapath,rename_fnm)
    #ogrListLayers(inFile_shp$datapath[shp_pos])
    #list.files(dirname(inFile_shp$datapath))
    pp<-readOGR(rename_fnm[shp_pos1],layer_nm)
    print(names(pp))
    if("district" %in% names(pp)){
     # if(!input$dat_spat$datapath==""){
        
    "id match yeah yeah !!"
    }else{
      NULL
  }
  }

  
  #var_names_spat <- eventReactive(c(input$dat_spat,
  #input$shape_file,
  #input$selectInput),{
  #req(input$shape_file)
  #req(input$dat_spat)
  
  var_names_spat <- eventReactive(
                                   
    c(input$dat_spat,
      input$shape_file,
      input$spat_Input_Type),{           
                                     
    req(input$shape_file,input$dat_spat)
    #req()
    #req(input$spat_Input_Type)
    
                                      
                                      get_D_ata<-function(p){
                                        if(str_detect(p,".xlsx$")){
                                          data <- data.frame(read_xlsx(p,sheet=1),stringsAsFactors =F)
                                        }
                                        else if(str_detect(p,".xls$")){
                                          data <- data.frame(read_xls(p,sheet=1),stringsAsFactors =F)
                                        } 
                                        else if(str_detect(p,".csv$")){
                                          data <- read.csv(p,header =T,stringsAsFactors =F)
                                        } else{
                                          data <- data.frame(read_xlsx("Demo_Data.xlsx",sheet=1),stringsAsFactors =F)
                                        }
                                        data
                                      }
                                      
                                      
                                      inFile <- input$dat_spat
                                      data<-get_D_ata(inFile$datapath)
                                      print("from vars_names")
                                      print(head(data))
                                      print("from vars_names...")
                                      #dist<-sort(unique(data$district))
                                      #names(data)
                                      #data<-data %>% arrange(district,year,week) 
                                      #choose  temp , rainfall
                                      
                                      inFile_shp <- input$shape_file
                                      print(inFile_shp)
                                      shp_pos1<-grep(".shp",inFile_shp$name)
                                      layer_nm<-str_remove(inFile_shp$name[shp_pos1],'.shp')
                                      #rename file
                                      
                                      print("layer_name")
                                      print(layer_nm)
                                      ##get file with shp
                                      rename_fnm<-paste0(dirname(inFile_shp$datapath),'/',inFile_shp$name)
                                      file.rename(inFile_shp$datapath,rename_fnm)
                                      #ogrListLayers(inFile_shp$datapath[shp_pos])
                                      #list.files(dirname(inFile_shp$datapath))
                                      pp<-readOGR(rename_fnm[shp_pos1],layer_nm)
                                      
                                      list(vars=names(data),dat=data,dists=dist,SHP=pp)
                                      
                                      
                                     
                                    })
  
  
  
  observe({
    
    
    #al_vs<-grep("rain|temp|rainfall|precipitation|prec|humid|rh|humid|ovi|eggs",var_names()$vars,ignore.case=T)
    al_vs<-grep("rain|temp|rainfall|precipitation|prec",var_names_spat()$vars,ignore.case=T)
    
    hos_vs<-grep("hosp",var_names_spat()$vars,ignore.case=T)
    pop_vs<-grep("population|poblacion",var_names_spat()$vars,ignore.case=T)
    
    #freezeReactiveValue(input, "alarm_indicators")
    #freezeReactiveValue(input, "number_of_cases")
    #freezeReactiveValue(input, "population")
    
    
    updateSelectInput(session,"alarm_indicators_spat",choices=var_names_spat()$vars,
                      selected =var_names_spat()$vars[al_vs] )
    if(input$spat_Input_Type=="sub_district"){
    updateSelectInput(session,"number_of_cases_spat",choices=var_names_spat()$vars,
                      #selected=var_names()$vars[3])
                      selected=var_names_spat()$vars[hos_vs])
    updateSelectInput(session,"population_spat",choices=var_names_spat()$vars,
                      selected =var_names_spat()$vars[pop_vs])
    }
    #selected ="population")
    
    #selected=15)
    updateSliderInput(session,"stop_runinYear_spat",
                      min=min(var_names_spat()$dat$year),
                      max=max(var_names_spat()$dat$year),
                      value =max(var_names_spat()$dat$year))
    
    updateSliderInput(session,"stop_runinWeek_spat",
                      min=min(var_names_spat()$dat$week),
                      max=max(var_names_spat()$dat$week),
                      value =max(var_names_spat()$dat$week))
    #stop_runinWeek_spat
    
    updateSliderInput(session,"Year_plot_spat",
                      min=min(var_names_spat()$dat$year),
                      max=max(var_names_spat()$dat$year),
                      value =max(var_names_spat()$dat$year))
                      #value=max(var_names_spat()$dat$year)-1)
    
    updateSliderInput(session,"Week_plot_spat",
                      min=min(var_names_spat()$dat$week),
                      max=max(var_names_spat()$dat$week),
                      value =min(var_names_spat()$dat$week))
    #value=max(var_names_spat()$dat$year)-1)
    
    output$Uploaded_data<-DT::renderDataTable(DT::datatable(var_names_spat()$dat,
                                                            options = list(autoWidth = TRUE,
                                                                           searching = T)))
    
    ##plot the shapefile and render the data
    
    ##update the risk reactive values
    
  },priority =-1
  ) 
  
  var_Update_Vnames <- eventReactive(
    
    c(input$dat_spat),{           
      
      req(input$dat_spat)
      #req()
      #req(input$spat_Input_Type)
      
      
      get_D_ata<-function(p){
        if(str_detect(p,".xlsx$")){
          data <- data.frame(read_xlsx(p,sheet=1),stringsAsFactors =F)
        }
        else if(str_detect(p,".xls$")){
          data <- data.frame(read_xls(p,sheet=1),stringsAsFactors =F)
        } 
        else if(str_detect(p,".csv$")){
          data <- read.csv(p,header =T,stringsAsFactors =F)
        } else{
          data <- data.frame(read_xlsx("Demo_Data.xlsx",sheet=1),stringsAsFactors =F)
        }
        data
      }
      
      
      inFile <- input$dat_spat
      data<-get_D_ata(inFile$datapath)
      print("from vars_names")
      print(head(data))
      print("from vars_names...")
      #dist<-sort(unique(data$district))
      #names(data)
      #data<-data %>% arrange(district,year,week) 
      #choose  temp , rainfall
      list(vars=names(data),dat=data)
      
      
      
    })
  
  
  
  observe({
    
    
    #al_vs<-grep("rain|temp|rainfall|precipitation|prec|humid|rh|humid|ovi|eggs",var_names()$vars,ignore.case=T)
    al_vs<-grep("rain|temp|rainfall|precipitation|prec",var_Update_Vnames()$vars,ignore.case=T)
    
    hos_vs<-grep("hosp",var_Update_Vnames()$vars,ignore.case=T)
    pop_vs<-grep("population|poblacion",var_Update_Vnames()$vars,ignore.case=T)
    
    
    updateSelectInput(session,"alarm_indicators_spat",choices=var_Update_Vnames()$vars,
                      selected =var_Update_Vnames()$vars[al_vs] )
    if(input$spat_Input_Type=="sub_district"){
      updateSelectInput(session,"number_of_cases_spat",choices=var_Update_Vnames()$vars,
                        #selected=var_names()$vars[3])
                        selected=var_Update_Vnames()$vars[hos_vs])
      updateSelectInput(session,"population_spat",choices=var_Update_Vnames()$vars,
                        selected =var_Update_Vnames()$vars[pop_vs])
    }
    #selected ="population")
    
    #selected=15)
    updateSliderInput(session,"stop_runinYear_spat",
                      min=min(var_Update_Vnames()$dat$year),
                      max=max(var_Update_Vnames()$dat$year),
                      value =max(var_Update_Vnames()$dat$year))
    
    updateSliderInput(session,"stop_runinWeek_spat",
                      min=min(var_Update_Vnames()$dat$week),
                      max=max(var_Update_Vnames()$dat$week),
                      value =max(var_Update_Vnames()$dat$week))
    #stop_runinWeek_spat
    
    updateSliderInput(session,"Year_plot_spat",
                      min=min(var_Update_Vnames()$dat$year),
                      max=max(var_Update_Vnames()$dat$year),
                      value =max(var_Update_Vnames()$dat$year))
    #value=max(var_names_spat()$dat$year)-1)
    
    updateSliderInput(session,"Week_plot_spat",
                      min=min(var_Update_Vnames()$dat$week),
                      max=max(var_Update_Vnames()$dat$week),
                      value =min(var_Update_Vnames()$dat$week))
    #value=max(var_names_spat()$dat$year)-1)
    
    
    ##plot the shapefile and render the data
    
    ##update the risk reactive values
    
  }
  ) 
  
  observeEvent(input$spat_Input_Type,{
    #observe({
    
    if(input$spat_Input_Type=="sub_district"){
      tx_t<-parse(text=readLines("Spatial_areal.R"))
      out_dis<-output_graphs_spat_Agg
    }else{                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
      tx_t<-parse(text=readLines("Spatial_point.R"))
      out_dis<-output_graphs_spat_point
    }
    output$spat_Display<-renderUI({out_dis})
    eval(tx_t)
  }
  )
  
}